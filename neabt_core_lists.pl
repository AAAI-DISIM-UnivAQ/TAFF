:- module(neabt_core_lists, [
    neabt/5,               % +UserSensor, +KG, +Env, +RootNode, -SuccessNodes
    neural_node/4,         % +UserSensor, +KG, +Env, -Emotion
    emo_selector/6,        % +EmotionIn,+KG,+Env,+NodeId,-EmotionOut,-Decision
    reset_last_emotion/0
]).

/* ===============================================================
   NEABT CORE — list-only version
   Data shapes (as used by neabt_io_lists.pl):

   Env  = [[traffic_status,Atom],
           [driving_hours,Int],
           [speed,Int],
           [stops,Int],
           [avg_stop_duration,Int],
           [location,Atom]]

   KG   = [[user,   [[crashes_last_year,Int],[address,Atom],
                     [driving_safety,Int],[driving_experience,Int]]],
           [domain, [[road_conditions,Atom],[weather_conditions,Atom],
                     [road_dangerousness,Atom]]]]

   UserSensor = arbitrary list (we don't rely on structure here)

   Behavior tree here is a simple PRIORITY selector:
     1) stop
     2) advisory
     3) explicative
     4) reassurance

   SuccessNodes is the list of nodes that "succeed" in order tried.
   We also publish last_emotion_/1 as a side effect.
   =============================================================== */

:- dynamic last_emotion_/1.

reset_last_emotion :- retractall(last_emotion_(_)).
reset_last_emotion() :- reset_last_emotion.

%% -------------------------
%% Entry point
%% -------------------------
neabt(UserSensor, KG, Env, _RootNode, SuccessNodes) :-
    % 1) infer emotion (placeholder neural node)
    neural_node(UserSensor, KG, Env, Emotion0),
    retractall(last_emotion_(_)),
    asserta(last_emotion_(Emotion0)),

    % 2) run a simple priority behavior tree
    priority_selector(KG, Env, Emotion0, SuccessNodes, _FinalEmotion).

%% -------------------------
%% “Neural” node (rule-based stand-in)
%% -------------------------
% Derives a coarse emotion label from environment + driver profile.
neural_node(_UserSensor, KG, Env, Emotion) :-
    env_get(speed, Env, Speed, 0),
    env_get(traffic_status, Env, TStat, green),
    kg_dom_get(weather_conditions, KG, Weather, clear),
    kg_dom_get(road_dangerousness, KG, Danger, low),
    kg_user_get(driving_safety, KG, Safety, 3),        % 1..5 (low..high)
    kg_user_get(driving_experience, KG, Exp, 3),

    risk_score(TStat, Weather, Danger, Speed, BaseRisk),
    skill_factor(Safety, Exp, SkillF),
    AdjRisk is max(0, BaseRisk - SkillF),

    ( AdjRisk >= 7 -> Emotion = stressed
    ; AdjRisk >= 4 -> Emotion = alert
    ; AdjRisk >= 2 -> Emotion = wary
    ;                 Emotion = calm ).

risk_score(TStat, Weather, Danger, Speed, Score) :-
    traffic_risk(TStat, RT),
    weather_risk(Weather, RW),
    danger_risk(Danger, RD),
    speed_risk(Speed, RS),
    Score is RT + RW + RD + RS.

traffic_risk(black, 4).
traffic_risk(red,   3).
traffic_risk(orange,2).
traffic_risk(green, 0).
traffic_risk(_,     1).

weather_risk(storm, 4).
weather_risk(snow,  3).
weather_risk(rain,  2).
weather_risk(fog,   2).
weather_risk(clear, 0).
weather_risk(_,     1).

danger_risk(high,   4).
danger_risk(medium, 2).
danger_risk(low,    0).
danger_risk(_,      1).

speed_risk(Speed, R) :-
    ( Speed >= 130 -> R = 4
    ; Speed >= 110 -> R = 3
    ; Speed >= 90  -> R = 2
    ; Speed >= 60  -> R = 1
    ;                 R = 0 ).

skill_factor(Safety, Exp, F) :-
    clamp_1_5(Safety, S1),
    clamp_1_5(Exp,    E1),
    F is (S1 + E1) // 2.       % simple attenuation

clamp_1_5(X, Y) :- ( X < 1 -> Y=1 ; X > 5 -> Y=5 ; Y=X ).

%% -------------------------
%% Behavior tree
%% -------------------------
priority_selector(KG, Env, EmoIn, Successes, EmoOut) :-
    try_node(stop,         KG, Env, EmoIn,  S1,  Emo1),
    try_node(advisory,     KG, Env, Emo1,   S2,  Emo2),
    try_node(explicative,  KG, Env, Emo2,   S3,  Emo3),
    try_node(reassurance,  KG, Env, Emo3,   S4,  EmoOut),
    append_all([S1,S2,S3,S4], Successes).

try_node(NodeId, KG, Env, EmoIn, Success, EmoOut) :-
    ( node_condition(NodeId, KG, Env) ->
        emo_selector(EmoIn, KG, Env, NodeId, EmoOut, _Decision),
        Success = [NodeId] ;   EmoOut = EmoIn,
        Success = []
    ).

% Node preconditions (purely illustrative; tweak as needed)
node_condition(stop, KG, Env) :-
    kg_dom_get(road_dangerousness, KG, Danger, low),
    env_get(speed, Env, Speed, 0),
    kg_dom_get(weather_conditions, KG, Weather, clear),
    ( Danger = high ;
      (Weather = storm ; Weather = snow),
      Speed >= 90
    ).

node_condition(advisory, _KG, Env) :-
    env_get(traffic_status, Env, T, green),
    memberchk(T, [red, black]);
    env_get(stops, Env, Stops, 0),
    Stops >= 3.

node_condition(explicative, KG, Env) :-
    kg_user_get(crashes_last_year, KG, C, 0), C > 0 ;
    ( kg_user_get(driving_experience, KG, Exp, 3), Exp =< 2 ),
    env_get(avg_stop_duration, Env, ASD, 0), ASD >= 60.

node_condition(reassurance, KG, Env) :-
    kg_user_get(driving_safety, KG, S, 3), S >= 4,
    kg_dom_get(road_dangerousness, KG, D, low), D \= high,
    env_get(traffic_status, Env, T, green), T \= black.

% Emotion adjustment per node (toy logic to keep it predictable)
% emo_selector(+EmoIn,+KG,+Env,+NodeId,-EmoOut,-Decision)
emo_selector(EmoIn, _KG, _Env, stop, EmoOut, stop) :-
    escalate(EmoIn, EmoOut).
emo_selector(EmoIn, _KG, _Env, advisory, EmoOut, advisory) :-
    nudge_towards(EmoIn, alert, EmoOut).
emo_selector(EmoIn, _KG, _Env, explicative, EmoOut, explicative) :-
    nudge_towards(EmoIn, wary, EmoOut).
emo_selector(EmoIn, _KG, _Env, reassurance, EmoOut, reassurance) :-
    deescalate(EmoIn, EmoOut).

escalate(calm,   alert).
escalate(wary,   alert).
escalate(alert,  stressed).
escalate(stressed, stressed).
escalate(X, X) :- var(X).

deescalate(stressed, alert).
deescalate(alert,    wary).
deescalate(wary,     calm).
deescalate(calm,     calm).
deescalate(X, X) :- var(X).

nudge_towards(From, Target, Out) :-
    ( From == Target -> Out = From
    ; path_to(Target, From, Path),
      ( Path = [_,Next|_] -> Out = Next ; Out = Target )
    ).

% a simple lattice path among {calm, wary, alert, stressed}
path_to(calm,    wary,     [wary, calm]).
path_to(calm,    alert,    [alert, wary, calm]).
path_to(calm,    stressed, [stressed, alert, wary, calm]).
path_to(wary,    calm,     [calm, wary]).
path_to(wary,    alert,    [alert, wary]).
path_to(wary,    stressed, [stressed, alert, wary]).
path_to(alert,   calm,     [calm, wary, alert]).
path_to(alert,   wary,     [wary, alert]).
path_to(alert,   stressed, [stressed, alert]).
path_to(stressed,calm,     [calm, wary, alert, stressed]).
path_to(stressed,wary,     [wary, alert, stressed]).
path_to(stressed,alert,    [alert, stressed]).
path_to(X, X, [X]).

append_all(Lists, Out) :- foldl(append, Lists, [], Out).

%% -------------------------
%% Key/value helpers (lists only)
%% -------------------------
% Env is [[Key,Val]...]
env_get(Key, Env, Val, Default) :-
    ( member([Key, V], Env) -> Val = V ; Val = Default ).

% KG is [[user,UserPairs],[domain,DomainPairs]]
kg_section(KG, Section, Pairs, Default) :-
    ( member([Section, Pairs], KG) -> true ; Pairs = Default ).

kg_user_get(Key, KG, Val, Default) :-
    kg_section(KG, user, User, []),
    ( member([Key, V], User) -> Val = V ; Val = Default ).

kg_dom_get(Key, KG, Val, Default) :-
    kg_section(KG, domain, Dom, []),
    ( member([Key, V], Dom) -> Val = V ; Val = Default ).
