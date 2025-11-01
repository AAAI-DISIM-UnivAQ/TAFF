:- module(neabt_io_lists, [
    load_observations/2,            % +File, -ObsList
    neabt_from_observation/4,       % +Obs, +RootNode, -SuccessNodes, -EmoState
    run_efficiency/2,               % +Files, -Report
    run_effectiveness/4,            % +File, +DropSpec, -Accuracy, -Details
    select_subset/3                 % +Obs, +DropSpec, -ObsSubset
]).

/* ------------------------------------------------------------------
   DATA SHAPES (lists only)
   ------------------------------------------------------------------

   One observation (Obs) is a list with three lists:
     Obs = [Env, User, Domain].

   Each of Env/User/Domain is a list of 2-element lists [Key, Value].

   Keys are atoms as listed below; Values are atoms/ints from the file.

   Keys and order:

   ENV (6):
     [traffic_status, driving_hours, speed, stops,
      avg_stop_duration, location]

   USER (4):
     [crashes_last_year, address, driving_safety, driving_experience]

   DOMAIN (3):
     [road_conditions, weather_conditions, road_dangerousness]

   Knowledge Graph (KG) we pass to your NEABT is:
     KG = [[user, User], [domain, Domain]]

   DropSpec (what to hide in “partial knowledge” runs) is one of:
     ["env",    [Key,...]]
     ["user",   [Key,...]]
     ["domain", [Key,...]]
     ["all",    [Key,...]]

   “Report” (for efficiency) is a list of rows, each row is:
     [file, File, n_obs, N, load_ms, LoadMS, exec_ms, ExecMS]

   “Details” (for effectiveness) is a list:
     [total, N, matches, M]
------------------------------------------------------------------- */

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(statistics)).

%% -------------------------
%% Build one observation (lists only)
%% -------------------------

make_obs([ TStatus, DHrs, Speed, Stops, AvgStop, Loc,
           Crashes, Address, DSafety, DExp,
           RoadCond, Weather, Danger
         ],
         [Env, User, Domain]) :-
    Env   = [[traffic_status,     TStatus],
             [driving_hours,      DHrs],
             [speed,              Speed],
             [stops,              Stops],
             [avg_stop_duration,  AvgStop],
             [location,           Loc]],
    User  = [[crashes_last_year,  Crashes],
             [address,            Address],
             [driving_safety,     DSafety],
             [driving_experience, DExp]],
    Domain= [[road_conditions,     RoadCond],
             [weather_conditions,  Weather],
             [road_dangerousness,  Danger]].

%% -------------------------
%% File loading (13 facts per observation)
%% -------------------------

% load_observations(+File, -ObsList)
load_observations(File, ObsList) :-
    read_file_terms_(File, Terms),
    group13_(Terms, Groups),
    maplist(terms_to_obs_, Groups, ObsList).

read_file_terms_(File, Terms) :-
    setup_call_cleanup(
        open(File, read, In),
        read_lines_as_terms_(In, Terms),
        close(In)
    ).

read_lines_as_terms_(In, Terms) :-
    read_string(In, _, Raw),
    split_string(Raw, "\n", "\r", Lines0),
    exclude(=( ""), Lines0, Lines),
    maplist(parse_term_string_, Lines, Terms).

parse_term_string_(S, Term) :-
    string_concat(S, ".", Sdot),
    catch(read_term_from_atom(Sdot, Term, []), _, fail), !.
parse_term_string_(S, Term) :-
    read_term_from_atom(S, Term, []).

group13_([], []).
group13_(L, [G|Rest]) :-
    length(G, 13),
    append(G, Tail, L),
    group13_(Tail, Rest).

terms_to_obs_([
    traffic_status(TS),
    driving_hours(H),
    speed(SP),
    stops(ST),
    avg_stop_duration(ASD),
    location(Loc),
    crashes_last_year(Cr),
    address(Addr),
    driving_safety(Safe),
    driving_experience(Exp),
    road_conditions(RC),
    weather_conditions(WC),
    road_dangerousness(Dang)
  ], Obs) :-
    make_obs([TS,H,SP,ST,ASD,Loc,Cr,Addr,Safe,Exp,RC,WC,Dang], Obs).

%% -------------------------
%% Adapter to core NEABT (lists in, lists out)
%% -------------------------

% neabt_from_observation(+Obs, +RootNode, -SuccessNodes, -EmoState)
% Obs = [Env, User, Domain], KG = [[user,User],[domain,Domain]]
% NOTE: the NEABT entry predicate accepts Env (list) and KG (list).
neabt_from_observation([Env, User, Domain], RootNode, SuccessNodes, EmoState) :-
    KG = [[user, User], [domain, Domain]],
    user_sensor_default(UserSensor),
    % Call into your existing NEABT (adapt the arity/name if needed):
    % Expected signature: neabt(UserSensor, KG, Env, RootNode, SuccessNodes)
    neabt(UserSensor, KG, Env, RootNode, SuccessNodes),
    ( last_emotion_(EmoState) -> true ; EmoState = unknown ).

user_sensor_default([[source, synthetic]]).

:- dynamic last_emotion_/1.

%% -------------------------
%% Effectiveness experiments (lists only)
%% -------------------------

% run_effectiveness(+File, +DropSpec, -Accuracy, -Details)
% DropSpec = ["env"|Keys] ; ["user"|Keys] ; ["domain"|Keys] ; ["all"|Keys]
run_effectiveness(File, DropSpec, Accuracy, [total, N, matches, M]) :-
    load_observations(File, ObsList),
    Root = root,
    maplist(neabt_decision_(Root), ObsList, FullOuts),
    maplist(select_subset(DropSpec), ObsList, SubList),
    maplist(neabt_decision_(Root), SubList, SubOuts),
    same_length(FullOuts, SubOuts),
    pairwise_equal_(FullOuts, SubOuts, M),
    length(FullOuts, N),
    ( N =:= 0 -> Accuracy = 0.0 ; Accuracy is M / N ).

% neabt_decision_(+Root, +Obs, -Decision)
% Decision is [emotion,E, success,S] where S is the first success node or none.
neabt_decision_(Root, Obs, [emotion, E, success, S]) :-
    neabt_from_observation(Obs, Root, SuccessNodes, E),
    ( SuccessNodes = [H|_] -> S = H ; S = none ),
	!.

pairwise_equal_(A, B, Count) :-
    maplist(eq_, A, B, EqualFlags),
    include(=(true), EqualFlags, Trues),
    length(Trues, Count).

eq_(X, Y, true)  :- X == Y, !.
eq_(_, _, false).

% select_subset(+DropSpec, +Obs, -ObsSubset)
% DropSpec examples:
%   ["env",[traffic_status,speed]]
%   ["user",[driving_experience]]
%   ["domain",[weather_conditions]]
%   ["all",[traffic_status,weather_conditions]]
select_subset([Which, Keys], [Env, User, Domain], [Env2, User2, Domain2]) :-
    ( Which == env    -> drop_keys_list_(Env,    Keys, Env2),  User2=User,   Domain2=Domain
    ; Which == user   -> drop_keys_list_(User,   Keys, User2), Env2=Env,     Domain2=Domain
    ; Which == domain -> drop_keys_list_(Domain, Keys, Domain2), Env2=Env,   User2=User
    ; Which == all    -> drop_keys_list_(Env, Keys, Env2),
                           drop_keys_list_(User, Keys, User2),
                           drop_keys_list_(Domain, Keys, Domain2)
    ; % default passthrough
      Env2=Env, User2=User, Domain2=Domain
    ).

% drop_keys_list_(+Pairs, +Keys, -PairsOut)
% Pairs is [[Key,Val],...]; remove any whose Key is in Keys.
drop_keys_list_(Pairs0, Keys, Pairs) :-
    exclude(has_key_(Keys), Pairs0, Pairs).

has_key_(Keys, [K,_]) :- memberchk(K, Keys).

%% -------------------------
%% Efficiency experiments (lists only)
%% -------------------------

% run_efficiency(+Files, -Report)
% Report = [[file,File,n_obs,N,load_ms,LoadMS,exec_ms,ExecMS], ...]
run_efficiency(Files, Report) :-
    maplist(time_file_, Files, Report).

time_file_(File, [file,File,n_obs,N,load_ms,LoadMS,exec_ms,ExecMS]) :-
    statistics(walltime, _),
    load_observations(File, ObsList),
    statistics(walltime, [_, LEnd]),
    length(ObsList, N),
    LoadMS = LEnd,
    Root = root,
    statistics(walltime, _),
    forall(member(Obs, ObsList),
           ( neabt_from_observation(Obs, Root, _S, _E) -> true ; true )),
    statistics(walltime, [_, EEnd]),
    ExecMS = EEnd.