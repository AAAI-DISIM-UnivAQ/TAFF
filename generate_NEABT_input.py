import sys
import random

#environment
traffic_statuses = ['green', 'orange', 'red', 'black']
driving_hours = range(0,9)
speed = range(30,150)
stops = range(0,6)
stop_duration = range(1,13) #to be mutiplied by 5 minutes
locations = range(1,51) #range of 50 possible location IDs

#KG user profile
crashes_last_year = range(0,4)
addresses = locations
driving_safety = range(1,6) #level of driving safety; the higher, the better
driving_experience = range(1,6) #level of driving experience; the higher, the better

#KG domain knowledge
road_conditions = ['bad', 'fair', 'good']
weather_conditions = ['sunny', 'cloudy', 'rainy', 'snowy']
road_dangerousness = range(1,6) #level of dangerousness of the road; the higher, the more dangerous

def generate_prolog_predicates():
	predicates = []

	#environment predicates
	predicates.append("traffic_status(" + traffic_statuses[random.randint(0,len(traffic_statuses)-1)] + ")")
	predicates.append("driving_hours(" + str(random.randint(min(driving_hours),max(driving_hours))) + ")")
	predicates.append("speed(" + str(random.randint(min(speed),max(speed))) + ")")
	gen_stops = random.randint(min(stops),max(stops))
	predicates.append("stops(" + str(gen_stops) + ")") #number of stops done during the current driving session
	gen_avg_stop_duration = 0
	if gen_stops > 0:
		for i in range(gen_stops):
			gen_stop_duration = 5*random.randint(min(stop_duration),max(stop_duration))
			gen_avg_stop_duration += gen_stop_duration
		gen_avg_stop_duration /= gen_stops
		gen_avg_stop_duration = int(round(gen_avg_stop_duration,0))
	predicates.append("avg_stop_duration(" + str(gen_avg_stop_duration) + ")") #average duration (in minutes) of the stops done during the current driving session
	predicates.append("location(" + str(random.randint(min(locations),max(locations))) + ")") #ID (integer \in [1,50]) of the current location

	#KG user profile predicates
	predicates.append("crashes_last_year(" + str(random.randint(min(crashes_last_year),max(crashes_last_year))) + ")")
	predicates.append("address(" + str(random.randint(min(locations),max(locations))) + ")") #ID (integer \in [1,50]) of driver's address
	predicates.append("driving_safety(" + str(random.randint(min(driving_safety),max(driving_safety))) + ")")
	predicates.append("driving_experience(" + str(random.randint(min(driving_experience),max(driving_experience))) + ")")

	#KG domain knowledge predicates
	predicates.append("road_conditions(" + road_conditions[random.randint(0,len(road_conditions)-1)] + ")")
	predicates.append("weather_conditions(" + weather_conditions[random.randint(0,len(weather_conditions)-1)] + ")")
	predicates.append("road_dangerousness(" + str(random.randint(min(road_dangerousness),max(road_dangerousness))) + ")")

	return predicates

if __name__ == '__main__':
	n_simulations = int(sys.argv[1])
	output_file = "NEABT_input_" + str(n_simulations) + ".pl"
	with open(output_file, "w") as output:
		for i in range(n_simulations):
			predicates = generate_prolog_predicates()
			for p in predicates:
				output.write(p + '\n')
	output.close()