% format_time([H|T], Hr, Min) :-
% 	Hr is H,
% 	Min is T.
format_Dtime(time(Hours, Mins), Hr, Min) :-
	Hr is Hours,
	Min is Mins.

format_Atime(Time, Hr, Min) :-
	Time_in_min is round(Time * 60),
	divmod(Time_in_min, 60, Hr1, Min1),
	Hr is Hr1,
	Min is Min1.

print_out([]) :- nl.

print_out([ [ID1, DTime1, ATime1], ID2 | [] ]) :-
	airport(ID1, Name1, _, _),
	airport(ID2, Name2, _, _),
	format_Dtime(DTime1, DHr, DMin),
	format_Atime(ATime1, AHr, AMin),
	upcase_atom(ID1, ID1_upcase),
	upcase_atom(ID2, ID2_upcase),
	format("Depart  ~w ~w  ~|~`0t~d~2+:~|~`0t~d~2+ ~n", [ID1_upcase, Name1, DHr, DMin]),
	format("Arrive  ~w ~w  ~|~`0t~d~2+:~|~`0t~d~2+ ~n", [ID2_upcase, Name2, AHr, AMin]), !.

print_out([[ID1, DTime1, ATime1], [ID2, DTime2, ATime2] | List]) :-
	airport(ID1, Name1, _, _),
	airport(ID2, Name2, _, _),
	format_Dtime(DTime1, DHr, DMin),
	format_Atime(ATime1, AHr, AMin),
	upcase_atom(ID1, ID1_upcase),
	upcase_atom(ID2, ID2_upcase),
	format("Depart  ~w ~w  ~|~`0t~d~2+:~|~`0t~d~2+ ~n", [ID1_upcase, Name1, DHr, DMin]),
	format("Arrive  ~w ~w  ~|~`0t~d~2+:~|~`0t~d~2+ ~n", [ID2_upcase, Name2, AHr, AMin]),
	print_out([[ID2, DTime2, ATime2] | List]).

to_radians(degmin(Deg, Min), Result) :-
	Result is ((Deg + Min/60) * pi/180).

haversine(Lat, Lon, Lat1, Lon1, Dist_miles) :-
	EarthRadius is 3961,
	Dlon is Lon1 - Lon,
	Dlat is Lat1 - Lat,
	Tmpa is sin(Dlat / 2) ** 2
		+ cos(Lat) * cos(Lat1) * sin(Dlon / 2) ** 2,
	Unit_dist is 2 * atan2(sqrt(Tmpa), sqrt( 1 - Tmpa)),
	Dist_miles is Unit_dist * EarthRadius.

distance(From, To, Result) :-
	airport(From, _, Deg_Lat, Deg_Lon),
	airport(To, _, Deg_Lat1, Deg_Lon1),
	to_radians(Deg_Lat, Rad_Lat),
	to_radians(Deg_Lon, Rad_Lon),
	to_radians(Deg_Lat1, Rad_Lat1),
	to_radians(Deg_Lon1, Rad_Lon1),
	haversine(Rad_Lat, Rad_Lon, Rad_Lat1, Rad_Lon1, Result).

convert_to_hours(time(Hours, Mins), Result) :-
	Result is Hours + Mins / 60.

%if arrived in dest
fly_plan(Arrive, Arrive, [Arrive], _).

%else if go to last destination
fly_plan(Depart, Arrive,
	[[Depart, Time, Arrival_time] | Schedules], VisitedAirports) :-
	flight(Depart, Arrive, Time),
	convert_to_hours(Time, Departure_time),
	distance(Depart, Arrive, Miles_distance),
	Flight_time is (Miles_distance / 500),
	Arrival_time is Departure_time + Flight_time,
	Arrival_time < 24,
	fly_plan(Arrive, Arrive, Schedules, [Arrive|VisitedAirports]).

%else need to hub
fly_plan(Depart, Arrive,
	[[Depart, Time, Arrival_time] | Schedules], VisitedAirports) :-
	flight(Depart, NextHub, Time),
	convert_to_hours(Time, Departure_time),
	distance(Depart, Arrive, Miles_distance),
	% how long it takes to get to destination
	Flight_time is (Miles_distance / 500),
	Arrival_time is Departure_time + Flight_time,
	% before midnight
	Arrival_time < 24,
	% flight(NextHub, _, Hub_departure_time),
	% convert_to_hours(Hub_departure_time, NextFlightTime),
	% Total_flight_time is (NextFlightTime - (Arrival_time + 0.5)),
	% Total_flight_time >= 0,
	fly_plan(NextHub, Arrive, Schedules, [NextHub|VisitedAirports]).

fly(From, From) :-
	format("~w and ~w are the same. ", [From, From]),
	!,
	fail.


fly(From, To) :-
	airport(From, _, _, _),
	airport(To, _, _, _),
	fly_plan(From, To, Result, [From]),
	print_out(Result).

fly(From, To) :-
	airport(From, _, _, _),
	airport(To, _, _, _),
	format("Flight ~w to ~w not possible").

fly(_,_) :-
	format("airport doesnt exist").
