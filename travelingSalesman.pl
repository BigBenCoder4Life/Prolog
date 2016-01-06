%
% Solve the travelling salesman problem for a list of cities.
% mod15PA
% Spring 2015
% Ben Kirtley
% This work complies with the James Madison Univeristy Honor code
%

% The cities
cities([calgary, charlottetown, halifax, montreal, ottawa, toronto, victoria, winnipeg]).

% Distances between the cities
distance(calgary, charlottetown, 4675).
distance(calgary, halifax, 4770).
distance(calgary, montreal, 3534).
distance(calgary, ottawa, 3338).
distance(calgary, toronto, 3412).
distance(calgary, victoria, 1061).
distance(calgary, winnipeg, 1327).
distance(charlottetown, calgary, 4675).
distance(charlottetown, halifax, 326).
distance(charlottetown, montreal, 1146).
distance(charlottetown, ottawa, 1338).
distance(charlottetown, toronto, 1697).
distance(charlottetown, victoria, 6133).
distance(charlottetown, winnipeg, 3403).
distance(halifax, calgary, 4771).
distance(halifax, charlottetown, 325).
distance(halifax, montreal, 1242).
distance(halifax, ottawa, 1435).
distance(halifax, toronto, 1794).
distance(halifax, victoria, 6230).
distance(halifax, winnipeg, 3499).
distance(montreal, calgary, 3535).
distance(montreal, charlottetown, 1145).
distance(montreal, halifax, 1240).
distance(montreal, ottawa, 198).
distance(montreal, toronto, 542).
distance(montreal, victoria, 4978).
distance(montreal, winnipeg, 2269).
distance(ottawa, calgary, 3339).
distance(ottawa, charlottetown, 1340).
distance(ottawa, halifax, 1435).
distance(ottawa, montreal, 198).
distance(ottawa, toronto, 450).
distance(ottawa, victoria, 4429).
distance(ottawa, winnipeg, 2140).
distance(toronto, calgary, 3413).
distance(toronto, charlottetown, 1697).
distance(toronto, halifax, 1792).
distance(toronto, montreal, 541).
distance(toronto, ottawa, 449).
distance(toronto, victoria, 4450).
distance(toronto, winnipeg, 2232).
distance(victoria, calgary, 1060).
distance(victoria, charlottetown, 6132).
distance(victoria, halifax, 6227).
distance(victoria, montreal, 4976).
distance(victoria, ottawa, 4430).
distance(victoria, toronto, 4449).
distance(victoria, winnipeg, 2383).
distance(winnipeg, calgary, 1329).
distance(winnipeg, charlottetown, 3407).
distance(winnipeg, halifax, 3501).
distance(winnipeg, montreal, 2271).
distance(winnipeg, ottawa, 2142).
distance(winnipeg, toronto, 2233).
distance(winnipeg, victoria, 2384).
distance(_,_,_) :- fail.

:- dynamic(total_path/1).
total_path(0).

:- dynamic(min_path/1).
min_path([]).

:- dynamic(min_distance/1).
min_distance(999999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules

% citcuits(P) succeeds if P is a circuit of the cities. It uses permute to generate
% circuits starting and ending at the first city in cities.
circuits(P) :- cities([H|T]), permutation(T,Z), append([H|Z],[H], P).  

% path_length(P,N) succeeds if the length of the path along the cities in P is N.
% If the distance between a pair of cities in P is unknown, or the path is empty,
% then the predicate fails. The length of a single city path is 0.
path_length([],_) :- fail.
path_length([_|[]], N) :- total_path(X), N is X, reset_accumulator. 
path_length(P,N)  :- P = [C1,C2|T], accumulator(C1,C2), path_length([C2|T],N).
    
% set up min_path and min_distance variables as discussed in the lab.
% set_min_path(P) takes a path and succeeds if the path is shorter than the recorded path;
% if it succeeds, it also sets the recorded path and distance.
set_min_path(P) :- path_length(P, N), min_distance(M), N < M, retractall(min_path(_)),
	retractall(min_distance(_)), asserta(min_path(P)), asserta(min_distance(N)).  

% dynamic varible accumulator for paths
% accumulator is read pathlength from C1 to C2 will be added to total_path dynamic variable 
accumulator(C1,C2) :- \+distance(C1,C2, _), reset_accumulator, fail. 
accumulator(C1, C2) :- 
	distance(C1,C2,D),
	total_path(X),
	Z is D + X,
	retractall(total_path(_)),
	asserta(total_path(Z)).

% resets the total_path variable between path accumulations.	
reset_accumulator :- 
	retractall(total_path(_)), 
	asserta(total_path(0)).

% reset_min_path sets the min_path to [] and the min_distance to 999999.
reset_min_path :- retractall(min_path(_)),
	retractall(min_distance), asserta(min_distance(999999)), asserta(min_path([])).

% record_min_circuit always fails. But it resets the min_path variables, generates
% all circuits starting and ending at the first city, and records the shortest circuit found.
record_min_circuit :- reset_min_path, circuits(P), set_min_path(P), fail. 


% min_circuit(P,D) succeeds when the shortest circuit through the cities is P and
% its length is D
min_circuit(P,D) :- \+record_min_circuit, min_path(M), min_distance(N), M == P, D == N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests

test_circuits :-
   cities(C), append(C,[calgary], P), circuits(P),
   circuits([calgary, victoria, winnipeg, ottawa, toronto, halifax, charlottetown, montreal, calgary]),
   \+circuits([charlotesville,wanyesboro,staunton, grottoes, verona]).
   
test_path_length :-
   \+path_length([],_),
   path_length([any],0),
   path_length([victoria,calgary], 1060),
   \+path_length([victoria,none,calgary], _),
   path_length([victoria,calgary,winnipeg,toronto], 4620).

test_min_path :-
	set_min_path([toronto,montreal,victoria]),
	\+set_min_path([halifax,victoria,ottawa]),
    set_min_path([ottawa,toronto,montreal]),
	\+set_min_path([charlottetown,ottawa,winnipeg]),
   min_path([ottawa,toronto,montreal]),
   min_distance(991).	
   
test_accumulator :- accumulator(toronto, montreal), total_path(541).	

test_reset_accumulator :- reset_accumulator, 
	accumulator(victoria, calgary),
	total_path(1060),
	reset_accumulator,
	total_path(0).
	
test_reset_min_path :-
   asserta(min_path([a,b,c])),
   asserta(min_distance(8)),
   reset_min_path,
   min_path([]),
   min_distance(999999).

test_record_min_circuit :-
   \+record_min_circuit,
   min_path([calgary, victoria, toronto, montreal, halifax, charlottetown, ottawa, winnipeg, calgary]),
   min_distance(12423). 

test_min_circuit :-
   min_circuit([calgary, victoria, toronto, montreal, halifax, charlottetown, ottawa, winnipeg, calgary], 12423).
   
test :-
   test_path_length,
   test_circuits,
   test_reset_min_path,
   test_min_path,
   test_record_min_circuit,
   test_min_circuit,
   !.
