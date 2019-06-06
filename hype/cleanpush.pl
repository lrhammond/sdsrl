%%% -*- Mode: Prolog; -*-

% :- use_module(library(distributionalclause)).
% :- use_module(library(dcpf)).
% :- use_module(library(sst)).
% :- use_module(library(planning)).
% :- use_module(library(lists)).
% :- use_module(library(system)).
%
% :- set_options(default).
% :- set_current2nextcopy(false).


% Libraries
:- use_module(library(planning)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(dcpf)).
:- use_module(library(distributionalclause)).
:- use_module(library(sst)).

% Options
:- set_options(default).
:- set_query_propagation(true).
:- set_inference(backward(lazy)).
:- set_current2nextcopy(false).


builtin(dim(_,_)).
builtin(color(_,_)).
builtin(type(_,_)).
builtin(deltaT(_)).
builtin(varQ(_)).
builtin(cov(_,_,_)).

deltaT(1).
varQ(0.0004).

maxV(100).
maxV(D,V) :- V is 100-1.
maxV(D,100):t <- true.

cov(1,[Cov],VarQ) :-
	deltaT(DeltaT),
	Cov is VarQ*DeltaT^2.

%dim(0,(2,2,0.01)).
dim(table,(11,11,2)).
dim(1,(0.02,0.02,0.02)).
dim(2,(0.20,0.255,0.155)).
dim(3,(0.05,0.09,0.05)).
dim(4,(0.055,0.19,0.145)).
dim(5,(0.08,0.08,0.05)).
dim(6,(0.03,0.03,0.03)).
dim(7,(1.1,0.55,0.4)).

type(1,cube).
color(1,green).
rgbcolor(1,(0.0,1.0,0.0)).

color(2,brown).
rgbcolor(2,(0.8,0.5,0.1)).
type(2,box).

color(3,blue).
rgbcolor(3,(0.0,0.0,1.0)).
type(3,cube).

color(4,fucsia).
rgbcolor(4,(1.0,0.0,1.0)).
type(4,box).

color(5,grey).
rgbcolor(5,(0.3,0.3,0.3)).
type(5,cylinder).

color(6,white).
rgbcolor(6,(1.0,1.0,1.0)).
type(6,cube).

color(7,white).
rgbcolor(7,(1.0,1.0,1.0)).
type(7,table).

reward:t ~ val(100) <-
	stop:t.

reward:t ~ val(-10) <-
	\+stop:t,
	object(ID):t ~= (X,Y,Z),
	sqrt((X-0.5)^2+(Y-0.8)^2)<0.2.

reward:t ~ val(-1) <-
	\+stop:t,
	object(ID):t ~= (X,Y,Z),
	sqrt((X-0.5)^2+(Y-0.8)^2)>=0.2.

% stop condition (if the plan has terminal states, e.g. when you reach the goal)
stop:t <-
	object(ID):t ~= (X,Y,Z),
	sqrt((X-0.6)^2+(Y-1)^2)<0.1.

% observation is used to set the current position (fully observable model)
object(ID):0 ~  val(P) <-
	observation(object(ID)) ~= P.

object(ID):t+1 ~  val(P) <-
	observation(object(ID)) ~= P.

% observation is used to set the current position (fully observable model)
object(ID):t+1 ~  indepGaussians([ ([X],[0]), ([Y],[0]), ([Z],[0]) ]) <-
	observation(object(ID)) ~=(X,Y,Z),
	\+(object(ID):t ~=_).


object(ID):t+1 ~ indepGaussians([ ([NX],Cov), ([NY],Cov), ([0],Cov) ]) <-
	object(ID):t ~= (X,Y,Z),
%	\+stop:t,
	\+observation(object(ID)) ~=_,
	action(move(DX,DY)),
	NX is X+DX,
	NY is Y+DY,
	varQ(VarQ),
	cov(1,Cov,VarQ),
	deltaT(DeltaT).

object(ID):t+1 ~ indepGaussians([ ([X],Cov), ([Y],Cov), ([Z],Cov) ]) <-
	object(ID):t ~= (X,Y,Z),
	\+observation(object(ID)) ~=_,
	\+action(move(ID,_,_)),
	varQ(VarQ),
	cov(1,Cov,VarQ),
	deltaT(DeltaT).

observation(object(ID)):t+1 ~ finite([1:_]) <-
	true.

% admissible actions
adm(action(move(A,B))):t <-
	%m(X,Y):t ~= (A,B).
	member((X,Y),[(-1,0),(1,0),(0,-1),(0,1)]),A is X/10,B is Y/10.

% random actions (not used)
m(X,Y):t ~ contUniform(A,B,C,D) <-
	between(1,3,X),
	between(1,3,Y),
	\+ (X=2,Y=2),
	A is (-1+X*2/3-2/3)/10,
	B is (-1+X*2/3)/10,
	C is (-1+Y*2/3-2/3)/10,
	D is (-1+Y*2/3)/10.






teest :-
	fullplan('resultspush.csv',[observation(object(1)) ~= (0.3,0.4,0)],AVG,test1,5,10,' ').




% fullplan execute the plan several times and store the performance in the specified file
% it uses the same model to find the best action and to simulate the execution of the action.
% fullplan(name_file,initial_state_list,Result,name_domain,number_trials,max_steps,empty_space)
% :- initialization(fullplan('resultspush.csv',[observation(object(1)) ~= (0.3,0.4,0)],AVG,test1,30,50,' ')).


% fullplantest([observation(object(1)) ~= (0.3,0.4,0)],AVG,test1,10)

%single step planner (online execution, e.g. for robotics)
% inititialize with: executedplan_start
% find best action from current state: executedplan_step(BestAction,use_abstraction,list_observations_current_state,samples,max_horizon,TotalReward,Time,used_horizon,STOP_condition_reached)
% call executedplan_start once, then executedplan_step to get the first action to execute, then you have to execute the action in the real world or simulation and call again executedplan_step with the new observation (state) you reached after the action, then you repeat.
% executedplan_start and executedplan_step are used in plan.cpp and planner.cpp for robotics.
% useless test:
testp :-
	executedplan_start,
	executedplan_step(BA,false,[observation(object(1)) ~= (0.3,0.4,0)],11,2,TotalR,T,2,STOP),
	executedplan_step(BA2,false,[observation(object(1)) ~= (0.3,0.4,0)],11,2,TotalR2,T2,2,STOP2),
	writeln((BA,T,BA2,T2)).

plotepisode(E,N) :-!. % if commented print info about the episode in a file for visualization
plotV(MinE,E,DD) :-!. % if commented print info about the V function in a file for visualization

% parameters used by fullplan
% namedomain,number samples, planner horizon, End (leave 1)
par(test1,N,UsedD,End) :-
	End=1,
	N=500, % max number of samples (for the entire plan)
	UsedD=12, % planner horizon
	getparam(test1).

% abolish_all_tables :-!.

% additional parameters for first step
getparam(test1) :-
	bb_put(user:spant,0),
	setparam(false,10,true,max,best,propfalse,1,0,0.4,50,0,1,1,false,0.015,egreedy,110,-0.1,-0.1),!.

% additional parameters for remaining steps (using the same)
getparam2(test1,N) :-
	par(test1,N,_,_).

getparam(pick1) :-
  bb_put(user:spant,0),
  setparam(
      % enable abstraction (true/false) you can try this
      false,
      % the ratio of samples reserved for the first action (e.g. 3 means that
      % the first action will get 3 samples more than the remaining actions).
      % the first action should get more samples because it search the entire
      % policy, the remaining steps are just and improving the found policy.
      25,
      % use correct formula for the proposal (leave true)
      true,
      % strategy used to store the V function. Use 'max' that generally works
      % better
      max,
      % ExecAction
      best,
      % Domain
      propfalse,
      % Discount
      0.9,
      % probability to explore in the beginning (first sample)
      0.3,
      % probability to explore in the end (last sample)
      0.4,
      % how many previous samples you want to use to estimate the Q, bigger
      % gives better performance but it is slower, around 100 is generally ok
      50,
      % Max horizon span
      0,
      % Lambda Init
      1,
      % Lambda Final
      1,
      % UCBV
      false,
      % Decay
      0.015,
      % Action selection
      egreedy,
      % higher is better but it is slower
      110,
      % WHeuInit
      -0.1,
      % WHeuFinal
      -0.1)
      ,!.


























% the rest is for plotting and testing
score(_,Avg,Avg).

fullplantest(Init,AVG,Instance,D) :-
	par(Instance,N,UsedD,Startp),
	statistics(runtime,_),
	init_particle(1),
	dcpf:step_particle([],Init,[],1,1),
	dcpf:bb_get(dcpf:offset,Offset),I is Offset+1,
	dcpf:clean_sample(realplan),
	dcpf:plaincopyparticles(I,realplan),
	printkeyp(realplan),
	plan(1,0,false,realplan,[],N,D,AVGFinal,T1,BAction),
	writeln(plan(1,0,false,realplan,[],N,D,AVGFinal,T1,BAction)),
	writeparam,
	statistics(runtime,[_,Time]),
	T is round(Time/10)/100,
	writeln(seconds(T)),
	writetofile('test1.csv',Instance,AVGFinal,0,T1,N,D,UsedD,T,' ').


averageobject(Particles,Mean) :-
	dcpf:bb_get(offset,Offset),
	bb_put(sumobj,0.0),
	(
		between(1,Particles,Pos),
		I is Offset+Pos,
		recorded(I,current(object) ~= Val,_),
		bb_get(sumobj,OldTOT),
		NewTOT is OldTOT+Val,
		bb_put(sumobj,NewTOT),
		fail;
		true
	),
	bb_delete(sumobj,T),
	Mean is T/Particles.

search_query(I,Q) :-
	distributionalclause:proof_query_backward(I,Q).

plotepisode(E,N) :-
%	dcpf:bb_get(offset,Offset),
	open('datatest.txt','write',S),
	NN is N+1,
	term_to_atom(p(NN,E),I),
%	writeln('datatest1.txt'),
%	I is Offset+1,
	(
		search_query(I,next(object(ID)) ~= (X,Y,Z)),
		dim(ID,(DX,DY,DZ)),
		rgbcolor(ID,(Rc,Gc,Bc)),
		write(S,ID),write(S,' '),
		write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
		write(S,Rc),write(S,' '),write(S,Gc),write(S,' '),write(S,Bc),nl(S),
		fail;
		true
	),

	forall(	between(0,N,T),
	(
		D is N-T,
		term_to_atom(p(D,E),Key),
		search_query(Key,next(object(ID)) ~= (X,Y,Z)),
		search_query(Key,next(greedy(GR))),
		rgbcolor(ID,(Rc,Gc,Bc)),
		write(S,ID),write(S,' '),
		write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
		write(S,Rc),write(S,' '),write(S,Gc),write(S,' '),write(S,Bc),write(S,' '),write(S,GR),nl(S)
		;true
	)
	),
	nl(S),
	close(S),!.

plotV(MinE,MaxE,Depth) :-
	dcpf:bb_get(offset,Offset),
	open('dataV.txt','write',S),
	abolish_all_tables,
	(
		between(1,Depth,T),
		%T is Depth-2, % to remove
		between(MinE,MaxE,E),
		term_to_atom(p(T,E),Key),
		search_query(Key,next(object(ID)) ~= (X,Y,Z)),
		search_query(Key,v(next,V)),
		(recorded(Key,proposallikelihood(NumValues,SumPropLikelihood,PiProp),_) ->
		PropLikelihood is SumPropLikelihood/NumValues
		;
		(PropLikelihood is 0)
		),
		%Temp is sign(V)*sqrt(abs(V))/100+0.99,
		Color is PropLikelihood,%min(1,max(0,Temp)),% min(1,max(0,V)),
		%Color2 is V,
		%rgbcolor(ID,(Color,Color,Color)),
		write(S,T),write(S,' '),
		write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
		write(S,Color),write(S,' '),write(S,Color),write(S,' '),write(S,Color),nl(S),
		fail;
		true
	),
	nl(S),
	close(S).
	%(system('mv data2temp.txt data2.txt');true).

plotremoved(MinE,MaxE,Depth) :-
	dcpf:bb_get(offset,Offset),
	open('data3.txt','write',S),
	abolish_all_tables,
	(
		between(0,Depth,T),
		between(MinE,MaxE,E),
		term_to_atom(p(T,E),Key),
		search_query(Key,next(object(ID)) ~= (X,Y,Z)),
		\+search_query(Key,v(next,V)),
		%Temp is sign(V)*sqrt(abs(V))/100+0.99,
		Color is 1+(E-MaxE)/MaxE,%min(1,max(0,Temp)),% min(1,max(0,V)),
		%Color2 is V,
		%rgbcolor(ID,(Color,Color,Color)),
		write(S,ID),write(S,' '),
		write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
		write(S,Color),write(S,' '),write(S,Color),write(S,' '),write(S,Color),nl(S),
		fail;
		true
	),
	nl(S),
	close(S).


plotV2(MinE,MaxE,T) :-
	dcpf:bb_get(offset,Offset),
	open('data2temp.txt','write',S),
	(
		%between(0,Depth,T),
		between(MinE,MaxE,E),
		term_to_atom(p(T,E),Key),
		search_query(Key,next(object(ID)) ~= (X,Y,Z)),
		search_query(Key,v(next,V)),
		Color is min(1,max(0,V)),
		%Color2 is V,
		%rgbcolor(ID,(Color,Color,Color)),
		write(S,ID),write(S,' '),
		write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
		write(S,Color),write(S,' '),write(S,Color),write(S,' '),write(S,Color),nl(S),
		fail;
		true
	),
	nl(S),
	close(S),
	system('mv data2temp.txt data2.txt').

plotdata(N) :-
	dcpf:bb_get(offset,Offset),
	open('datatest.txt','write',S),
	(
			between(1,N,Pos),
			I is Offset+Pos,
			search_query(I,current(object(ID)) ~= (X,Y,Z)),
			dim(ID,(DX,DY,DZ)),
			rgbcolor(ID,(Rc,Gc,Bc)),
			write(S,ID),write(S,' '),
			write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
			write(S,Rc),write(S,' '),write(S,Gc),write(S,' '),write(S,Bc),nl(S),
			fail;
			true
	),
	nl(S),
	close(S).

plotplanning(Depth) :-
	dcpf:bb_get(offset,Offset),
	open('datatest.txt','write',S),
	(
			between(0,Depth,I),
			search_query(I,current(object(ID)) ~= (X,Y,Z)),
			dim(ID,(DX,DY,DZ)),
			rgbcolor(ID,(Rc,Gc,Bc)),
			write(S,ID),write(S,' '),
			write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
			write(S,Rc),write(S,' '),write(S,Gc),write(S,' '),write(S,Bc),nl(S),
			fail;
			true
	),
	nl(S),
	close(S).
