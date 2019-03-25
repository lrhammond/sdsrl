:- use_module(library(planning)).
:- use_module(library(lists)).
:- use_module(library(distributionalclause)).
:- use_module(library(dcpf)).
:- use_module(library(sst)).
:- use_module(library(system)).

% Options
:- set_options(default),
   set_query_propagation(true),
   set_inference(backward(lazy)).
:- set_current2nextcopy(false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_pos(X):t <- pos:t ~= distribution(val(X)).
get_pos(X):t <- pos:t ~= X.

reward:t ~ val(R) <- stop:t, R is 10.0.
reward:t ~ val(R) <-  \+stop:t, R is -1.0.
stop:t <- get_pos(X):t, X>4.

adm(action(move(A,B))):t <-
   member((A,B),[(1.0,0.0),(-1.0,0.0)]).

pos:t+1 ~ val(X) <-
   \+pos:t ~= _,
   observation(pos) ~= X.
pos:t+1 ~ val(X) <-
   observation(pos) ~= X.
pos:t+1 ~ val(NX) <-
   action(move(DX,DY)),
   pos:t ~= X,
   NX is X+DX.
pos:t+1 ~ val(X) <-
   pos:t ~= X.

observation(pos):t+1 ~ val(_) <-
   pos:t+1 ~= _.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
maxV(D,100):t <- true.

par(left_right,N,UsedD,End) :-
	End=1,
	N=200, % max number of samples (for the entire plan)
	UsedD=12, % planner horizon
	getparam(left_right).

getparam(left_right) :-
	bb_put(user:spant,0),
	setparam(
        % enable abstraction
        false,
        % ratio of the samples reserved for the first action
        1.0,
        % use correct formula (leave true)
        true,
        % strategy to store V function
        max,
        % ExecAction
        best,
        % most,
        % Domain
        propfalse,
        % relfalse,
        % Discount
        0.95,
        % probability to explore in the beginning (first sample)
        0.0,
        % probability to explore in the end (last sample)
        0.0,
        % number of previous samples to use to estimate Q. Larger is better but slower
        100,
        % max horizon span
        100,
        % lambda init
        0.9,
        % lambda final
        0.9,
        % UCBV
        false,
        % decay
        0.015,
        % action selection: softmax | egreedy
        softmax,
        % egreedy,
        % Pruning
        0,
        % WHeuInit
        -0.1,
        % WHeuFinal
        -0.1),
        !.

getparam2(left_right,N) :-
  par(left_right,N,_,_).

score(_,Avg,Avg).

search_query(I,Q) :-
	distributionalclause:proof_query_backward(I,Q).


% plotepisode(E,N) :-
% %	dcpf:bb_get(offset,Offset),
% 	open('dataE.txt','write',S),
% 	NN is N+1,
% 	term_to_atom(p(NN,E),I),
% %	writeln('datatest1.txt'),
% %	I is Offset+1,
% 	(
% 		search_query(I,next(object(ID)) ~= (X,Y,Z)),
% 		dim(ID,(DX,DY,DZ)),
% 		rgbcolor(ID,(Rc,Gc,Bc)),
% 		write(S,ID),write(S,' '),
% 		write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
% 		write(S,Rc),write(S,' '),write(S,Gc),write(S,' '),write(S,Bc),nl(S),
% 		fail;
% 		true
% 	),
%
% 	forall(	between(0,N,T),
% 	(
% 		D is N-T,
% 		term_to_atom(p(D,E),Key),
% 		search_query(Key,next(object(ID)) ~= (X,Y,Z)),
% 		search_query(Key,next(greedy(GR))),
% 		rgbcolor(ID,(Rc,Gc,Bc)),
% 		write(S,ID),write(S,' '),
% 		write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
% 		write(S,Rc),write(S,' '),write(S,Gc),write(S,' '),write(S,Bc),write(S,' '),write(S,GR),nl(S)
% 		;true
% 	)
% 	),
% 	nl(S),
% 	close(S),!.


myplotV(MinE,MaxE,Depth) :-
	% dcpf:bb_get(offset,Offset),
	% open('dataV.txt','write',S),
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
		writeln(T),writeln(' '),
		writeln(X),writeln(' '),writeln(Y),writeln(' '),writeln(Z),writeln(' '),
		writeln(Color),writeln(' '),writeln(Color),writeln(' '),writeln(Color),nl,
		fail;
		true
	).






% plotV(MinE,MaxE,Depth) :-
% 	dcpf:bb_get(offset,Offset),
% 	open('dataV.txt','write',S),
% 	abolish_all_tables,
% 	(
% 		between(1,Depth,T),
% 		%T is Depth-2, % to remove
% 		between(MinE,MaxE,E),
% 		term_to_atom(p(T,E),Key),
% 		search_query(Key,next(object(ID)) ~= (X,Y,Z)),
% 		search_query(Key,v(next,V)),
% 		( ->
% 		PropLikelihood is SumPropLikelihood/NumValues
% 		;
% 		(PropLikelihood is 0)
% 		),
% 		%Temp is sign(V)*sqrt(abs(V))/100+0.99,
% 		Color is PropLikelihood,%min(1,max(0,Temp)),% min(1,max(0,V)),
% 		%Color2 is V,
% 		%rgbcolor(ID,(Color,Color,Color)),
% 		write(S,T),write(S,' '),
% 		write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
% 		write(S,Color),write(S,' '),write(S,Color),write(S,' '),write(S,Color),nl(S),
% 		fail;
% 		true
% 	),
% 	nl(S),
% 	close(S).



myfullplan(File,Init,AVG,Instance,D,Times1,Notes) :-
	statistics(runtime,_),
	par(Instance,N,UsedD,Startp),
%	Init=[],
	bb_get(user:abstraction,Abstract),
	resamplingplan(0,Abstract,Init,[],N,D,AVG,T1,UsedD,Startp,Endp),
	writeln(resamplingplan(0,Abstract,Init,[],N,D,AVG,T1,UsedD,Startp,Endp)),
	(T1==true -> bb_put(numterminatedexplans,1);bb_put(numterminatedexplans,0)),
%	setparam(1,0,0,200,0,0.9,0.9,false,0.015,egreedy,200,0.00000001,-0.0001),
	getparam2(Instance,D2),
%	D2 is D*2,
	bb_put(currentepisode,Endp),
	Times is Times1-1,
%	Notes1='withoutrestart+100',
	Notes1='withrestart',
	findall(AVG2,
		(between(1,Times,X),
		bb_get(currentepisode,Start2),
		Start3 is Startp+100*Times,
		Start4 is 1, %Start3,% Start3,%Start2,%
		resamplingplan(0,Abstract,Init,[],D2,D,AVG2,T2,UsedD,Start4,End),
		writeln(resamplingplan(times(Times),0,Abstract,Init,[],D2,D,AVG2,T2,UsedD,Start4,End)),
		EndEpisode is Start2+End-Start4,
		bb_put(currentepisode,EndEpisode),
		bb_get(numterminatedexplans,OldNumT),
		(T2==true -> NewNumT is OldNumT+1;NewNumT is OldNumT),
		bb_put(numterminatedexplans,NewNumT)
	),L),
	statistics(runtime,[_,Time]),
	writeparam,nl,
	bb_get(currentepisode,TotalN),
	sum_list([AVG|L],Sum),
	length([AVG|L],Length),
	AVGFinal is Sum/Length,
	variance([AVG|L],AVGFinal,Var),
	STD is sqrt(Var),
	getparam(Instance),
	writeparam,

	findall(Score,(member(Elem,[AVG|L]),score(Instance,Elem,Score)),L2),
	sum_list(L2,SumScore),
	length(L2,LengthScore),
	AVGScore is SumScore/LengthScore,
	variance(L2,AVGScore,VarScore),
	Conf95 is 1.96*sqrt(VarScore)/sqrt(LengthScore),


	bb_get(numterminatedexplans,Terminated),
	T is round(Time/10)/100/Length,
	writeln(seconds(T)),
	score(Instance,AVGFinal,Score2),
	writeln(([AVG|L],AVGFinal,Score2,AVGScore+Conf95,Length,VarScore,Terminated)),
	(host_name(Host)->true;Host=unknown),

	writetofile(File,Instance,AVGFinal,AVGScore,Conf95,Startp+N+D2*Times=TotalN,D,UsedD,T,([AVG|L],Terminated,Host,Notes,Notes1)),
	!.

% plotV(1,100,10) :- !.
% plotepisode(100,100) :- !.

test:- myfullplan('resultspush.csv',[observation(pos)~=(0)],AVG,left_right,10,10,' ').
