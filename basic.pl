
:- use_module(library(planning)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(dcpf)).
:- use_module(library(distributionalclause)).
:- use_module(library(sst)).

% Options
:- set_options(default),
   set_query_propagation(true),
   set_inference(backward(lazy)).
:- set_current2nextcopy(false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_pos(X):t <- pos:t ~= distribution(val(X)).
% get_pos(X):t <- pos:t ~= X.

% KINDA WORKS
% rewardvalue(R):t :- (stop:t -> R = 1000 ; R = -1).
% reward:t ~ val(R) <- rewardvalue(R):t.
% reward:t ~ val(0) <- \+rewardvalue(R):t.

% WORKS!
% r1(X, Y) <- X=<2, Y = -1.
% r2(X, Y) <- X>2, Y = 100.
% reward:t ~ val(Y) <- posX(Obj):t ~= X, r1(X, Y).
% reward:t ~ val(Y) <- posX(Obj):t ~= X, r2(X, Y).
% reward:t ~ val(0) <- posX(Obj):t ~= X, \+r1(X, Y), \+r2(X, Y).

% SO DOES THIS!
% r1(X) <- X=<2.
% r2(X) <- X>2.
% reward:t ~ val(-1) <- posX(Obj):t ~= X, r1(X).
% reward:t ~ val(100) <- posX(Obj):t ~= X, \+r1(X).

% BEST SO FAR
% r1(X,Y) <- X=2, Y=2.
% r2(X,Y) <- -Y >= X + 2.
% reward(Obj):t ~ val(100) <- attributes(Obj, X, Y):t, r1(X,Y).
% reward(Obj):t ~ val(10) <- attributes(Obj, X, Y):t, r2(X,Y).
% reward(Obj):t ~ val(-1) <- attributes(Obj, X, Y):t, \+r1(X,Y), \+r2(X,Y).
% attributes(Obj, X, Y):t <- posX(Obj):t ~= X, posY(Obj):t ~= Y.
% reward:t ~ val(R) <- reward(Obj):t ~= R.

% NEW BEST
r(X,Y,R) <- R=100, X>=1, Y>=1, X=<5, Y=<5.
r(X,Y,R) <- R=10, -Y >= X + 2.
reward(Obj):t ~ val(R) <- attributes(Obj, X, Y, S):t, r(X,Y,R).
reward(Obj):t ~ val(-1) <- attributes(Obj, X, Y, S):t, \+r(X,Y,R).
reward:t ~ val(R) <- reward(Obj):t ~= R.

% r(Obj,R):t <- posX(Obj):t ~= X, posY(Obj):t ~= Y, R=100, X=2, Y=2.
% r(Obj,R):t <- posX(Obj):t ~= X, posY(Obj):t ~= Y, R=10, -Y >= X + 2.
% r_yes(Obj, R):t <- r(Obj,R):t.
% r_no(Obj, R):t <- R = -1 , \+r(Obj,Reward):t.
% reward(Obj):t ~ val(R) <- r_yes(Obj, R):t.
% reward(Obj):t ~ val(R) <- r_no(Obj, R):t.
% reward:t ~ val(R) <- reward(Obj):t ~= R.

attributes(Obj, X, Y, S):t <- posX(Obj):t ~= X, posY(Obj):t ~= Y, shape(Obj):t ~= S.

% r1(Obj) <- posX(Obj):t ~= X, X>2.
% % r2(Obj) <- posX(Obj):t ~= X, X>2.
% reward(Obj):t ~ val(10) <- r1(Obj).
% % reward(Obj):t ~ val(100) <- r2(Obj).
% reward(Obj):t ~ val(-1) <- \+r1(Obj).
% object(Obj) <- member(Obj, [obj]).

% r1(Obj, R):t <- posX(Obj):t ~= X, X = 2, posY(Obj):t ~= Y, Y = 2, R = 100.
% r2(Obj, R):t <- posX(Obj):t ~= X, posY(Obj):t ~= Y, Y =< Bound, Bound is -(X + 2), R = 10.
% rn(Obj, R) <- \+r1(Obj, R), R = -1.
%
% reward(Obj):t ~ val(R) <- r1(Obj, R):t.
% reward(Obj):t ~ val(R) <- r2(Obj, R):t.
% reward(Obj):t ~ val(R) <- rn(Obj, R):t.

% reward:0 ~ val(0) <- true.

% c1(Obj):t <- attributes(Obj, X, Y):t, \+X=1, \+Y=1.
% c1(Obj):t.

adm(action(A)):t <-
   member(A,[l, d, u, r, none]).

% pos:t+1 ~ val(X) <-
%    \+pos:t ~= _,
%    observation(pos) ~= X.

% pos:t ~ val(X) <- pos ~= X.

% pos:t+1 ~ val(X) <- start ~= X.
% % pos:t+1 ~ val(X) <- pos ~= X.
% observation(pos):t+1 ~ val(_) <- pos:t+1 ~= _.

% observation(pos):t+1 ~ val(X) <- observation(pos) ~= X.

Y:t+1 ~ val(X) <- observation(Y) ~= X.
observation(Y):t+1 ~ val(X) <- Y:t+1 ~= X.
%
% observation(Y):t+1 ~ val(X) <- \+(Y:t+1 ~= X), Y:t ~= X.


% Att:t+1 ~ val(X) <- \+Att:t ~= _, observation(Att) ~= X.
%
% % Att:t+1 ~ val(NX) <-
% %    action(move(DX,DY)),
% %    Att:t ~= X,
% %    NX is X+DX.
% Att:t+1 ~ val(X) <- Att:t ~= X.
%
% observation(Att):t+1 ~ val(Val) <- Att:t+1 ~= Val.
% Att:t+1 ~ val(X) <- observation(Att) ~= X.

% Y:t+1 ~ val(X) <- action(_), Y:t ~= X.

% posX(Obj):t+1 ~ val(NX) <- action(A), posX(Obj):t ~= X, NX is X.
% posY(Obj):t+1 ~ val(NX) <- action(A), posY(Obj):t ~= X, NX is X.

% sl(Obj, NX):t <- action(l), posX(Obj):t ~= X, NX is X-1.
% sr(Obj, NX):t <- action(r), posX(Obj):t ~= X, NX is X+1.
% sd(Obj, NY):t <- action(d), posY(Obj):t ~= Y, NY is Y-1.
% su(Obj, NY):t <- action(u), posY(Obj):t ~= Y, NY is Y+1.
% xn(Obj, NX):t <- \+sl(Obj, NX):t, \+sr(Obj, NX):t, posX(Obj):t ~= X, NX is X.
% yn(Obj, NY):t <- \+su(Obj, NY):t, \+sd(Obj, NY):t, posY(Obj):t ~= Y, NY is Y.
%
% posX(Obj):t+1 ~ val(NX) <- sl(Obj, NX):t.
% posX(Obj):t+1 ~ val(NX) <- sr(Obj, NX):t.
% posX(Obj):t+1 ~ val(NX) <- xn(Obj, NX):t.
% posY(Obj):t+1 ~ val(NY) <- sd(Obj, NY):t.
% posY(Obj):t+1 ~ val(NY) <- su(Obj, NY):t.
% posY(Obj):t+1 ~ val(NY) <- yn(Obj, NY):t.

% existsRound(Obj):t ~ val(RoundObj) <- shape(RoundObj):t ~= Round, member(RoundObj, [obj,obj2]).

% existsRound(Obj):t ~ val(RoundObj) <- shape(RoundObj):t ~= Goal, member(RoundObj, [obj,obj2]).
% existsRound(Obj):t ~ val(nope) <- \+((shape(RoundObj):t ~= Goal, member(RoundObj, [obj,obj2]))).



% THIS WORKS!!!!!!!!!!!!!!!!!!!
% existsRound(RO):t <- shape(RO):t ~= Round.
% existsRO(Obj):t ~ val(RO) <- existsRound(RO):t.
% existsRO(Obj):t ~ val(nope) <- \+existsRound(RO):t.
% s1(Obj, NX):t <- action(l), shape(Obj):t ~= Square, posX(Obj):t ~= X, NX is X-1.
% s1(Obj, NX):t <- action(r), shape(Obj):t ~= Square, posX(Obj):t ~= X, NX is X+1.
% xn(Obj, NX):t <- \+s1(Obj, _):t, posX(Obj):t ~= NX.
% posX(Obj):t+1 ~ val(NX) <- s1(Obj, NX):t, existsRound(RO):t, posX(RO):t ~= F, F > 10.
% posX(Obj):t+1 ~ val(NX) <- \+s1(Obj, _):t, posX(Obj):t ~= NX, existsRound(RO):t, posX(RO):t ~= F, F > 10.




% existsRound(RO):t <- shape(RO):t ~= Round.
% existsRO(Obj):t ~ val(RO) <- existsRound(RO):t.
% existsRO(Obj):t ~ val(nope) <- \+existsRound(RO):t.

minus1(X,Y) <- Y is X - 1.
newpred(Obj):t ~ val(RO) <- shape(RO):t ~= S.

s1(Obj, NX):t <- action(l), shape(Obj):t ~= Square, posX(Obj):t ~= X, NX is X-1, newpred(Obj):t ~= RO, x_pos(RO):t < 0.
s1(Obj, NX):t <- action(r), shape(Obj):t ~= Square, posX(Obj):t ~= X, NX is X+1, newpred(Obj):t ~= RO, x_pos(RO):t < 0.
xn(Obj, NX):t <- \+s1(Obj, _):t, posX(Obj):t ~= NX.

schema(Obj, X):t <- newpred(RO, X):t.

posX(Obj):t+1 ~ val(NX) <- s1(Obj, NX):t.
posX(Obj):t+1 ~ val(X) <- \+s1(Obj, _):t, posX(Obj):t ~= X.


% s1(Obj):t ~ finite([0.6:NX,0.2:X]) <- action(l), shape(Obj):t ~= Square, posX(Obj):t ~= X, NX is X-1.
% s1(Obj):t ~ val(NX) <- action(r), shape(Obj):t ~= Square, posX(Obj):t ~= X, NX is X+1.
% xn(Obj):t ~ val(NX) <- \+(s1(Obj):t ~= _), posX(Obj):t ~= NX.
%
% schema(Obj, X):t <- newpred(RO, X):t.
%
% posX(Obj):t+1 ~ val(NX) <- s1(Obj):t ~= NX.
% posX(Obj):t+1 ~ val(X) <- xn(Obj):t ~= NX, posX(Obj):t ~= X.





% posX(Obj):t+1 ~ val(NX) <- posX(Obj):t ~= X, NX is X+1.
% posY(Obj):t+1 ~ val(NY) <- posY(Obj):t ~= Y, NY is Y+1.

s2(Obj, NY):t <- action(d), shape(Obj):t ~= Square, posY(Obj):t ~= Y, NY is Y-1.
s2(Obj, NY):t <- action(u), shape(Obj):t ~= Square, posY(Obj):t ~= Y, NY is Y+1.
yn(Obj, NY):t <- \+s2(Obj, _):t, posY(Obj):t ~= NY.
posY(Obj):t+1 ~ val(NY) <- s2(Obj, NY):t.
posY(Obj):t+1 ~ val(NY) <- yn(Obj, NY):t.

shape(Obj):t+1 ~ val(New) <- shape(Obj):t ~= New.

% Constants
constants <- Square = square, Round = round, Goal = goal, No = no, Wall = wall, Agent = agent, Small = small, Hole = hole, Yes = yes.
constants.


% xn(Obj, NX):t <- \+s1(Obj, _):t, posX(Obj):t ~= X, posX(Obj):t ~= NX.
% yn(Obj, NY):t <- \+s2(Obj, _):t, posY(Obj):t ~= Y, posY(Obj):t ~= NY.


% no_schema_y_pos(Obj, New):t <- \+schema_y_pos(Obj, _):t, y_pos(Obj):t ~= New.


% posX(Obj):t+1 ~ val(NX) <- action(l), posX(Obj):t ~= X, NX is X-1.
% posX(Obj):t+1 ~ val(NX) <- action(r), posX(Obj):t ~= X, NX is X+1.
% posX(Obj):t+1 ~ val(NX) <- action(A), member(A, [u,d,none]), posX(Obj):t ~= X, NX is X.
% posY(Obj):t+1 ~ val(NX) <- action(d), posY(Obj):t ~= X, NX is X-1.
% posY(Obj):t+1 ~ val(NX) <- action(u), posY(Obj):t ~= X, NX is X+1.
% posY(Obj):t+1 ~ val(NX) <- action(A), member(A, [l,r,none]), posY(Obj):t ~= X, NX is X.

% posX(Obj):t+1 ~ val(NX) <- action(move(DX,DY)), posX(Obj):t ~= X, NX is X+DX.
% posY(Obj):t+1 ~ val(NY) <- action(move(DX,DY)), posY(Obj):t ~= Y, NY is Y+DY.

% pos:t+1 ~ val(X) <-
%    pos:t ~= X.

% executedplan_step(BAction,Abstract,Init,N,MaxD,TotalR,T,MaxDSearch,STOP)

b :- fullplan('bAssic.csv',[observation(pos)~=2],AVG,left_right,5,10,' ').
c :- executedplan_start,
	executedplan_step(BA,true,[observation(posX(obj))~=(0),observation(posY(obj))~=(0),observation(posX(obj2))~=(50),observation(posY(obj2))~=(50)],300,15,TotalR,T,15,STOP),
	% executedplan_step(BA2,false,[observation(pos)~=(2)],100,10,TotalR2,T2,10,STOP2),
	% writeln((BA,T,BA2,T2)).
  writeln(BA).

% par(left_right,N,UsedD,End) :-
% 	End=1,
% 	N=100, % max number of samples (for the entire plan)
% 	UsedD=6, % planner horizon
% 	getparam(left_right).
% getparam2(left_right,N) :-
% 		par(left_right,N,_,_).
% score(_,Avg,Avg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
maxV(D,100):t <- true.

getparam(left_right) :-
	bb_put(user:spant,0),
	setparam(
        % enable abstraction
        true,
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
        0.3,
        % probability to explore in the end (last sample)
        0.05,
        % number of previous samples to use to estimate Q. Larger is better but slower
        100,
        % max horizon span
        200,
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
