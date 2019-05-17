% MWE

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

% Parameters
maxV(D,100):t <- true.
getparam(params) :- bb_put(user:spant,0),
                    setparam(
                        % Enable abstraction
                        true,
                        % Ratio of the samples reserved for the first action
                        1.0,
                        % Use correct formula (leave true)
                        true,
                        % Strategy to store V function
                        max,
                        % Execute action
                        best,
                        % most,
                        % Domain
                        propfalse,
                        % relfalse,
                        % Discount
                        1.00,
                        % Probability to explore in the beginning (first sample)
                        0.25,
                        % Probability to explore in the end (last sample)
                        0.05,
                        % Number of previous samples to use to estimate Q (larger is better but slower)
                        100,
                        % Max horizon span
                        200,
                        % Lambda init
                        0.9,
                        % Lambda final
                        0.9,
                        % UCBV
                        false,
                        % Decay
                        0.015,
                        % Action selection
                        softmax,
                        % egreedy,
                        % Pruning
                        0,
                        % WHeuInit
                        -0.1,
                        % WHeuFinal
                        -0.1),
                    !.

% Needed, I think...
Var:t+1 ~ val(Val) <- observation(Var) ~= Val.
observation(Var):t+1 ~ val(Val) <- Var:t+1 ~= Val.

% Constants
constants <- No = no, Yes = yes, Wall = wall, Agent = agent.
constants.

% Actions
adm(action(A)):t <- member(A, [l,r,n]).

% Set nothing predicate to yes for the 'no object' constant
% nothing(no_object):t ~= val(Yes) <- Yes = yes.
% nothing(no_object):t+1 ~= val(Yes) <- Yes = yes.

% Neighbour on right
nbRpred(X, Y, NbR):t <- NbX is X + 1, NbY is Y, x_pos(NbR):t ~= NbX, y_pos(NbR):t ~= NbY.
nbR(Obj):t ~ val(NbR) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y, map(X, Y, NbR):t.
% nbR(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nbRpred(X, Y, _):t)).
% nbR(Obj):t+1 ~ val(NbR) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nbRpred(NX, NY, NbR):t.
% nbR(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nbRpred(NX, NY, _):t)).

% Neighbour on left
nbLpred(X, Y, NbL):t <- NbX is X - 1, NbY is Y, x_pos(NbL):t ~= NbX, y_pos(NbL):t ~= NbY.
nbL(Obj):t ~ val(NbL) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nbLpred(X, Y, NbL):t.
% nbL(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nbLpred(X, Y, _):t)).
% nbL(Obj):t+1 ~ val(NbL) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nbLpred(NX, NY, NbL):t.
% nbL(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nbLpred(NX, NY, _):t)).

% Attempt 1
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1, y_pos(Obj):t ~= Y, map(New, Y, no_object):t.
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.



s1(Obj, New):t <- colour(Obj):t ~= agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1, y_pos(Obj):t ~= Y, map(New, Y, NB):t, nothing(NB):t ~= yes.
s2(Obj, New):t <- colour(Obj):t ~= agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1, y_pos(Obj):t ~= Y, map(New, Y, NB):t, nothing(NB):t ~= yes.

s1no(Obj, New):t <- nothing(Obj):t ~= yes, x_pos(Obj):t ~= X, s1(NB, X):t, New is X - 1.
s2no(Obj, New):t <- nothing(Obj):t ~= yes, x_pos(Obj):t ~= X, s2(NB, X):t, New is X + 1.


map(X, Y, Obj):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y.
% map(X, Y, no_object):t <- \+((x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y)).

map(New, Y, Obj):t+1 <- schema_x_pos(Obj, New):t, y_pos(Obj):t ~= Y.

% map(X, Y, no_object):t+1 <- schema_x_pos(Obj, New):t, y_pos(Obj):t ~= Y, x_pos(Obj):t ~= X.
map(Curr, Y, Obj):t+1 <- no_schema_x_pos(Obj, Curr):t, y_pos(Obj):t ~= Y.


schema_x_pos(Obj, New):t <- s1(Obj, New):t.
schema_x_pos(Obj, New):t <- s2(Obj, New):t.
schema_x_pos(Obj, New):t <- s1no(Obj, New):t.
schema_x_pos(Obj, New):t <- s2no(Obj, New):t.

% Attempt 2
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1, nbR(Obj):t ~= NbR, NbR = no_object.
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.

% Attempt 3
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= X, x_pos(Obj):t ~= Y, New is X + 1, \+((x_pos(NbR):t ~= New, y_pos(NbR):t ~= Y)).
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(l), x_pos(Obj):t ~= X, New is X - 1.

% When no x position schema is activated
no_schema_x_pos(Obj, Curr):t <- \+schema_x_pos(Obj, _):t, x_pos(Obj):t ~= Curr.

% Update x position depending on schema
x_pos(Obj):t+1 ~ val(New) <- schema_x_pos(Obj, New):t.
x_pos(Obj):t+1 ~ val(New) <- no_schema_x_pos(Obj, New):t.
% new_x_pos(Obj, NX):t <- schema_x_pos(Obj, NX):t.
% new_x_pos(Obj, NX):t <- no_schema_x_pos(Obj, NX):t.

% Other variables stay the same
y_pos(Obj):t+1 ~ val(Curr) <- y_pos(Obj):t ~= Curr.
colour(Obj):t+1 ~ val(New) <- colour(Obj):t ~= New.
nothing(Obj):t+1 ~ val(New) <- nothing(Obj):t ~= New.

% Reward Schemas
r(X, Y, Type, R) <- R = 10000, X > 3, Type = agent.
r(X, Y, Type, R) <- R = 1000, X = 3, Type = agent.
r(X, Y, Type, R) <- R = -100, X = 1, Type = agent.

% Produce reward depending on schema
schema_reward(Obj):t ~ val(R) <- map(X, Y, Obj):t, colour(Obj):t ~= Type, r(X, Y, Type, R).
schema_reward(Obj):t ~ val(-1) <- map(X, Y, Obj):t, colour(Obj):t ~= Type, \+r(X, Y, Type, _).
reward:t ~ val(R) <- schema_reward(Obj):t ~= R.

% Run example with three objects, an agent between two walls
c :- executedplan_start, executedplan_step(BA,false,
                              [observation(x_pos(obj0)) ~= 0,
                              observation(y_pos(obj0)) ~= 0,
                              observation(colour(obj0)) ~= wall,
                              observation(nothing(obj0)) ~= no,
                              observation(x_pos(obj1)) ~= 4,
                              observation(y_pos(obj1)) ~= 0,
                              observation(colour(obj1)) ~= wall,
                              observation(nothing(obj1)) ~= no,
                              observation(x_pos(obj2)) ~= 3,
                              observation(y_pos(obj2)) ~= 0,
                              observation(colour(obj2)) ~= agent,
                              observation(nothing(obj2)) ~= no,
                              observation(x_pos(no_obj0)) ~= 2,
                              observation(y_pos(no_obj0)) ~= 0,
                              observation(colour(no_obj0)) ~= none,
                              observation(nothing(no_obj0)) ~= yes,
                              observation(x_pos(no_obj1)) ~= 1,
                              observation(y_pos(no_obj1)) ~= 0,
                              observation(colour(no_obj1)) ~= none,
                              observation(nothing(no_obj1)) ~= yes]
                        ,100,6,TotalR,T,6,STOP).
