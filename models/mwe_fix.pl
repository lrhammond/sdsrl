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
adm(action(A)):t <- member(A, [l,r]).

% Set nothing predicate to yes for the 'no object' constant
nothing(no_object):t ~= val(Yes) <- Yes = yes.
nothing(no_object):t+1 ~= val(Yes) <- Yes = yes.

% Neighbour on right
nbRpred(X, Y, NbR):t <- NbX is X + 1, NbY is Y, x_pos(NbR):t ~= NbX, y_pos(NbR):t ~= NbY.
nbR(Obj):t ~ val(NbR) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nbRpred(X, Y, NbR):t.
nbR(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nbRpred(X, Y, _):t)).
% nbR(Obj):t+1 ~ val(NbR) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nbRpred(NX, NY, NbR):t.
% nbR(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nbRpred(NX, NY, _):t)).

% Neighbour on left
nbLpred(X, Y, NbL):t <- NbX is X - 1, NbY is Y, x_pos(NbL):t ~= NbX, y_pos(NbL):t ~= NbY.
nbL(Obj):t ~ val(NbL) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nbLpred(X, Y, NbL):t.
nbL(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nbLpred(X, Y, _):t)).
% nbL(Obj):t+1 ~ val(NbL) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nbLpred(NX, NY, NbL):t.
% nbL(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nbLpred(NX, NY, _):t)).

% Attempt 1
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1, nbR(Obj):t ~= NbR, nothing(NbR):t ~= Yes.
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.

% Attempt 2
schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1, nbR(Obj):t ~= NbR, NbR = no_object.
schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.

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
reward:t ~ val(1) <-  x_pos(obj2):t ~= 1.
reward:t ~ val(10) <- x_pos(obj2):t ~= 2.
reward:t ~ val(100) <- x_pos(obj2):t ~= 3.
reward:t ~ val(0) <- x_pos(obj2):t ~= X, X \= 1, X \= 2, X \= 3.

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
                              observation(nothing(obj2)) ~= no]
                        ,100,6,TotalR,T,6,STOP).
