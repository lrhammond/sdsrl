% HYPE MWE

% Libraries
% I haven't actually tried removing any of these to see whether they're all necessary.
:- use_module(library(planning)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(dcpf)).
:- use_module(library(distributionalclause)).
:- use_module(library(sst)).

% Options
% These were the options that I used to make my examples work, there might be better alternatives.
:- set_options(default).
:- set_query_propagation(true).
:- set_inference(backward(lazy)).
:- set_current2nextcopy(false).

% Parameters
% Most of these can be edited as you see fit. Note that the 'Enable abstraction' parameter is overidden by the second argument of 'executedplan_step'.
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
                        1,
                        % Probability to explore in the beginning (first sample)
                        0.25,
                        % Probability to explore in the end (last sample)
                        0.15,
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

% Functions
% It will probably break if you don't use the first two lines here. The attributes predicate can be helpful but isn't necessary.
Var:t+1 ~ val(Val) <- observation(Var) ~= Val.
observation(Var):t+1 ~ val(Val) <- Var:t+1 ~= Val.
attributes(Obj, X_pos, Y_pos, Type, Nothing):t <- x_pos(Obj):t ~= X_pos,
                                                  y_pos(Obj):t ~= Y_pos,
                                                  type(Obj):t ~= Type,
                                                  nothing(Obj):t ~= Nothing.

% Actions
% This can be made more complicated if you want, but should suffice for now.
adm(action(A)):t <- member(A, [u,l,d,r]).

% Neighbours
% You probably won't need to use this kind of thing unless you're using deictic rules, but I've left it in just in case as it's what I've used for my example.
nb1(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y + 1, map(NbX, NbY, Nb):t.
nb2(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y + 1, map(NbX, NbY, Nb):t.
nb3(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y + 1, map(NbX, NbY, Nb):t.
nb4(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y    , map(NbX, NbY, Nb):t.
nb5(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb6(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y - 1, map(NbX, NbY, Nb):t.
nb7(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb8(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y    , map(NbX, NbY, Nb):t.

% Map
% Helper predicate to keep track of where objects and their neighbours move to at each time-step. The second line is deliberately grounded so that abstraction can be used.
map(X, Y, Obj):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y.
map(X, Y, no_object):t <- \+((map(X, Y, obj0):t)),
                          \+((map(X, Y, obj1):t)),
                          \+((map(X, Y, obj2):t)),
                          \+((map(X, Y, obj3):t)),
                          \+((map(X, Y, obj4):t)),
                          \+((map(X, Y, obj5):t)),
                          \+((map(X, Y, obj6):t)),
                          \+((map(X, Y, obj7):t)),
                          \+((map(X, Y, obj8):t)),
                          \+((map(X, Y, obj9):t)),
                          \+((map(X, Y, obj10):t)),
                          \+((map(X, Y, obj11):t)),
                          \+((map(X, Y, obj12):t)),
                          \+((map(X, Y, obj13):t)),
                          \+((map(X, Y, obj14):t)),
                          \+((map(X, Y, obj15):t)),
                          \+((map(X, Y, obj16):t)),
                          \+((map(X, Y, obj17):t)).

% Nothing
% This might not be necessary anymore.
nothing(no_object):t+1 ~ val(Curr) <- nothing(no_object):t ~= Curr.

% Attribute rules
% These govern the dynamics of the MDP by updating the attributes of objects in the domain.
% new_x_pos(Obj, New):t <- type(Obj):t ~= agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1, nb4(Obj,Nb4):t, nothing(Nb4):t ~= yes.
% new_x_pos(Obj, New):t <- type(Obj):t ~= agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1, nb8(Obj,Nb8):t, nothing(Nb8):t ~= yes.
% x_pos(Obj):t+1 ~ val(New) <- new_x_pos(Obj, New):t.
% x_pos(Obj):t+1 ~ val(Curr) <- x_pos(Obj):t ~= Curr.
% new_y_pos(Obj, New):t <- type(Obj):t ~= agent, action(u), y_pos(Obj):t ~= Curr, New is Curr + 1, nb2(Obj,Nb2):t, nothing(Nb2):t ~= yes.
% new_y_pos(Obj, New):t <- type(Obj):t ~= agent, action(d), y_pos(Obj):t ~= Curr, New is Curr - 1, nb6(Obj,Nb6):t, nothing(Nb6):t ~= yes.
% y_pos(Obj):t+1 ~ val(New) <- new_y_pos(Obj, New):t.
% y_pos(Obj):t+1 ~ val(Curr) <- y_pos(Obj):t ~= Curr.
% type(Obj):t+1 ~ val(Curr) <- type(Obj):t ~= Curr.
% nothing(Obj):t+1 ~ val(Curr) <- nothing(Obj):t ~= Curr.

x1(X1,F1):t ~ val((5,2)) <- type(Obj):t ~= agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1, nb4(Obj,Nb4):t, nothing(Nb4):t ~= yes.
x1(X1,F1):t ~ val((0,0)) <- true.

x2(X2,F2):t ~ val((4,1)) <- type(Obj):t ~= agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1, nb8(Obj,Nb8):t, nothing(Nb8):t ~= yes.
x2(X2,F2):t ~ val((0,0)) <- true.

% x_pos(Obj):t+1 ~ val(New) <- new_x_pos(Obj, New):t.
% x_pos(Obj):t+1 ~ val(Curr) <- x_pos(Obj):t ~= Curr.

x_pos(Obj):t+1 ~ finite([P1:R, P2:L, P3:Curr]) <- x_pos(Obj):t ~= Curr, L is Curr - 1, R is Curr + 1,
                                                  x1(X1,F1):t, x2(X2,F2):t,
                                                  P1 is X1 / (X1 + F1 + X2 + F2),
                                                  P2 is X2 / (X1 + F1 + X2 + F2),
                                                  P3 is (F1 + F2) / (X1 + F1 + X2 + F2).



new_y_pos(Obj, New):t <- type(Obj):t ~= agent, action(u), y_pos(Obj):t ~= Curr, New is Curr + 1, nb2(Obj,Nb2):t, nothing(Nb2):t ~= yes.
new_y_pos(Obj, New):t <- type(Obj):t ~= agent, action(d), y_pos(Obj):t ~= Curr, New is Curr - 1, nb6(Obj,Nb6):t, nothing(Nb6):t ~= yes.
y_pos(Obj):t+1 ~ val(New) <- new_y_pos(Obj, New):t.
y_pos(Obj):t+1 ~ val(Curr) <- y_pos(Obj):t ~= Curr.
% type(Obj):t+1 ~ val(Curr) <- type(Obj):t ~= Curr.

type(Obj):t+1 ~ finite([X:wall,0.5:Curr]) <- type(Obj):t ~= Curr, X = 0.5.

nothing(Obj):t+1 ~ val(Curr) <- nothing(Obj):t ~= Curr.

% Reward rules
% I haven't finished working out how to represent my reward rules yet, so this is a simple temporary solution.
reward:t ~ val(10) <- type(obj17):t ~= wall.
reward:t ~ val(0) <- \+((type(obj17):t ~= wall)).

% reward:t ~ val(-10) <- map(1, 1, obj17):t.
% reward:t ~ val(10) <- map(3, 1, obj17):t.
% reward:t ~ val(-1) <- \+((map(1, 1, obj17):t)), \+((map(3, 1, obj17):t)).

% Run command
% Plans for one step using the initialization given as an observation. Most of the parameters can be changed I think.
run :- executedplan_start,executedplan_step(BA,False,[observation(x_pos(obj0))~=0,
                                                    observation(y_pos(obj0))~=4,
                                                    observation(type(obj0))~=wall,
                                                    observation(nothing(obj0))~=no,
                                                    observation(x_pos(obj1))~=1,
                                                    observation(y_pos(obj1))~=4,
                                                    observation(type(obj1))~=wall,
                                                    observation(nothing(obj1))~=no,
                                                    observation(x_pos(obj2))~=2,
                                                    observation(y_pos(obj2))~=4,
                                                    observation(type(obj2))~=wall,
                                                    observation(nothing(obj2))~=no,
                                                    observation(x_pos(obj3))~=3,
                                                    observation(y_pos(obj3))~=4,
                                                    observation(type(obj3))~=wall,
                                                    observation(nothing(obj3))~=no,
                                                    observation(x_pos(obj4))~=4,
                                                    observation(y_pos(obj4))~=4,
                                                    observation(type(obj4))~=wall,
                                                    observation(nothing(obj4))~=no,
                                                    observation(x_pos(obj5))~=0,
                                                    observation(y_pos(obj5))~=3,
                                                    observation(type(obj5))~=wall,
                                                    observation(nothing(obj5))~=no,
                                                    observation(x_pos(obj6))~=4,
                                                    observation(y_pos(obj6))~=3,
                                                    observation(type(obj6))~=wall,
                                                    observation(nothing(obj6))~=no,
                                                    observation(x_pos(obj7))~=0,
                                                    observation(y_pos(obj7))~=2,
                                                    observation(type(obj7))~=wall,
                                                    observation(nothing(obj7))~=no,
                                                    observation(x_pos(obj8))~=3,
                                                    observation(y_pos(obj8))~=2,
                                                    observation(type(obj8))~=wall,
                                                    observation(nothing(obj8))~=no,
                                                    observation(x_pos(obj9))~=4,
                                                    observation(y_pos(obj9))~=2,
                                                    observation(type(obj9))~=wall,
                                                    observation(nothing(obj9))~=no,
                                                    observation(x_pos(obj10))~=0,
                                                    observation(y_pos(obj10))~=1,
                                                    observation(type(obj10))~=wall,
                                                    observation(nothing(obj10))~=no,
                                                    observation(x_pos(obj11))~=4,
                                                    observation(y_pos(obj11))~=1,
                                                    observation(type(obj11))~=wall,
                                                    observation(nothing(obj11))~=no,
                                                    observation(x_pos(obj12))~=0,
                                                    observation(y_pos(obj12))~=0,
                                                    observation(type(obj12))~=wall,
                                                    observation(nothing(obj12))~=no,
                                                    observation(x_pos(obj13))~=1,
                                                    observation(y_pos(obj13))~=0,
                                                    observation(type(obj13))~=wall,
                                                    observation(nothing(obj13))~=no,
                                                    observation(x_pos(obj14))~=2,
                                                    observation(y_pos(obj14))~=0,
                                                    observation(type(obj14))~=wall,
                                                    observation(nothing(obj14))~=no,
                                                    observation(x_pos(obj15))~=3,
                                                    observation(y_pos(obj15))~=0,
                                                    observation(type(obj15))~=wall,
                                                    observation(nothing(obj15))~=no,
                                                    observation(x_pos(obj16))~=4,
                                                    observation(y_pos(obj16))~=0,
                                                    observation(type(obj16))~=wall,
                                                    observation(nothing(obj16))~=no,
                                                    observation(x_pos(obj17))~=1,
                                                    observation(y_pos(obj17))~=3,
                                                    observation(type(obj17))~=agent,
                                                    observation(nothing(obj17))~=no,
                                                    observation(nothing(no_object))~=yes],
                                                    100,5,TotalR,T,5,STOP).
