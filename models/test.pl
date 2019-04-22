% Prolog model for test

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
% maxV(D,100):t <- true.
% getparam(params) :- bb_put(user:spant,0),
%                     setparam(
%                         % Enable abstraction
%                         false,
%                         % Ratio of the samples reserved for the first action
%                         5,
%                         % Use correct formula (leave true)
%                         true,
%                         % Strategy to store V function
%                         max,
%                         % Execute action
%                         best,
%                         % most,
%                         % Domain
%                         propfalse,
%                         % relfalse,
%                         % Discount
%                         0.95,
%                         % Probability to explore in the beginning (first sample)
%                         0.25,
%                         % Probability to explore in the end (last sample)
%                         0.10,
%                         % Number of previous samples to use to estimate Q (larger is better but slower)
%                         100,
%                         % Max horizon span
%                         200,
%                         % Lambda init
%                         0.9,
%                         % Lambda final
%                         0.9,
%                         % UCBV
%                         false,
%                         % Decay
%                         0.015,
%                         % Action selection
%                         softmax,
%                         % egreedy,
%                         % Pruning
%                         1,
%                         % WHeuInit
%                         -0.1,
%                         % WHeuFinal
%                         -0.1),
%                     !.
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
                        0.95,
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

% Functions
% builtin(x_pos(_)).
% builtin(y_pos(_)).
% builtin(x_size(_)).
% builtin(y_size(_)).
% builtin(colour(_)).
% builtin(shape(_)).
% builtin(nothing(_)).
Var:t+1 ~ val(Val) <- observation(Var) ~= Val.
observation(Var):t+1 ~ val(Val) <- Var:t+1 ~= Val.
attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t <- x_pos(Obj):t ~= X_pos,
                                                                           y_pos(Obj):t ~= Y_pos,
                                                                           x_size(Obj):t ~= X_size,
                                                                           y_size(Obj):t ~= Y_size,
                                                                           colour(Obj):t ~= Colour,
                                                                           shape(Obj):t ~= Shape,
                                                                           nothing(Obj):t ~= Nothing.

% Constants
constants <- Square = square, Goal = goal, No = no, Wall = wall, Agent = agent, Small = small, Hole = hole, Yes = yes.
constants.

% Actions
adm(action(A)):t <- member(A, [l,r]).

% Neighbours
% nb1(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj - 1, Tmp2 is Yobj + 1, Xnb = Tmp1, Ynb = Tmp2.
% nb2(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t,                   Tmp2 is Yobj + 1, Xnb = Xobj, Ynb = Tmp2.
% nb3(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj + 1, Tmp2 is Yobj + 1, Xnb = Tmp1, Ynb = Tmp2.
% nb4(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, Tmp1 is Xobj + 1,                   Xnb = Tmp1, Ynb = Yobj, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t.
% nb5(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj + 1, Tmp2 is Yobj - 1, Xnb = Tmp1, Ynb = Tmp2.
% nb6(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t,                   Tmp2 is Yobj - 1, Xnb = Xobj, Ynb = Tmp2.
% nb7(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj - 1, Tmp2 is Yobj - 1, Xnb = Tmp1, Ynb = Tmp2.
% nb8(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, Tmp1 is Xobj - 1,                   Xnb = Tmp1, Ynb = Yobj, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t.
% neb1(Obj):t ~ val(Nb) <- nb1(Obj,Nb):t.
% neb1(Obj):t ~ val(no_object) <- \+nb1(Obj,_):t.
% neb2(Obj):t ~ val(Nb) <- nb2(Obj,Nb):t.
% neb2(Obj):t ~ val(no_object) <- \+nb2(Obj,_):t.
% neb3(Obj):t ~ val(Nb) <- nb3(Obj,Nb):t.
% neb3(Obj):t ~ val(no_object) <- \+nb3(Obj,_):t.
% neb4(Obj):t ~ val(Nb) <- nb4(Obj,Nb):t.
% neb4(Obj):t ~ val(no_object) <- \+nb4(Obj,_):t.
% neb5(Obj):t ~ val(Nb) <- nb5(Obj,Nb):t.
% neb5(Obj):t ~ val(no_object) <- \+nb5(Obj,_):t.
% neb6(Obj):t ~ val(Nb) <- nb6(Obj,Nb):t.
% neb6(Obj):t ~ val(no_object) <- \+nb6(Obj,_):t.
% neb7(Obj):t ~ val(Nb) <- nb7(Obj,Nb):t.
% neb7(Obj):t ~ val(no_object) <- \+nb7(Obj,_):t.
% neb8(Obj):t ~ val(Nb) <- nb8(Obj,Nb):t.
% neb8(Obj):t ~ val(no_object) <- \+nb8(Obj,_):t.
nb1pred(X, Y, Nb1):t <- NbX is X - 1, NbY is Y - 1, x_pos(Nb1):t ~= NbX, y_pos(Nb1):t ~= NbY.
nb2pred(X, Y, Nb2):t <- NbX is X    , NbY is Y - 1, x_pos(Nb2):t ~= NbX, y_pos(Nb2):t ~= NbY.
nb3pred(X, Y, Nb3):t <- NbX is X + 1, NbY is Y - 1, x_pos(Nb3):t ~= NbX, y_pos(Nb3):t ~= NbY.


% nb4pred(X, Y, Nb4):t <- NbX is X + 1, NbY is Y - 1, x_pos(Nb4):t ~= NbX, y_pos(Nb4):t ~= NbY.
% nb4pred(X, Y, no_object):t <- NbX is X + 1, NbY is Y - 1, \+map(_, NbX, NbY):t.



map(Obj, X, Y):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y.
map_oot(X, Y):t ~ val(Obj) <- map(Obj, X, Y):t.
map_oot(X, Y):t ~ val(no_object) <- \+map(_, X, Y):t.


% nb4pred(X, Y, Nb4):t <- NbX is X + 1, NbY is Y    , x_pos(Nb4):t ~= NbX, y_pos(Nb4):t ~= NbY.


nb5pred(X, Y, Nb5):t <- NbX is X + 1, NbY is Y + 1, x_pos(Nb5):t ~= NbX, y_pos(Nb5):t ~= NbY.
nb6pred(X, Y, Nb6):t <- NbX is X    , NbY is Y + 1, x_pos(Nb6):t ~= NbX, y_pos(Nb6):t ~= NbY.
nb7pred(X, Y, Nb7):t <- NbX is X - 1, NbY is Y + 1, x_pos(Nb7):t ~= NbX, y_pos(Nb7):t ~= NbY.
nb8pred(X, Y, Nb8):t <- NbX is X - 1, NbY is Y    , x_pos(Nb8):t ~= NbX, y_pos(Nb8):t ~= NbY.



nb1(Obj):t ~ val(Nb1) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, isobj(Nb1):t, nb1pred(X, Y, Nb1):t.
nb1(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb1pred(X, Y, _):t)).
nb2(Obj):t ~ val(Nb2) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, isobj(Nb2):t, nb2pred(X, Y, Nb2):t.
nb2(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb2pred(X, Y, _):t)).
nb3(Obj):t ~ val(Nb3) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, isobj(Nb3):t, nb3pred(X, Y, Nb3):t.
nb3(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb3pred(X, Y, _):t)).


% nb4(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb4pred(X, Y, _):t)).


% nb4(Obj):t ~ val(Nb4) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y, map(Nb4, NbX, NbY):t.
% nb4(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y, \+map(_, NbX, NbY):t.
% nb4(Obj):t ~ val(Nb4) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb4pred(X, Y, Nb4):t.


% nb4(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((isobj(Nb4):t, nb4pred(X, Y, Nb4):t)).


nb5(Obj):t ~ val(Nb5) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, isobj(Nb5):t, nb5pred(X, Y, Nb5):t.
nb5(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb5pred(X, Y, _):t)).
nb6(Obj):t ~ val(Nb6) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, isobj(Nb6):t, nb6pred(X, Y, Nb6):t.
nb6(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb6pred(X, Y, _):t)).
nb7(Obj):t ~ val(Nb7) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, isobj(Nb7):t, nb7pred(X, Y, Nb7):t.
nb7(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb7pred(X, Y, _):t)).
nb8(Obj):t ~ val(Nb8) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, isobj(Nb8):t, nb8pred(X, Y, Nb8):t.
nb8(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb8pred(X, Y, _):t)).


occupied(X, Y):t <- map_oot(X, Y):t ~= K, isobj(K):t.
% occupied(X, Y):t <- map(_, X, Y):t.


% nb4(Obj, Nb4):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y - 1, x_pos(Nb4):t ~= NbX, y_pos(Nb4):t ~= NbY.
% no_nb4(Obj, Nb4):t <- \+nb4(Obj, _):t, Nb4 = no_object.
% neb4(Obj):t ~ val(Nb4) <- nb4(Obj, Nb4):t.
% neb4(Obj):t ~ val(Nb4) <- no_nb4(Obj, Nb4):t.
%
%
% nb4(Obj, Nb4):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y, x_pos(Nb4):t ~= NbX, y_pos(Nb4):t ~= NbY.
% nbb4(Obj, Nb4):t <- nb4(Obj, Nb4):t.
% nbb4(Obj, Nb4):t <- \+nb4(Obj, _):t, Nb4 = no_object.
% % no_nb4(Obj, Nb4):t <- \+nb4(Obj, _):t, Nb4 = no_object.
% % neb4(Obj):t ~ val(Nb4) <- nb4(Obj, Nb4):t.
% % neb4(Obj):t ~ val(Nb4) <- no_nb4(Obj, Nb4):t.
%
% nwwwb4(Obj):t <- nbb4(Obj, Nb4):t, isobj(Nb4):t.

% nb4(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x + 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y = Y_pos_x


nothing(no_object):t ~= val(Nothing) <- Nothing = yes.

% Nothing
nothing(Obj):t ~ val(Nothing) <- member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]), Nothing = no.
nothing(Obj):t ~ val(Nothing) <- \+((member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]))), Nothing = yes.

isobj(Obj):t <- member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]).


nb4pred(X, Y, Nb4):t <- NbX is X + 1, NbY is Y    , x_pos(Nb4):t ~= NbX, y_pos(Nb4):t ~= NbY.
% nb4(Obj):t ~ val(Nb4) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb4pred(X, Y, Nb4):t.

% nb4pred(X,Y,Nb):t <-
%     (  NbX is X + 1, NbY is Y    , x_pos(Z):t ~= NbX, y_pos(Z):t ~= NbY
%     -> Nb = Z
%     ;  Nb = no_object
%     ).


nb4(Obj,Nb4):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y, x_pos(Nb4):t ~= NbX, y_pos(Nb4):t ~= NbY.
nb4(Obj,no_object):t.
% nb4(Obj,no_object):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb4pred(X, Y, Nb4):t.



% Attribute Schemas
schema_x_pos(Obj, New):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y, findall(Nb, map(Nb, NbX, NbY):t, List, []), length(List, 1), colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1.




schema_x_pos(Obj, New):t <- nb8(Obj):t ~= Nb8, nothing(Nb8):t ~= Yes, colour(Obj):t ~= Agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.
no_schema_x_pos(Obj, Curr):t <- \+schema_x_pos(Obj, _):t, x_pos(Obj):t ~= Curr.
x_pos(Obj):t+1 ~ val(New) <- schema_x_pos(Obj, New):t.
x_pos(Obj):t+1 ~ val(New) <- no_schema_x_pos(Obj, New):t.
schema_y_pos(Obj, New):t <- nb6(Obj):t ~= Nb6, nothing(Nb6):t ~= Yes, colour(Obj):t ~= Agent, action(d), y_pos(Obj):t ~= Curr, New is Curr + 1.
schema_y_pos(Obj, New):t <- nb2(Obj):t ~= Nb2, nothing(Nb2):t ~= Yes, colour(Obj):t ~= Agent, action(u), y_pos(Obj):t ~= Curr, New is Curr - 1.
no_schema_y_pos(Obj, New):t <- \+schema_y_pos(Obj, _):t, y_pos(Obj):t ~= New.
y_pos(Obj):t+1 ~ val(New) <- schema_y_pos(Obj, New):t.
y_pos(Obj):t+1 ~ val(New) <- no_schema_y_pos(Obj, New):t.
x_size(Obj):t+1 ~ val(New) <- x_size(Obj):t ~= New.
y_size(Obj):t+1 ~ val(New) <- y_size(Obj):t ~= New.
colour(Obj):t+1 ~ val(New) <- colour(Obj):t ~= New.
shape(Obj):t+1 ~ val(New) <- shape(Obj):t ~= New.
nothing(Obj):t+1 ~ val(New) <- nothing(Obj):t ~= New.





% schema_x_pos(Obj, New):t <- nb4(Obj):t ~= Nb, nothing(Nb):t ~= Yes, colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1.
%
%
% % schema_x_pos(Obj, New):t <- x_pos(Obj):t ~= Curr, y_pos(Obj):t ~= Y, New is Curr + 1, \+((y_pos(Nb):t ~= Curr, x_pos(Nb):t ~= New)), colour(Obj):t ~= Agent, action(r).
% schema_x_pos(Obj, New):t <- nb8(Obj, Nb8):t, nothing(Nb8):t ~= Yes, colour(Obj):t ~= Agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.
%
% no_schema_x_pos(Obj, Curr):t <- \+schema_x_pos(Obj, _):t, x_pos(Obj):t ~= Curr.
%
% x_pos(Obj):t+1 ~ val(New) <- schema_x_pos(Obj, New):t.
% x_pos(Obj):t+1 ~ val(Curr) <- no_schema_x_pos(Obj, Curr):t.
%
%
%
% schema_y_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(d), y_pos(Obj):t ~= Curr, New is Curr + 1.
% schema_y_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(u), y_pos(Obj):t ~= Curr, New is Curr - 1.
%
% no_schema_y_pos(Obj, Curr):t <- \+schema_y_pos(Obj, _):t, y_pos(Obj):t ~= Curr.
%
% y_pos(Obj):t+1 ~ val(New) <- schema_y_pos(Obj, New):t.
% y_pos(Obj):t+1 ~ val(New) <- no_schema_y_pos(Obj, New):t.






% Reward Schemas
% r(Xobj, Yobj, Type, R):t <- R = 1000, Xobj < 1, Type=agent.
r(Xobj, Yobj, Type, R):t <- R = 10000, Xobj > 3, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = 1000, Yobj < 1, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = 1000, Yobj > 3, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = 10000, Xobj=3, Yobj=2, Type=agent.


r(Xobj, Yobj, Type, R):t <- R = -100, Xobj=1, Yobj=1, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = -10, action(l), Xobj=2, Yobj=3, Type=agent.
r(Xobj, Yobj, Type, R):t <- R = 1000, Xobj=3, Yobj=1, Type=agent.
schema_reward(Obj):t ~ val(R) <- attributes(Obj, Xobj, Yobj, _, _, Type, _, _):t, r(Xobj, Yobj, Type, R):t.
schema_reward(Obj):t ~ val(-1) <- attributes(Obj, Xobj, Yobj, _, _, Type, _, _):t, \+r(Xobj, Yobj, Type, _):t.
reward:t ~ val(R) <- schema_reward(Obj):t ~= R.

%
% r(R):t <- R = 10, x_pos(obj19) ~= 1, y_pos(obj19) ~= 3.
% r(R):t <- R = -10, x_pos(obj19) ~= 3, y_pos(obj19) ~= 3.
% schema_reward:t ~ val(R) <- r(R):t.
% schema_reward:t ~ val(-1) <- \+r(_):t.
% reward:t ~ val(R) <- schema_reward:t ~= R.

c :- executedplan_start,
	executedplan_step(BA,false,[observation(x_pos(obj0)) ~= 0, observation(y_pos(obj0)) ~= 0, observation(x_size(obj0)) ~= small, observation(y_size(obj0)) ~= small, observation(colour(obj0)) ~= wall, observation(shape(obj0)) ~= square, observation(nothing(obj0)) ~= no, observation(x_pos(obj1)) ~= 1, observation(y_pos(obj1)) ~= 0, observation(x_size(obj1)) ~= small, observation(y_size(obj1)) ~= small, observation(colour(obj1)) ~= wall, observation(shape(obj1)) ~= square, observation(nothing(obj1)) ~= no, observation(x_pos(obj2)) ~= 2, observation(y_pos(obj2)) ~= 0, observation(x_size(obj2)) ~= small, observation(y_size(obj2)) ~= small, observation(colour(obj2)) ~= wall, observation(shape(obj2)) ~= square, observation(nothing(obj2)) ~= no, observation(x_pos(obj3)) ~= 3, observation(y_pos(obj3)) ~= 0, observation(x_size(obj3)) ~= small, observation(y_size(obj3)) ~= small, observation(colour(obj3)) ~= wall, observation(shape(obj3)) ~= square, observation(nothing(obj3)) ~= no, observation(x_pos(obj4)) ~= 4, observation(y_pos(obj4)) ~= 0, observation(x_size(obj4)) ~= small, observation(y_size(obj4)) ~= small, observation(colour(obj4)) ~= wall, observation(shape(obj4)) ~= square, observation(nothing(obj4)) ~= no, observation(x_pos(obj5)) ~= 0, observation(y_pos(obj5)) ~= 1, observation(x_size(obj5)) ~= small, observation(y_size(obj5)) ~= small, observation(colour(obj5)) ~= wall, observation(shape(obj5)) ~= square, observation(nothing(obj5)) ~= no, observation(x_pos(obj6)) ~= 4, observation(y_pos(obj6)) ~= 1, observation(x_size(obj6)) ~= small, observation(y_size(obj6)) ~= small, observation(colour(obj6)) ~= wall, observation(shape(obj6)) ~= square, observation(nothing(obj6)) ~= no, observation(x_pos(obj7)) ~= 0, observation(y_pos(obj7)) ~= 2, observation(x_size(obj7)) ~= small, observation(y_size(obj7)) ~= small, observation(colour(obj7)) ~= wall, observation(shape(obj7)) ~= square, observation(nothing(obj7)) ~= no, observation(x_pos(obj8)) ~= 3, observation(y_pos(obj8)) ~= 2, observation(x_size(obj8)) ~= small, observation(y_size(obj8)) ~= small, observation(colour(obj8)) ~= wall, observation(shape(obj8)) ~= square, observation(nothing(obj8)) ~= no, observation(x_pos(obj9)) ~= 4, observation(y_pos(obj9)) ~= 2, observation(x_size(obj9)) ~= small, observation(y_size(obj9)) ~= small, observation(colour(obj9)) ~= wall, observation(shape(obj9)) ~= square, observation(nothing(obj9)) ~= no, observation(x_pos(obj10)) ~= 0, observation(y_pos(obj10)) ~= 3, observation(x_size(obj10)) ~= small, observation(y_size(obj10)) ~= small, observation(colour(obj10)) ~= wall, observation(shape(obj10)) ~= square, observation(nothing(obj10)) ~= no, observation(x_pos(obj11)) ~= 4, observation(y_pos(obj11)) ~= 3, observation(x_size(obj11)) ~= small, observation(y_size(obj11)) ~= small, observation(colour(obj11)) ~= wall, observation(shape(obj11)) ~= square, observation(nothing(obj11)) ~= no, observation(x_pos(obj12)) ~= 0, observation(y_pos(obj12)) ~= 4, observation(x_size(obj12)) ~= small, observation(y_size(obj12)) ~= small, observation(colour(obj12)) ~= wall, observation(shape(obj12)) ~= square, observation(nothing(obj12)) ~= no, observation(x_pos(obj13)) ~= 1, observation(y_pos(obj13)) ~= 4, observation(x_size(obj13)) ~= small, observation(y_size(obj13)) ~= small, observation(colour(obj13)) ~= wall, observation(shape(obj13)) ~= square, observation(nothing(obj13)) ~= no, observation(x_pos(obj14)) ~= 2, observation(y_pos(obj14)) ~= 4, observation(x_size(obj14)) ~= small, observation(y_size(obj14)) ~= small, observation(colour(obj14)) ~= wall, observation(shape(obj14)) ~= square, observation(nothing(obj14)) ~= no, observation(x_pos(obj15)) ~= 3, observation(y_pos(obj15)) ~= 4, observation(x_size(obj15)) ~= small, observation(y_size(obj15)) ~= small, observation(colour(obj15)) ~= wall, observation(shape(obj15)) ~= square, observation(nothing(obj15)) ~= no, observation(x_pos(obj16)) ~= 4, observation(y_pos(obj16)) ~= 4, observation(x_size(obj16)) ~= small, observation(y_size(obj16)) ~= small, observation(colour(obj16)) ~= wall, observation(shape(obj16)) ~= square, observation(nothing(obj16)) ~= no, observation(x_pos(obj17)) ~= 1, observation(y_pos(obj17)) ~= 3, observation(x_size(obj17)) ~= small, observation(y_size(obj17)) ~= small, observation(colour(obj17)) ~= hole, observation(shape(obj17)) ~= square, observation(nothing(obj17)) ~= no, observation(x_pos(obj18)) ~= 3, observation(y_pos(obj18)) ~= 3, observation(x_size(obj18)) ~= small, observation(y_size(obj18)) ~= small, observation(colour(obj18)) ~= goal, observation(shape(obj18)) ~= square, observation(nothing(obj18)) ~= no, observation(x_pos(obj19)) ~= 2, observation(y_pos(obj19)) ~= 1, observation(x_size(obj19)) ~= small, observation(y_size(obj19)) ~= small, observation(colour(obj19)) ~= agent, observation(shape(obj19)) ~= square, observation(nothing(obj19)) ~= no],100,6,TotalR,T,6,STOP).
