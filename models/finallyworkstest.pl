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
constants <- Square = square, Goal = goal, No = no, Wall = wall, Agent = agent, Small = small, Hole = hole, Yes = yes, Ja = ja.
% constants:t+1 <- constants:t.

% Actions
adm(action(A)):t <- member(A, [l,r]).

% Neighbours
nb1pred(X, Y, Nb1):t <- NbX is X - 1, NbY is Y - 1, x_pos(Nb1):t ~= NbX, y_pos(Nb1):t ~= NbY.
nb2pred(X, Y, Nb2):t <- NbX is X    , NbY is Y - 1, x_pos(Nb2):t ~= NbX, y_pos(Nb2):t ~= NbY.
nb3pred(X, Y, Nb3):t <- NbX is X + 1, NbY is Y - 1, x_pos(Nb3):t ~= NbX, y_pos(Nb3):t ~= NbY.
nb4pred(X, Y, Nb4):t <- NbX is X + 1, NbY is Y    , x_pos(Nb4):t ~= NbX, y_pos(Nb4):t ~= NbY.
nb5pred(X, Y, Nb5):t <- NbX is X + 1, NbY is Y + 1, x_pos(Nb5):t ~= NbX, y_pos(Nb5):t ~= NbY.
nb6pred(X, Y, Nb6):t <- NbX is X    , NbY is Y + 1, x_pos(Nb6):t ~= NbX, y_pos(Nb6):t ~= NbY.
nb7pred(X, Y, Nb7):t <- NbX is X - 1, NbY is Y + 1, x_pos(Nb7):t ~= NbX, y_pos(Nb7):t ~= NbY.
nb8pred(X, Y, Nb8):t <- NbX is X - 1, NbY is Y    , x_pos(Nb8):t ~= NbX, y_pos(Nb8):t ~= NbY.
nb1(Obj):t ~ val(Nb1) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb1pred(X, Y, Nb1):t.
nb1(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb1pred(X, Y, _):t)).
nb2(Obj):t ~ val(Nb2) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb2pred(X, Y, Nb2):t.
nb2(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb2pred(X, Y, _):t)).
nb3(Obj):t ~ val(Nb3) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb3pred(X, Y, Nb3):t.
nb3(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb3pred(X, Y, _):t)).
nb4(Obj):t ~ val(Nb4) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb4pred(X, Y, Nb4):t.
nb4(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb4pred(X, Y, _):t)).
nb5(Obj):t ~ val(Nb5) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb5pred(X, Y, Nb5):t.
nb5(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb5pred(X, Y, _):t)).
nb6(Obj):t ~ val(Nb6) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb6pred(X, Y, Nb6):t.
nb6(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb6pred(X, Y, _):t)).
nb7(Obj):t ~ val(Nb7) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb7pred(X, Y, Nb7):t.
nb7(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb7pred(X, Y, _):t)).
nb8(Obj):t ~ val(Nb8) <-       x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, nb8pred(X, Y, Nb8):t.
nb8(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nb8pred(X, Y, _):t)).
nb1(Obj):t+1 ~ val(Nb1) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nb1pred(NX, NY, Nb1):t.
nb1(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nb1pred(NX, NY, _):t)).
nb2(Obj):t+1 ~ val(Nb2) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nb2pred(NX, NY, Nb2):t.
nb2(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nb2pred(NX, NY, _):t)).
nb3(Obj):t+1 ~ val(Nb3) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nb3pred(NX, NY, Nb3):t.
nb3(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nb3pred(NX, NY, _):t)).
nb4(Obj):t+1 ~ val(Nb4) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nb4pred(NX, NY, Nb4):t.
nb4(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nb4pred(NX, NY, _):t)).
nb5(Obj):t+1 ~ val(Nb5) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nb5pred(NX, NY, Nb5):t.
nb5(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nb5pred(NX, NY, _):t)).
nb6(Obj):t+1 ~ val(Nb6) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nb6pred(NX, NY, Nb6):t.
nb6(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nb6pred(NX, NY, _):t)).
nb7(Obj):t+1 ~ val(Nb7) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nb7pred(NX, NY, Nb7):t.
nb7(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nb7pred(NX, NY, _):t)).
nb8(Obj):t+1 ~ val(Nb8) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nb8pred(NX, NY, Nb8):t.
nb8(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nb8pred(NX, NY, _):t)).


nothing(no_object):t ~= val(Yes) <- Yes = yes.
nothing(no_object):t+1 ~= val(Yes) <- nothing(no_object):t ~= Yes.


% Nothing
% nothing(Obj):t ~ val(Nothing) <- member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]), Nothing = no.
% nothing(Obj):t ~ val(Nothing) <- \+((member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]))), Nothing = yes.

isobj(Obj):t <- member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]).


% Attribute Schemas
% schema_x_pos(Obj, New):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y, findall(Nb, map(Nb, NbX, NbY):t, List, []), length(List, 1), colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1.


schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(r), nb4(Obj):t ~= Nb4, Nb4 = no_object, x_pos(Obj):t ~= Curr, New is Curr + 1.
schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(l), nb8(Obj):t ~= Nb8, Nb8 = no_object, x_pos(Obj):t ~= Curr, New is Curr - 1.


no_schema_x_pos(Obj, Curr):t <- \+schema_x_pos(Obj, _):t, x_pos(Obj):t ~= Curr.
new_x_pos(Obj, New):t <- schema_x_pos(Obj, New):t.
new_x_pos(Obj, New):t <- no_schema_x_pos(Obj, New):t.
x_pos(Obj):t+1 ~ val(New) <- new_x_pos(Obj, New):t.


schema_y_pos(Obj, New):t <- nb6(Obj):t ~= Nb6, nothing(Nb6):t ~= Yes, colour(Obj):t ~= Agent, action(d), y_pos(Obj):t ~= Curr, New is Curr + 1.
schema_y_pos(Obj, New):t <- nb2(Obj):t ~= Nb2, nothing(Nb2):t ~= Yes, colour(Obj):t ~= Agent, action(u), y_pos(Obj):t ~= Curr, New is Curr - 1.
no_schema_y_pos(Obj, New):t <- \+schema_y_pos(Obj, _):t, y_pos(Obj):t ~= New.
new_y_pos(Obj, New):t <- schema_y_pos(Obj, New):t.
new_y_pos(Obj, New):t <- no_schema_y_pos(Obj, New):t.
y_pos(Obj):t+1 ~ val(New) <- new_y_pos(Obj, New):t.
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
r(Xobj, Type, R) <- R = 10000, Xobj > 3, Type=agent.
r(Xobj, Type, R) <- R = 0, Xobj = 0, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = 1000, Yobj < 1, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = 1000, Yobj > 3, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = 10000, Xobj=3, Yobj=2, Type=agent.


r(Xobj, Type, R) <- R = -100, Xobj=1, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = -10, action(l), Xobj=2, Yobj=3, Type=agent.
r(Xobj, Type, R) <- R = 1000, Xobj=3, Type=agent.


schema_reward(Obj):t ~ val(R) <- x_pos(Obj):t ~= X, colour(Obj):t ~= Type, r(X, Type, R).
schema_reward(Obj):t ~ val(-1) <- x_pos(Obj):t ~= X, colour(Obj):t ~= Type, \+r(X, Type, _).
reward:t ~ val(R) <- schema_reward(Obj):t ~= R.

%
% r(R):t <- R = 10, x_pos(obj19) ~= 1, y_pos(obj19) ~= 3.
% r(R):t <- R = -10, x_pos(obj19) ~= 3, y_pos(obj19) ~= 3.
% schema_reward:t ~ val(R) <- r(R):t.
% schema_reward:t ~ val(-1) <- \+r(_):t.
% reward:t ~ val(R) <- schema_reward:t ~= R.

c :- executedplan_start,
	executedplan_step(BA,false,[observation(x_pos(obj0)) ~= 0, observation(y_pos(obj0)) ~= 0, observation(x_size(obj0)) ~= small, observation(y_size(obj0)) ~= small, observation(colour(obj0)) ~= wall, observation(shape(obj0)) ~= square, observation(nothing(obj0)) ~= no, observation(x_pos(obj1)) ~= 1, observation(y_pos(obj1)) ~= 0, observation(x_size(obj1)) ~= small, observation(y_size(obj1)) ~= small, observation(colour(obj1)) ~= wall, observation(shape(obj1)) ~= square, observation(nothing(obj1)) ~= no, observation(x_pos(obj2)) ~= 2, observation(y_pos(obj2)) ~= 0, observation(x_size(obj2)) ~= small, observation(y_size(obj2)) ~= small, observation(colour(obj2)) ~= wall, observation(shape(obj2)) ~= square, observation(nothing(obj2)) ~= no, observation(x_pos(obj3)) ~= 3, observation(y_pos(obj3)) ~= 0, observation(x_size(obj3)) ~= small, observation(y_size(obj3)) ~= small, observation(colour(obj3)) ~= wall, observation(shape(obj3)) ~= square, observation(nothing(obj3)) ~= no, observation(x_pos(obj4)) ~= 4, observation(y_pos(obj4)) ~= 0, observation(x_size(obj4)) ~= small, observation(y_size(obj4)) ~= small, observation(colour(obj4)) ~= wall, observation(shape(obj4)) ~= square, observation(nothing(obj4)) ~= no, observation(x_pos(obj5)) ~= 0, observation(y_pos(obj5)) ~= 1, observation(x_size(obj5)) ~= small, observation(y_size(obj5)) ~= small, observation(colour(obj5)) ~= wall, observation(shape(obj5)) ~= square, observation(nothing(obj5)) ~= no, observation(x_pos(obj6)) ~= 4, observation(y_pos(obj6)) ~= 1, observation(x_size(obj6)) ~= small, observation(y_size(obj6)) ~= small, observation(colour(obj6)) ~= wall, observation(shape(obj6)) ~= square, observation(nothing(obj6)) ~= no, observation(x_pos(obj7)) ~= 0, observation(y_pos(obj7)) ~= 2, observation(x_size(obj7)) ~= small, observation(y_size(obj7)) ~= small, observation(colour(obj7)) ~= wall, observation(shape(obj7)) ~= square, observation(nothing(obj7)) ~= no, observation(x_pos(obj8)) ~= 3, observation(y_pos(obj8)) ~= 2, observation(x_size(obj8)) ~= small, observation(y_size(obj8)) ~= small, observation(colour(obj8)) ~= wall, observation(shape(obj8)) ~= square, observation(nothing(obj8)) ~= no, observation(x_pos(obj9)) ~= 4, observation(y_pos(obj9)) ~= 2, observation(x_size(obj9)) ~= small, observation(y_size(obj9)) ~= small, observation(colour(obj9)) ~= wall, observation(shape(obj9)) ~= square, observation(nothing(obj9)) ~= no, observation(x_pos(obj10)) ~= 0, observation(y_pos(obj10)) ~= 3, observation(x_size(obj10)) ~= small, observation(y_size(obj10)) ~= small, observation(colour(obj10)) ~= wall, observation(shape(obj10)) ~= square, observation(nothing(obj10)) ~= no, observation(x_pos(obj11)) ~= 4, observation(y_pos(obj11)) ~= 3, observation(x_size(obj11)) ~= small, observation(y_size(obj11)) ~= small, observation(colour(obj11)) ~= wall, observation(shape(obj11)) ~= square, observation(nothing(obj11)) ~= no, observation(x_pos(obj12)) ~= 0, observation(y_pos(obj12)) ~= 4, observation(x_size(obj12)) ~= small, observation(y_size(obj12)) ~= small, observation(colour(obj12)) ~= wall, observation(shape(obj12)) ~= square, observation(nothing(obj12)) ~= no, observation(x_pos(obj13)) ~= 1, observation(y_pos(obj13)) ~= 4, observation(x_size(obj13)) ~= small, observation(y_size(obj13)) ~= small, observation(colour(obj13)) ~= wall, observation(shape(obj13)) ~= square, observation(nothing(obj13)) ~= no, observation(x_pos(obj14)) ~= 2, observation(y_pos(obj14)) ~= 4, observation(x_size(obj14)) ~= small, observation(y_size(obj14)) ~= small, observation(colour(obj14)) ~= wall, observation(shape(obj14)) ~= square, observation(nothing(obj14)) ~= no, observation(x_pos(obj15)) ~= 3, observation(y_pos(obj15)) ~= 4, observation(x_size(obj15)) ~= small, observation(y_size(obj15)) ~= small, observation(colour(obj15)) ~= wall, observation(shape(obj15)) ~= square, observation(nothing(obj15)) ~= no, observation(x_pos(obj16)) ~= 4, observation(y_pos(obj16)) ~= 4, observation(x_size(obj16)) ~= small, observation(y_size(obj16)) ~= small, observation(colour(obj16)) ~= wall, observation(shape(obj16)) ~= square, observation(nothing(obj16)) ~= no, observation(x_pos(obj17)) ~= 1, observation(y_pos(obj17)) ~= 3, observation(x_size(obj17)) ~= small, observation(y_size(obj17)) ~= small, observation(colour(obj17)) ~= hole, observation(shape(obj17)) ~= square, observation(nothing(obj17)) ~= no, observation(x_pos(obj18)) ~= 3, observation(y_pos(obj18)) ~= 3, observation(x_size(obj18)) ~= small, observation(y_size(obj18)) ~= small, observation(colour(obj18)) ~= goal, observation(shape(obj18)) ~= square, observation(nothing(obj18)) ~= no, observation(x_pos(obj19)) ~= 1, observation(y_pos(obj19)) ~= 1, observation(x_size(obj19)) ~= small, observation(y_size(obj19)) ~= small, observation(colour(obj19)) ~= agent, observation(shape(obj19)) ~= square, observation(nothing(obj19)) ~= no],100,6,TotalR,T,6,STOP).
