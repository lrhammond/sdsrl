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
adm(action(A)):t <- member(A, [u,l,d,r,none]).

% Neighbours
nb1(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj - 1, Tmp2 is Yobj + 1, Xnb = Tmp1, Ynb = Tmp2.
nb2(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t,                   Tmp2 is Yobj + 1, Xnb = Xobj, Ynb = Tmp2.
nb3(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj + 1, Tmp2 is Yobj + 1, Xnb = Tmp1, Ynb = Tmp2.
nb4(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj + 1,                   Xnb = Tmp1, Ynb = Yobj.
nb5(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj + 1, Tmp2 is Yobj - 1, Xnb = Tmp1, Ynb = Tmp2.
nb6(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t,                   Tmp2 is Yobj - 1, Xnb = Xobj, Ynb = Tmp2.
nb7(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj - 1, Tmp2 is Yobj - 1, Xnb = Tmp1, Ynb = Tmp2.
nb8(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Tmp1 is Xobj - 1,                   Xnb = Tmp1, Ynb = Yobj.

% Nothing
nothing(Obj):t ~ val(Nothing) <- member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]), Nothing = no.
nothing(Obj):t ~ val(Nothing) <- \+member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]), Nothing = yes.

% Attribute Schemas
x_pos(Obj):t+1 ~ val(New) <- x_pos(Obj):t ~= New.
y_pos(Obj):t+1 ~ val(New) <- y_pos(Obj):t ~= New.
x_size(Obj):t+1 ~ val(New) <- x_size(Obj):t ~= New.
y_size(Obj):t+1 ~ val(New) <- y_size(Obj):t ~= New.
colour(Obj):t+1 ~ val(New) <- colour(Obj):t ~= New.
shape(Obj):t+1 ~ val(New) <- shape(Obj):t ~= New.
nothing(Obj):t+1 ~ val(New) <- nothing(Obj):t ~= New.

% Reward Schemas
r(Xobj, Yobj, Type, R):t <- R = -10, action(d), Xobj=1, Yobj=2, Type=agent.
r(Xobj, Yobj, Type, R):t <- R = -10, action(l), Xobj=2, Yobj=3, Type=agent.
r(Xobj, Yobj, Type, R):t <- R = 10, action(r), Xobj=2, Yobj=3, Type=agent.
schema_reward(Obj):t ~ val(R) <- attributes(Obj, Xobj, Yobj, _, _, Type, _, _):t, r(Xobj, Yobj, Type, R):t.
schema_reward(Obj):t ~ val(-1) <- attributes(Obj, Xobj, Yobj, _, _, Type, _, _):t, \+r(Xobj, Yobj, Type, _):t.
reward:t ~ val(R) <- schema_reward(Obj):t ~= R.

c :- executedplan_start,
	executedplan_step(BA,true,[observation(x_pos(obj0)) ~= 0, observation(y_pos(obj0)) ~= 0, observation(x_size(obj0)) ~= small, observation(y_size(obj0)) ~= small, observation(colour(obj0)) ~= wall, observation(shape(obj0)) ~= square, observation(nothing(obj0)) ~= no, observation(x_pos(obj1)) ~= 1, observation(y_pos(obj1)) ~= 0, observation(x_size(obj1)) ~= small, observation(y_size(obj1)) ~= small, observation(colour(obj1)) ~= wall, observation(shape(obj1)) ~= square, observation(nothing(obj1)) ~= no, observation(x_pos(obj2)) ~= 2, observation(y_pos(obj2)) ~= 0, observation(x_size(obj2)) ~= small, observation(y_size(obj2)) ~= small, observation(colour(obj2)) ~= wall, observation(shape(obj2)) ~= square, observation(nothing(obj2)) ~= no, observation(x_pos(obj3)) ~= 3, observation(y_pos(obj3)) ~= 0, observation(x_size(obj3)) ~= small, observation(y_size(obj3)) ~= small, observation(colour(obj3)) ~= wall, observation(shape(obj3)) ~= square, observation(nothing(obj3)) ~= no, observation(x_pos(obj4)) ~= 4, observation(y_pos(obj4)) ~= 0, observation(x_size(obj4)) ~= small, observation(y_size(obj4)) ~= small, observation(colour(obj4)) ~= wall, observation(shape(obj4)) ~= square, observation(nothing(obj4)) ~= no, observation(x_pos(obj5)) ~= 0, observation(y_pos(obj5)) ~= 1, observation(x_size(obj5)) ~= small, observation(y_size(obj5)) ~= small, observation(colour(obj5)) ~= wall, observation(shape(obj5)) ~= square, observation(nothing(obj5)) ~= no, observation(x_pos(obj6)) ~= 4, observation(y_pos(obj6)) ~= 1, observation(x_size(obj6)) ~= small, observation(y_size(obj6)) ~= small, observation(colour(obj6)) ~= wall, observation(shape(obj6)) ~= square, observation(nothing(obj6)) ~= no, observation(x_pos(obj7)) ~= 0, observation(y_pos(obj7)) ~= 2, observation(x_size(obj7)) ~= small, observation(y_size(obj7)) ~= small, observation(colour(obj7)) ~= wall, observation(shape(obj7)) ~= square, observation(nothing(obj7)) ~= no, observation(x_pos(obj8)) ~= 3, observation(y_pos(obj8)) ~= 2, observation(x_size(obj8)) ~= small, observation(y_size(obj8)) ~= small, observation(colour(obj8)) ~= wall, observation(shape(obj8)) ~= square, observation(nothing(obj8)) ~= no, observation(x_pos(obj9)) ~= 4, observation(y_pos(obj9)) ~= 2, observation(x_size(obj9)) ~= small, observation(y_size(obj9)) ~= small, observation(colour(obj9)) ~= wall, observation(shape(obj9)) ~= square, observation(nothing(obj9)) ~= no, observation(x_pos(obj10)) ~= 0, observation(y_pos(obj10)) ~= 3, observation(x_size(obj10)) ~= small, observation(y_size(obj10)) ~= small, observation(colour(obj10)) ~= wall, observation(shape(obj10)) ~= square, observation(nothing(obj10)) ~= no, observation(x_pos(obj11)) ~= 4, observation(y_pos(obj11)) ~= 3, observation(x_size(obj11)) ~= small, observation(y_size(obj11)) ~= small, observation(colour(obj11)) ~= wall, observation(shape(obj11)) ~= square, observation(nothing(obj11)) ~= no, observation(x_pos(obj12)) ~= 0, observation(y_pos(obj12)) ~= 4, observation(x_size(obj12)) ~= small, observation(y_size(obj12)) ~= small, observation(colour(obj12)) ~= wall, observation(shape(obj12)) ~= square, observation(nothing(obj12)) ~= no, observation(x_pos(obj13)) ~= 1, observation(y_pos(obj13)) ~= 4, observation(x_size(obj13)) ~= small, observation(y_size(obj13)) ~= small, observation(colour(obj13)) ~= wall, observation(shape(obj13)) ~= square, observation(nothing(obj13)) ~= no, observation(x_pos(obj14)) ~= 2, observation(y_pos(obj14)) ~= 4, observation(x_size(obj14)) ~= small, observation(y_size(obj14)) ~= small, observation(colour(obj14)) ~= wall, observation(shape(obj14)) ~= square, observation(nothing(obj14)) ~= no, observation(x_pos(obj15)) ~= 3, observation(y_pos(obj15)) ~= 4, observation(x_size(obj15)) ~= small, observation(y_size(obj15)) ~= small, observation(colour(obj15)) ~= wall, observation(shape(obj15)) ~= square, observation(nothing(obj15)) ~= no, observation(x_pos(obj16)) ~= 4, observation(y_pos(obj16)) ~= 4, observation(x_size(obj16)) ~= small, observation(y_size(obj16)) ~= small, observation(colour(obj16)) ~= wall, observation(shape(obj16)) ~= square, observation(nothing(obj16)) ~= no, observation(x_pos(obj17)) ~= 1, observation(y_pos(obj17)) ~= 3, observation(x_size(obj17)) ~= small, observation(y_size(obj17)) ~= small, observation(colour(obj17)) ~= hole, observation(shape(obj17)) ~= square, observation(nothing(obj17)) ~= no, observation(x_pos(obj18)) ~= 3, observation(y_pos(obj18)) ~= 3, observation(x_size(obj18)) ~= small, observation(y_size(obj18)) ~= small, observation(colour(obj18)) ~= goal, observation(shape(obj18)) ~= square, observation(nothing(obj18)) ~= no, observation(x_pos(obj19)) ~= 1, observation(y_pos(obj19)) ~= 2, observation(x_size(obj19)) ~= small, observation(y_size(obj19)) ~= small, observation(colour(obj19)) ~= agent, observation(shape(obj19)) ~= square, observation(nothing(obj19)) ~= no],300,15,TotalR,T,15,STOP).
