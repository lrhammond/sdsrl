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
adm(action(A)):t <- member(A, [u,l,d,r]).

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
schema_x_pos(Obj, New):t <- nb4(Obj,Nb4):t, nothing(Nb4):t ~= Yes, colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1.
schema_x_pos(Obj, New):t <- nb8(Obj,Nb8):t, nothing(Nb8):t ~= Yes, colour(Obj):t ~= Agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.
no_schema_x_pos(Obj, New):t <- \+schema_x_pos(Obj, _):t, x_pos(Obj):t ~= New.
x_pos(Obj):t+1 ~ val(New) <- schema_x_pos(Obj, New):t.
x_pos(Obj):t+1 ~ val(New) <- no_schema_x_pos(Obj, New):t.
schema_y_pos(Obj, New):t <- nb6(Obj,Nb6):t, nothing(Nb6):t ~= Yes, colour(Obj):t ~= Agent, action(d), y_pos(Obj):t ~= Curr, New is Curr + 1.
schema_y_pos(Obj, New):t <- nb2(Obj,Nb2):t, nothing(Nb2):t ~= Yes, colour(Obj):t ~= Agent, action(u), y_pos(Obj):t ~= Curr, New is Curr - 1.
no_schema_y_pos(Obj, New):t <- \+schema_y_pos(Obj, _):t, y_pos(Obj):t ~= New.
y_pos(Obj):t+1 ~ val(New) <- schema_y_pos(Obj, New):t.
y_pos(Obj):t+1 ~ val(New) <- no_schema_y_pos(Obj, New):t.
x_size(Obj):t+1 ~ val(New) <- x_size(Obj):t ~= New.
y_size(Obj):t+1 ~ val(New) <- y_size(Obj):t ~= New.
colour(Obj):t+1 ~ val(New) <- colour(Obj):t ~= New.
shape(Obj):t+1 ~ val(New) <- shape(Obj):t ~= New.
nothing(Obj):t+1 ~ val(New) <- nothing(Obj):t ~= New.

% Reward Schemas
r(Xobj, Yobj, Type, R):t <- R = -100, action(d), Xobj=1, Yobj=2, Type=agent.
r(Xobj, Yobj, Type, R):t <- R = -100, action(l), Xobj=2, Yobj=3, Type=agent.
r(Xobj, Yobj, Type, R):t <- R = 100, action(r), Xobj=2, Yobj=3, Type=agent.
schema_reward(Obj):t ~ val(R) <- attributes(Obj, Xobj, Yobj, _, _, Type, _, _):t, r(Xobj, Yobj, Type, R):t.
schema_reward(Obj):t ~ val(-1) <- attributes(Obj, Xobj, Yobj, _, _, Type, _, _):t, \+r(Xobj, Yobj, Type, _):t.
reward:t ~ val(R) <- schema_reward(Obj):t ~= R.
