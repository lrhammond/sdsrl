% Prolog model for test

% Libraries
:- use_module(library(planning)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(dcpf)).
:- use_module(library(distributionalclause)).
:- use_module(library(sst)).

% Options
% :- set_options(default).
% :- set_query_propagation(true).
% :- set_inference(backward(lazy)).
% :- set_current2nextcopy(false).
:- set_options(default),
   set_query_propagation(true),
   set_inference(backward(lazy)).
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
nb1(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj - 1, Ynb is Yobj + 1.
nb2(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj    , Ynb is Yobj + 1.
nb3(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj + 1, Ynb is Yobj + 1.
nb4(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj + 1, Ynb is Yobj    .
nb5(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj + 1, Ynb is Yobj - 1.
nb6(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj    , Ynb is Yobj - 1.
nb7(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj - 1, Ynb is Yobj - 1.
nb8(Obj,Nb):t <- attributes(Obj, Xobj, Yobj, _, _, _, _, _):t, attributes(Nb, Xnb, Ynb, _, _, _, _, _):t, Xnb is Xobj - 1, Ynb is Yobj    .

% Nothing
nothing(Obj):t ~ val(Nothing) <- member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]), Nothing = no.
nothing(Obj):t ~ val(Nothing) <- \+member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]), Nothing = yes.

% Attribute Schemas
schema_x_pos(Obj, New):t <- nb8(X,NB8):t, colour(NB8):t ~= Wall, nb2(X,NB2):t, colour(NB2):t ~= Wall, nb1(X,NB1):t, colour(NB1):t ~= Wall, colour(X):t ~= Agent, action(r), right = New, x_pos(Obj):t ~= Curr, right is Curr + 1.
schema_x_pos(Obj, New):t <- colour(X):t ~= Agent, action(l), left = New, x_pos(Obj):t ~= Curr, left is Curr - 1.
no_schema_x_pos(Obj, New):t <- \+schema_x_pos(Obj, _):t, x_pos(Obj):t ~= New.
x_pos(Obj):t+1 ~ val(New) <- schema_x_pos(Obj, New):t.
x_pos(Obj):t+1 ~ val(New) <- no_schema_x_pos(Obj, New):t.
schema_y_pos(Obj, New):t <- colour(X):t ~= Agent, action(d), below = New, y_pos(Obj):t ~= Curr, below is Curr - 1.
schema_y_pos(Obj, New):t <- nb4(X,NB4):t, colour(NB4):t ~= Goal, action(u), above = New, y_pos(Obj):t ~= Curr, above is Curr + 1.
no_schema_y_pos(Obj, New):t <- \+schema_y_pos(Obj, _):t, y_pos(Obj):t ~= New.
y_pos(Obj):t+1 ~ val(New) <- schema_y_pos(Obj, New):t.
y_pos(Obj):t+1 ~ val(New) <- no_schema_y_pos(Obj, New):t.
x_size(Obj):t+1 ~ val(New) <- x_size(Obj):t ~= New.
y_size(Obj):t+1 ~ val(New) <- y_size(Obj):t ~= New.
colour(Obj):t+1 ~ val(New) <- colour(Obj):t ~= New.
shape(Obj):t+1 ~ val(New) <- shape(Obj):t ~= New.
nothing(Obj):t+1 ~ val(New) <- nothing(Obj):t ~= New.

% Reward Schemas
reward:t+1 ~ val(Reward) <- (colour(X):t ~= Wall, action(r), 10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb6(X,NB6):t, colour(NB6):t ~= Wall, nb4(X,NB4):t, nothing(NB4):t ~= No, nb8(X,NB8):t, nothing(NB8):t ~= No, action(r), 10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb6(X,NB6):t, colour(NB6):t ~= Hole, -10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb5(X,NB5):t, colour(NB5):t ~= Agent, nb3(X,NB3):t, nothing(NB3):t ~= No, -10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb4(X,NB4):t, colour(NB4):t ~= Agent, action(d), -10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb7(X,NB7):t, nothing(NB7):t ~= Yes, nb2(X,NB2):t, nothing(NB2):t ~= Yes, nb1(X,NB1):t, nothing(NB1):t ~= Yes, action(d), -10 = Reward ; Reward = -1).
reward(Obj):t ~ val(R) <- attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t, schema_reward(R, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t.
reward(Obj):t ~ val(-1) <- attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t, \+schema_reward(R, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t.
reward:t ~ val(R) <- reward(Obj):t ~= R.
