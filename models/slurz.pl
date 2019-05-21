% Prolog model for slurz

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

% Functions
Var:t+1 ~ val(Val) <- observation(Var) ~= Val.
observation(Var):t+1 ~ val(Val) <- Var:t+1 ~= Val.
attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t <- x_pos(Obj):t ~= X_pos,
                                                                           y_pos(Obj):t ~= Y_pos,
                                                                           x_size(Obj):t ~= X_size,
                                                                           y_size(Obj):t ~= Y_size,
                                                                           colour(Obj):t ~= Colour,
                                                                           shape(Obj):t ~= Shape,
                                                                           nothing(Obj):t ~= Nothing.

% Actions
adm(action(A)):t <- member(A, [d,r]).

% Neighbours
nb1(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y + 1, map(NbX, NbY, Nb):t.
nb2(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y + 1, map(NbX, NbY, Nb):t.
nb3(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y + 1, map(NbX, NbY, Nb):t.
nb4(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y    , map(NbX, NbY, Nb):t.
nb5(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb6(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y - 1, map(NbX, NbY, Nb):t.
nb7(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb8(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y    , map(NbX, NbY, Nb):t.

% Map
map(X, Y, Obj):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y.
map(X, Y, no_object):t <- \+((map(X, Y, obj0):t)), \+((map(X, Y, obj1):t)), \+((map(X, Y, obj2):t)), \+((map(X, Y, obj3):t)), \+((map(X, Y, obj4):t)), \+((map(X, Y, obj5):t)), \+((map(X, Y, obj6):t)), \+((map(X, Y, obj7):t)), \+((map(X, Y, obj8):t)), \+((map(X, Y, obj9):t)), \+((map(X, Y, obj10):t)), \+((map(X, Y, obj11):t)), \+((map(X, Y, obj12):t)), \+((map(X, Y, obj13):t)), \+((map(X, Y, obj14):t)), \+((map(X, Y, obj15):t)), \+((map(X, Y, obj16):t)), \+((map(X, Y, obj17):t)), \+((map(X, Y, obj18):t)), \+((map(X, Y, obj19):t)).

% Nothing
nothing(no_object):t+1 ~ val(Curr) <- nothing(no_object):t ~= Curr.% Attribute Schemas
schema_x_pos(Obj, New):t <- nb4(Obj,Nb4):t, nothing(Nb4):t ~= yes, colour(Obj):t ~= agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1.
schema_x_pos(Obj, New):t <- nb5(Obj,Nb5):t, nothing(Nb5):t ~= yes, nb7(Obj,Nb7):t, colour(Nb7):t ~= wall, nb3(Obj,Nb3):t, colour(Nb3):t ~= wall, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1.
schema_x_pos(Obj, New):t <- nb8(Obj,Nb8):t, nothing(Nb8):t ~= yes, colour(Obj):t ~= agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.
x_pos(Obj):t+1 ~ val(New) <- schema_x_pos(Obj, New):t.
x_pos(Obj):t+1 ~ val(Curr) <- x_pos(Obj):t ~= Curr.
schema_y_pos(Obj, New):t <- nb5(Obj,Nb5):t, nothing(Nb5):t ~= no, nb8(Obj,Nb8):t, nothing(Nb8):t ~= yes, nb6(Obj,Nb6):t, nothing(Nb6):t ~= yes, action(d), y_pos(Obj):t ~= Curr, New is Curr - 1.
schema_y_pos(Obj, New):t <- nb6(Obj,Nb6):t, nothing(Nb6):t ~= yes, nb3(Obj,Nb3):t, colour(Nb3):t ~= wall, nb2(Obj,Nb2):t, colour(Nb2):t ~= wall, nb1(Obj,Nb1):t, colour(Nb1):t ~= wall, nb4(Obj,Nb4):t, nothing(Nb4):t ~= yes, action(d), y_pos(Obj):t ~= Curr, New is Curr - 1.
schema_y_pos(Obj, New):t <- nb5(Obj,Nb5):t, colour(Nb5):t ~= goal, action(u), y_pos(Obj):t ~= Curr, New is Curr + 1.
schema_y_pos(Obj, New):t <- nb3(Obj,Nb3):t, nothing(Nb3):t ~= yes, colour(Obj):t ~= agent, action(u), y_pos(Obj):t ~= Curr, New is Curr + 1.
y_pos(Obj):t+1 ~ val(New) <- schema_y_pos(Obj, New):t.
y_pos(Obj):t+1 ~ val(Curr) <- y_pos(Obj):t ~= Curr.
x_size(Obj):t+1 ~ val(Curr) <- x_size(Obj):t ~= Curr.
y_size(Obj):t+1 ~ val(Curr) <- y_size(Obj):t ~= Curr.
colour(Obj):t+1 ~ val(Curr) <- colour(Obj):t ~= Curr.
shape(Obj):t+1 ~ val(Curr) <- shape(Obj):t ~= Curr.
nothing(Obj):t+1 ~ val(Curr) <- nothing(Obj):t ~= Curr.

% Reward Schemas
% r(Xobj, Yobj, Type, R):t <- R = -10, action(d), Xobj=1, Yobj=2, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = -10, action(l), Xobj=2, Yobj=1, Type=agent.
% r(Xobj, Yobj, Type, R):t <- R = 10, action(r), Xobj=2, Yobj=1, Type=agent.
% schema_reward(Obj):t ~ val(R) <- attributes(Obj, Xobj, Yobj, _, _, Type, _, _):t, r(Xobj, Yobj, Type, R):t.
% schema_reward(Obj):t ~ val(-1) <- attributes(Obj, Xobj, Yobj, _, _, Type, _, _):t, \+r(Xobj, Yobj, Type, _):t.
% reward:t ~ val(R) <- schema_reward(Obj):t ~= R.


reward:t ~ val(-10) <-  map(1, 1, obj19):t.
reward:t ~ val(10) <- map(3, 1, obj19):t.
reward:t ~ val(-1) <- \+(map(1, 1, obj19):t), \+(map(3, 1, obj19):t).
% reward:t ~ val(100) <- map(3, 1, obj19):t.
% reward:t ~ val(100) <- map(2, 2, obj19):t.
% reward:t ~ val(-1) <- map(X,_,obj19):t, X \= 1, X \= 2, X \= 3.
% reward:t ~ val(0) <- map(_, Y, obj19):t, Y \= 1.

% Run example
c :- executedplan_start, executedplan_step(BA,true,
[observation(x_pos(obj0))~=0,
observation(y_pos(obj0))~=0,
observation(colour(obj0))~=wall,
observation(nothing(obj0))~=no,
observation(x_pos(obj1))~=1,
observation(y_pos(obj1))~=0,
observation(colour(obj1))~=wall,
observation(nothing(obj1))~=no,
observation(x_pos(obj2))~=2,
observation(y_pos(obj2))~=0,
observation(colour(obj2))~=wall,
observation(nothing(obj2))~=no,
observation(x_pos(obj3))~=3,
observation(y_pos(obj3))~=0,
observation(colour(obj3))~=wall,
observation(nothing(obj3))~=no,
observation(x_pos(obj4))~=4,
observation(y_pos(obj4))~=0,
observation(colour(obj4))~=wall,
observation(nothing(obj4))~=no,
observation(x_pos(obj5))~=0,
observation(y_pos(obj5))~=1,
observation(colour(obj5))~=wall,
observation(nothing(obj5))~=no,
observation(x_pos(obj6))~=4,
observation(y_pos(obj6))~=1,
observation(colour(obj6))~=wall,
observation(nothing(obj6))~=no,
observation(x_pos(obj7))~=0,
observation(y_pos(obj7))~=2,
observation(colour(obj7))~=wall,
observation(nothing(obj7))~=no,
observation(x_pos(obj8))~=3,
observation(y_pos(obj8))~=2,
observation(colour(obj8))~=wall,
observation(nothing(obj8))~=no,
observation(x_pos(obj9))~=4,
observation(y_pos(obj9))~=2,
observation(colour(obj9))~=wall,
observation(nothing(obj9))~=no,
observation(x_pos(obj10))~=0,
observation(y_pos(obj10))~=3,
observation(colour(obj10))~=wall,
observation(nothing(obj10))~=no,
observation(x_pos(obj11))~=4,
observation(y_pos(obj11))~=3,
observation(colour(obj11))~=wall,
observation(nothing(obj11))~=no,
observation(x_pos(obj12))~=0,
observation(y_pos(obj12))~=4,
observation(colour(obj12))~=wall,
observation(nothing(obj12))~=no,
observation(x_pos(obj13))~=1,
observation(y_pos(obj13))~=4,
observation(colour(obj13))~=wall,
observation(nothing(obj13))~=no,
observation(x_pos(obj14))~=2,
observation(y_pos(obj14))~=4,
observation(colour(obj14))~=wall,
observation(nothing(obj14))~=no,
observation(x_pos(obj15))~=3,
observation(y_pos(obj15))~=4,
observation(colour(obj15))~=wall,
observation(nothing(obj15))~=no,
observation(x_pos(obj16))~=4,
observation(y_pos(obj16))~=4,
observation(colour(obj16))~=wall,
observation(nothing(obj16))~=no,
observation(x_pos(obj17))~=1,
observation(y_pos(obj17))~=1,
observation(colour(obj17))~=hole,
observation(nothing(obj17))~=yes,
observation(x_pos(obj18))~=3,
observation(y_pos(obj18))~=1,
observation(colour(obj18))~=goal,
observation(nothing(obj18))~=yes,
observation(x_pos(obj19))~=1,
observation(y_pos(obj19))~=3,
observation(colour(obj19))~=agent,
observation(nothing(obj19))~=no,
% observation(x_pos(no_obj0))~=1,
% observation(y_pos(no_obj0))~=2,
% observation(nothing(no_obj0))~=yes,
% observation(x_pos(no_obj1))~=2,
% observation(y_pos(no_obj1))~=1,
% observation(nothing(no_obj1))~=yes,
% observation(x_pos(no_obj2))~=2,
% observation(y_pos(no_obj2))~=2,
% observation(nothing(no_obj2))~=yes,
% observation(x_pos(no_obj3))~=2,
% observation(y_pos(no_obj3))~=3,
% observation(nothing(no_obj3))~=yes,
% observation(x_pos(no_obj4))~=3,
% observation(y_pos(no_obj4))~=1,
% observation(nothing(no_obj4))~=yes,
observation(nothing(no_object))~=yes]
                        ,100,6,TotalR,T,6,STOP).
