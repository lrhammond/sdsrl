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
:- set_current2nextcopy(true).

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

% Actions
adm(action(A)):t <- member(A, [r,l,u,d]).


nb1(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y + 1, map(NbX, NbY, Nb):t.
nb2(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y + 1, map(NbX, NbY, Nb):t.
nb3(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y + 1, map(NbX, NbY, Nb):t.

nb4(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y    , map(NbX, NbY, Nb):t.
% nb4(Obj,no_object):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y    , \+((map(NbX, NbY, Nb):t)).

nb5(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X + 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb6(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X,     NbY is Y - 1, map(NbX, NbY, Nb):t.
nb7(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y - 1, map(NbX, NbY, Nb):t.
nb8(Obj,Nb):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y    , map(NbX, NbY, Nb):t.
% nb8(Obj,no_object):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, NbX is X - 1, NbY is Y    , \+((map(NbX, NbY, Nb):t)).


% nbL(Obj):t ~ val(no_object) <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y, \+((nbLpred(X, Y, _):t)).
% nbL(Obj):t+1 ~ val(NbL) <-       new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, nbLpred(NX, NY, NbL):t.
% nbL(Obj):t+1 ~ val(no_object) <- new_x_pos(Obj, NX):t, new_y_pos(Obj, NY):t, \+((nbLpred(NX, NY, _):t)).

% Attempt 1
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1, y_pos(Obj):t ~= Y, map(New, Y, no_object):t.
% schema_x_pos(Obj, New):t <- colour(Obj):t ~= Agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.

s1(Obj, New):t <- colour(Obj):t ~= agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1, nb4(Obj, NB):t, nothing(NB):t ~= yes.
s2(Obj, New):t <- colour(Obj):t ~= agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1, nb8(Obj, NB):t, nothing(NB):t ~= yes.
s1no(Obj, New):t <- nothing(Obj):t ~= yes, x_pos(Obj):t ~= X, s1(NB, X):t, New is X - 1.
s2no(Obj, New):t <- nothing(Obj):t ~= yes, x_pos(Obj):t ~= X, s2(NB, X):t, New is X + 1.

t1(Obj, New):t <- colour(Obj):t ~= agent, action(d), y_pos(Obj):t ~= Curr, New is Curr + 1, nb2(Obj, NB):t, nothing(NB):t ~= yes.
t2(Obj, New):t <- colour(Obj):t ~= agent, action(u), y_pos(Obj):t ~= Curr, New is Curr - 1, nb6(Obj, NB):t, nothing(NB):t ~= yes.
t1no(Obj, New):t <- nothing(Obj):t ~= yes, y_pos(Obj):t ~= Y, t1(NB, Y):t, New is Y - 1.
t2no(Obj, New):t <- nothing(Obj):t ~= yes, y_pos(Obj):t ~= Y, t2(NB, Y):t, New is Y + 1.


is_obj(Obj):t <- member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]).

map(X, Y, Obj):t <- x_pos(Obj):t ~= X, y_pos(Obj):t ~= Y.
map(X, Y, no_object):t <- \+((map(X, Y, obj1):t)),
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
                          \+((map(X, Y, obj17):t)),
                          \+((map(X, Y, obj18):t)),
                          \+((map(X, Y, obj19):t)).


% map(X, Y, no_object):t ~ val(no_object)<- .
% map(X, Y, Obj):t+1 <- schema_y_pos(Obj, Y):t, x_pos(Obj):t ~= X.
% map(X, Y, Obj):t+1 <- no_schema_y_pos(Obj, Y):t, x_pos(Obj):t ~= X.

% map(X, Y, Obj):t+1 <- schema_x_pos(Obj, X):t, schema_y_pos(Obj, Y):t.
% map(X, Y, Obj):t+1 <- no_schema_x_pos(Obj, X):t, schema_y_pos(Obj, Y):t.
% map(X, Y, Obj):t+1 <- schema_x_pos(Obj, X):t, no_schema_y_pos(Obj, Y):t.
% map(X, Y, Obj):t+1 <- no_schema_x_pos(Obj, X):t, no_schema_y_pos(Obj, Y):t.


schema_x_pos(Obj, New):t <- s1(Obj, New):t.
schema_x_pos(Obj, New):t <- s2(Obj, New):t.
% schema_x_pos(Obj, New):t <- s1no(Obj, New):t.
% schema_x_pos(Obj, New):t <- s2no(Obj, New):t.

schema_y_pos(Obj, New):t <- t1(Obj, New):t.
schema_y_pos(Obj, New):t <- t2(Obj, New):t.
% schema_y_pos(Obj, New):t <- t1no(Obj, New):t.
% schema_y_pos(Obj, New):t <- t2no(Obj, New):t.

% When no x position schema is activated
% no_schema_x_pos(Obj, Curr):t <- \+((schema_x_pos(Obj, _):t)), x_pos(Obj):t ~= Curr.
% no_schema_y_pos(Obj, Curr):t <- \+((schema_y_pos(Obj, _):t)), y_pos(Obj):t ~= Curr.

% Update x position depending on schema
x_pos(Obj):t+1 ~ val(New) <- schema_x_pos(Obj, New):t.
y_pos(Obj):t+1 ~ val(New) <- schema_y_pos(Obj, New):t.

% Other variables stay the same
x_pos(Obj):t+1 ~ val(Curr) <- x_pos(Obj):t ~= Curr.
y_pos(Obj):t+1 ~ val(Curr) <- y_pos(Obj):t ~= Curr.
colour(Obj):t+1 ~ val(New) <- colour(Obj):t ~= New.
nothing(Obj):t+1 ~ val(New) <- nothing(Obj):t ~= New.


% reward:t ~ val(1) <-  x_pos(obj19):t ~= 1.
% reward:t ~ val(10) <- x_pos(obj19):t ~= 2.
% reward:t ~ val(100) <- x_pos(obj19):t ~= 3.
% reward:t ~ val(0) <- x_pos(obj19):t ~= X, X \= 1, X \= 2, X \= 3.

% reward:t ~ val(1) <-  map(1, 1, obj19):t.
% reward:t ~ val(10) <- map(2, 1, obj19):t.
% reward:t ~ val(100) <- map(3, 1, obj19):t.
% reward:t ~ val(0) <- map(X, _, obj19):t, X \= 1, X \= 2, X \= 3.
% reward:t ~ val(0) <- map(_, Y, obj19):t, Y \= 1.

% reward:t ~ val(1000) <- map(0, 1, obj19):t.
% reward:t ~ val(1) <-  map(1, 1, obj19):t.
% reward:t ~ val(10) <- map(2, 1, obj19):t.
% reward:t ~ val(100) <- map(3, 1, obj19):t.
% reward:t ~ val(1000) <- map(4, 1, obj19):t.
% % reward:t ~ val(1000) <- map(2, 2, obj19):t.
% reward:t ~ val(0) <- map(X,_,obj19):t, X \= 0, X \= 1, X \= 2, X \= 3, X \= 4.
% % reward:t ~ val(0) <- map(X,Y, obj19):t, Y \= 1, Y \= 2, X \= 2.

reward:t ~ val(1) <-  map(1, 1, obj19):t.
reward:t ~ val(10) <- map(2, 1, obj19):t.
reward:t ~ val(100) <- map(3, 1, obj19):t.
% reward:t ~ val(100) <- map(2, 2, obj19):t.
reward:t ~ val(0) <- map(X,_,obj19):t, X \= 1, X \= 2, X \= 3.
reward:t ~ val(0) <- map(_, Y, obj19):t, Y \= 1.

% Run example
c :- executedplan_start, executedplan_step(BA,false,
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
observation(y_pos(obj17))~=3,
observation(colour(obj17))~=hole,
observation(nothing(obj17))~=yes,
observation(x_pos(obj18))~=3,
observation(y_pos(obj18))~=3,
observation(colour(obj18))~=goal,
observation(nothing(obj18))~=yes,
observation(x_pos(obj19))~=1,
observation(y_pos(obj19))~=1,
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
