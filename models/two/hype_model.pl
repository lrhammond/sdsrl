% Prolog model for two

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
maxV(D,50):t <- true.
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
                        0.15,
                        % Number of previous samples to use to estimate Q (larger is better but slower)
                        100,
                        % Max horizon span
                        100,
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

% Core Functions
Var:t+1 ~ val(Val) <- observation(Var) ~= Val.
observation(Var):t+1 ~ val(Val) <- Var:t+1 ~= Val.

% Helper Functions
dist(Obj1, Obj2):t ~ val(D) <- x_pos(Obj1):t ~= X1, y_pos(Obj1):t ~= Y1, x_pos(Obj2):t ~= X2, y_pos(Obj2):t ~= Y2, D is sqrt((X1 - X2)^2 + (Y1 - Y2)^2).
dist_gt(Obj1, Obj2, V):t <- dist(Obj1, Obj2):t ~= D, D > V.
dist_lt(Obj1, Obj2, V):t <- dist(Obj1, Obj2):t ~= D, D < V.
dist_eq(Obj1, Obj2, V):t <- dist(Obj1, Obj2):t ~= D, D = V.
right_of(Obj1, Obj2):t <- x_pos(Obj1):t ~= X1, x_pos(Obj2):t ~= X2, X1 > X2.
left_of(Obj1, Obj2):t <- x_pos(Obj1):t ~= X1, x_pos(Obj2):t ~= X2, X1 < X2.
above(Obj1, Obj2):t <- y_pos(Obj1):t ~= Y1, y_pos(Obj2):t ~= Y2, Y1 > Y2.
below(Obj1, Obj2):t <- y_pos(Obj1):t ~= Y1, y_pos(Obj2):t ~= Y2, Y1 < Y2.
bigger_x(Obj1, Obj2):t <- x_size(Obj1):t ~= XS1, x_size(Obj2):t ~= XS2, XS1 > XS2.
bigger_y(Obj1, Obj2):t <- y_size(Obj1):t ~= YS1, y_size(Obj2):t ~= YS2, YS1 > YS2.
bigger(Obj1, Obj2):t <- x_size(Obj1):t ~= XS1, x_size(Obj2):t ~= XS2, y_size(Obj1):t ~= YS1, y_size(Obj2):t ~= YS2, (XS1 * YS1) > (XS2 * YS2).
occupied_pos(X, Y):t <- is_object(Obj), map(X, Y, Obj):t.
unoccupied_pos(X, Y):t <- map(X, Y, no_object):t.
same_x_pos(Obj1, Obj2):t <- x_pos(Obj1):t ~= X1, x_pos(Obj2):t ~= X2, X1 = X2.
same_y_pos(Obj1, Obj2):t <- y_pos(Obj1):t ~= Y1, y_pos(Obj2):t ~= Y2, Y1 = Y2.
same_x_size(Obj1, Obj2):t <- x_size(Obj1):t ~= XS1, x_size(Obj2):t ~= XS2, XS1 = XS2.
same_y_size(Obj1, Obj2):t <- y_size(Obj1):t ~= YS1, y_size(Obj2):t ~= YS2, YS1 = YS2.
same_colour(Obj1, Obj2):t <- colour(Obj1):t ~= C1, colour(Obj2):t ~= C2, C1 = C2.
same_shape(Obj1, Obj2):t <- shape(Obj1):t ~= S1, shape(Obj2):t ~= S2, S1 = S2.
attributes(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing):t <- x_pos(Obj):t ~= X_pos, 
                                                                           y_pos(Obj):t ~= Y_pos, 
                                                                           x_size(Obj):t ~= X_size, 
                                                                           y_size(Obj):t ~= Y_size, 
                                                                           colour(Obj):t ~= Colour,
                                                                           shape(Obj):t ~= Shape,
                                                                           nothing(Obj):t ~= Nothing.

% Actions
adm(action(A)):t <- member(A, [u,l,d,r]).
\+(action_performed:0) <- true.
action_performed:t+1 <- action(A), member(A, [u,l,d,r]).

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
map(X, Y, no_object):t <- \+((map(X, Y, obj0):t)), \+((map(X, Y, obj1):t)), \+((map(X, Y, obj2):t)), \+((map(X, Y, obj3):t)), \+((map(X, Y, obj4):t)), \+((map(X, Y, obj5):t)), \+((map(X, Y, obj6):t)), \+((map(X, Y, obj7):t)), \+((map(X, Y, obj8):t)), \+((map(X, Y, obj9):t)), \+((map(X, Y, obj10):t)), \+((map(X, Y, obj11):t)), \+((map(X, Y, obj12):t)), \+((map(X, Y, obj13):t)), \+((map(X, Y, obj14):t)), \+((map(X, Y, obj15):t)), \+((map(X, Y, obj16):t)), \+((map(X, Y, obj17):t)), \+((map(X, Y, obj18):t)), \+((map(X, Y, obj19):t)), \+((map(X, Y, obj20):t)), \+((map(X, Y, obj21):t)), \+((map(X, Y, obj22):t)), \+((map(X, Y, obj23):t)), \+((map(X, Y, obj24):t)), \+((map(X, Y, obj25):t)), \+((map(X, Y, obj26):t)), \+((map(X, Y, obj27):t)), \+((map(X, Y, obj28):t)), \+((map(X, Y, obj29):t)), \+((map(X, Y, obj30):t)), \+((map(X, Y, obj31):t)), \+((map(X, Y, obj32):t)), \+((map(X, Y, obj33):t)), \+((map(X, Y, obj34):t)), \+((map(X, Y, obj35):t)), \+((map(X, Y, obj36):t)), \+((map(X, Y, obj37):t)), \+((map(X, Y, obj38):t)), \+((map(X, Y, obj39):t)), \+((map(X, Y, obj40):t)), \+((map(X, Y, obj41):t)), \+((map(X, Y, obj42):t)), \+((map(X, Y, obj43):t)), \+((map(X, Y, obj44):t)), \+((map(X, Y, obj45):t)), \+((map(X, Y, obj46):t)), \+((map(X, Y, obj47):t)), \+((map(X, Y, obj48):t)), \+((map(X, Y, obj49):t)), \+((map(X, Y, obj50):t)), \+((map(X, Y, obj51):t)).

% Nothing
nothing(no_object):t+1 ~ val(Curr) <- nothing(no_object):t ~= Curr.

% Objects
is_object(Obj) <- member(Obj, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19,obj20,obj21,obj22,obj23,obj24,obj25,obj26,obj27,obj28,obj29,obj30,obj31,obj32,obj33,obj34,obj35,obj36,obj37,obj38,obj39,obj40,obj41,obj42,obj43,obj44,obj45,obj46,obj47,obj48,obj49,obj50,obj51]).

% Constraints
constraints:t <- true.

% Attribute Schemas
schema_x_pos(Obj, New):t <- nb5(Obj,Nb5):t, nothing(Nb5):t ~= yes, nb7(Obj,Nb7):t, colour(Nb7):t ~= wall, nb8(Obj,Nb8):t, colour(Nb8):t ~= wall, nb4(Obj,Nb4):t, nothing(Nb4):t ~= yes, colour(Obj):t ~= agent, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1.
schema_x_pos(Obj, New):t <- nb5(Obj,Nb5):t, colour(Nb5):t ~= wall, nb4(Obj,Nb4):t, nothing(Nb4):t ~= yes, nb6(Obj,Nb6):t, nothing(Nb6):t ~= yes, nb1(Obj,Nb1):t, colour(Nb1):t ~= wall, nb8(Obj,Nb8):t, nothing(Nb8):t ~= yes, nb3(Obj,Nb3):t, colour(Nb3):t ~= wall, action(r), x_pos(Obj):t ~= Curr, New is Curr + 1.
schema_x_pos(Obj, New):t <- nb8(Obj,Nb8):t, nothing(Nb8):t ~= yes, colour(Obj):t ~= agent, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.
schema_x_pos(Obj, New):t <- nb8(Obj,Nb8):t, nothing(Nb8):t ~= yes, nb5(Obj,Nb5):t, colour(Nb5):t ~= wall, nb7(Obj,Nb7):t, nothing(Nb7):t ~= yes, nb1(Obj,Nb1):t, colour(Nb1):t ~= wall, action(l), x_pos(Obj):t ~= Curr, New is Curr - 1.
x_pos(Obj):t+1 ~ val(New) <- schema_x_pos(Obj, New):t.
x_pos(Obj):t+1 ~ val(Curr) <- x_pos(Obj):t ~= Curr.
schema_y_pos(Obj, New):t <- nb8(Obj,Nb8):t, nothing(Nb8):t ~= yes, nb6(Obj,Nb6):t, nothing(Nb6):t ~= yes, action(d), y_pos(Obj):t ~= Curr, New is Curr - 1.
schema_y_pos(Obj, New):t <- nb5(Obj,Nb5):t, nothing(Nb5):t ~= yes, nb7(Obj,Nb7):t, nothing(Nb7):t ~= no, nb1(Obj,Nb1):t, colour(Nb1):t ~= wall, nb8(Obj,Nb8):t, colour(Nb8):t ~= wall, action(d), y_pos(Obj):t ~= Curr, New is Curr - 1.
schema_y_pos(Obj, New):t <- nb4(Obj,Nb4):t, colour(Nb4):t ~= goal, action(u), y_pos(Obj):t ~= Curr, New is Curr + 1.
schema_y_pos(Obj, New):t <- nb7(Obj,Nb7):t, colour(Nb7):t ~= hole, action(u), y_pos(Obj):t ~= Curr, New is Curr + 1.
schema_y_pos(Obj, New):t <- nb6(Obj,Nb6):t, colour(Nb6):t ~= hole, action(u), y_pos(Obj):t ~= Curr, New is Curr + 1.
y_pos(Obj):t+1 ~ val(New) <- schema_y_pos(Obj, New):t.
y_pos(Obj):t+1 ~ val(Curr) <- y_pos(Obj):t ~= Curr.
x_size(Obj):t+1 ~ val(Curr) <- x_size(Obj):t ~= Curr.
y_size(Obj):t+1 ~ val(Curr) <- y_size(Obj):t ~= Curr.
colour(Obj):t+1 ~ val(Curr) <- colour(Obj):t ~= Curr.
shape(Obj):t+1 ~ val(Curr) <- shape(Obj):t ~= Curr.
nothing(Obj):t+1 ~ val(Curr) <- nothing(Obj):t ~= Curr.

% Reward Schemas
reward:t ~ val(100) <- constraints:t, action(A), member(A, [u]), \+action_performed:t.
reward:t ~ val(10) <- constraints:t, is_object(Obj), nb4(Obj,Nb4):t, colour(Nb4):t ~= goal, action(r).
reward:t ~ val(-1) <- constraints:t, is_object(Obj), nb1(Obj,Nb1):t, colour(Nb1):t ~= wall, colour(Obj):t ~= agent.
reward:t ~ val(-1) <- constraints:t, is_object(Obj), nb4(Obj,Nb4):t, colour(Nb4):t ~= wall, colour(Obj):t ~= agent.
reward:t ~ val(-1) <- constraints:t, is_object(Obj), nb6(Obj,Nb6):t, nothing(Nb6):t ~= yes, colour(Obj):t ~= agent.
reward:t ~ val(-1) <- constraints:t, is_object(Obj), nb7(Obj,Nb7):t, colour(Nb7):t ~= wall, nb2(Obj,Nb2):t, nothing(Nb2):t ~= yes, nb3(Obj,Nb3):t, colour(Nb3):t ~= wall, action(u).
reward:t ~ val(-1) <- constraints:t, is_object(Obj), nb6(Obj,Nb6):t, colour(Nb6):t ~= wall, nb2(Obj,Nb2):t, nothing(Nb2):t ~= yes, nb3(Obj,Nb3):t, colour(Nb3):t ~= wall, action(d).
reward:t ~ val(-10) <- constraints:t, is_object(Obj), nb8(Obj,Nb8):t, colour(Nb8):t ~= hole, action(l).

% Run command
run :- executedplan_start,executedplan_step(BA,true,[observation(x_pos(obj0)) ~= 0, observation(y_pos(obj0)) ~= 6, observation(x_size(obj0)) ~= 1, observation(y_size(obj0)) ~= 1, observation(colour(obj0)) ~= wall, observation(shape(obj0)) ~= square, observation(nothing(obj0)) ~= no,
observation(x_pos(obj1)) ~= 1, observation(y_pos(obj1)) ~= 6, observation(x_size(obj1)) ~= 1, observation(y_size(obj1)) ~= 1, observation(colour(obj1)) ~= wall, observation(shape(obj1)) ~= square, observation(nothing(obj1)) ~= no,
observation(x_pos(obj2)) ~= 2, observation(y_pos(obj2)) ~= 6, observation(x_size(obj2)) ~= 1, observation(y_size(obj2)) ~= 1, observation(colour(obj2)) ~= wall, observation(shape(obj2)) ~= square, observation(nothing(obj2)) ~= no,
observation(x_pos(obj3)) ~= 3, observation(y_pos(obj3)) ~= 6, observation(x_size(obj3)) ~= 1, observation(y_size(obj3)) ~= 1, observation(colour(obj3)) ~= wall, observation(shape(obj3)) ~= square, observation(nothing(obj3)) ~= no,
observation(x_pos(obj4)) ~= 4, observation(y_pos(obj4)) ~= 6, observation(x_size(obj4)) ~= 1, observation(y_size(obj4)) ~= 1, observation(colour(obj4)) ~= wall, observation(shape(obj4)) ~= square, observation(nothing(obj4)) ~= no,
observation(x_pos(obj5)) ~= 5, observation(y_pos(obj5)) ~= 6, observation(x_size(obj5)) ~= 1, observation(y_size(obj5)) ~= 1, observation(colour(obj5)) ~= wall, observation(shape(obj5)) ~= square, observation(nothing(obj5)) ~= no,
observation(x_pos(obj6)) ~= 6, observation(y_pos(obj6)) ~= 6, observation(x_size(obj6)) ~= 1, observation(y_size(obj6)) ~= 1, observation(colour(obj6)) ~= wall, observation(shape(obj6)) ~= square, observation(nothing(obj6)) ~= no,
observation(x_pos(obj7)) ~= 7, observation(y_pos(obj7)) ~= 6, observation(x_size(obj7)) ~= 1, observation(y_size(obj7)) ~= 1, observation(colour(obj7)) ~= wall, observation(shape(obj7)) ~= square, observation(nothing(obj7)) ~= no,
observation(x_pos(obj8)) ~= 8, observation(y_pos(obj8)) ~= 6, observation(x_size(obj8)) ~= 1, observation(y_size(obj8)) ~= 1, observation(colour(obj8)) ~= wall, observation(shape(obj8)) ~= square, observation(nothing(obj8)) ~= no,
observation(x_pos(obj9)) ~= 9, observation(y_pos(obj9)) ~= 6, observation(x_size(obj9)) ~= 1, observation(y_size(obj9)) ~= 1, observation(colour(obj9)) ~= wall, observation(shape(obj9)) ~= square, observation(nothing(obj9)) ~= no,
observation(x_pos(obj10)) ~= 10, observation(y_pos(obj10)) ~= 6, observation(x_size(obj10)) ~= 1, observation(y_size(obj10)) ~= 1, observation(colour(obj10)) ~= wall, observation(shape(obj10)) ~= square, observation(nothing(obj10)) ~= no,
observation(x_pos(obj11)) ~= 11, observation(y_pos(obj11)) ~= 6, observation(x_size(obj11)) ~= 1, observation(y_size(obj11)) ~= 1, observation(colour(obj11)) ~= wall, observation(shape(obj11)) ~= square, observation(nothing(obj11)) ~= no,
observation(x_pos(obj12)) ~= 12, observation(y_pos(obj12)) ~= 6, observation(x_size(obj12)) ~= 1, observation(y_size(obj12)) ~= 1, observation(colour(obj12)) ~= wall, observation(shape(obj12)) ~= square, observation(nothing(obj12)) ~= no,
observation(x_pos(obj13)) ~= 0, observation(y_pos(obj13)) ~= 5, observation(x_size(obj13)) ~= 1, observation(y_size(obj13)) ~= 1, observation(colour(obj13)) ~= wall, observation(shape(obj13)) ~= square, observation(nothing(obj13)) ~= no,
observation(x_pos(obj14)) ~= 9, observation(y_pos(obj14)) ~= 5, observation(x_size(obj14)) ~= 1, observation(y_size(obj14)) ~= 1, observation(colour(obj14)) ~= wall, observation(shape(obj14)) ~= square, observation(nothing(obj14)) ~= no,
observation(x_pos(obj15)) ~= 12, observation(y_pos(obj15)) ~= 5, observation(x_size(obj15)) ~= 1, observation(y_size(obj15)) ~= 1, observation(colour(obj15)) ~= wall, observation(shape(obj15)) ~= square, observation(nothing(obj15)) ~= no,
observation(x_pos(obj16)) ~= 0, observation(y_pos(obj16)) ~= 4, observation(x_size(obj16)) ~= 1, observation(y_size(obj16)) ~= 1, observation(colour(obj16)) ~= wall, observation(shape(obj16)) ~= square, observation(nothing(obj16)) ~= no,
observation(x_pos(obj17)) ~= 4, observation(y_pos(obj17)) ~= 4, observation(x_size(obj17)) ~= 1, observation(y_size(obj17)) ~= 1, observation(colour(obj17)) ~= wall, observation(shape(obj17)) ~= square, observation(nothing(obj17)) ~= no,
observation(x_pos(obj18)) ~= 11, observation(y_pos(obj18)) ~= 4, observation(x_size(obj18)) ~= 1, observation(y_size(obj18)) ~= 1, observation(colour(obj18)) ~= wall, observation(shape(obj18)) ~= square, observation(nothing(obj18)) ~= no,
observation(x_pos(obj19)) ~= 12, observation(y_pos(obj19)) ~= 4, observation(x_size(obj19)) ~= 1, observation(y_size(obj19)) ~= 1, observation(colour(obj19)) ~= wall, observation(shape(obj19)) ~= square, observation(nothing(obj19)) ~= no,
observation(x_pos(obj20)) ~= 0, observation(y_pos(obj20)) ~= 3, observation(x_size(obj20)) ~= 1, observation(y_size(obj20)) ~= 1, observation(colour(obj20)) ~= wall, observation(shape(obj20)) ~= square, observation(nothing(obj20)) ~= no,
observation(x_pos(obj21)) ~= 1, observation(y_pos(obj21)) ~= 3, observation(x_size(obj21)) ~= 1, observation(y_size(obj21)) ~= 1, observation(colour(obj21)) ~= wall, observation(shape(obj21)) ~= square, observation(nothing(obj21)) ~= no,
observation(x_pos(obj22)) ~= 2, observation(y_pos(obj22)) ~= 3, observation(x_size(obj22)) ~= 1, observation(y_size(obj22)) ~= 1, observation(colour(obj22)) ~= wall, observation(shape(obj22)) ~= square, observation(nothing(obj22)) ~= no,
observation(x_pos(obj23)) ~= 4, observation(y_pos(obj23)) ~= 3, observation(x_size(obj23)) ~= 1, observation(y_size(obj23)) ~= 1, observation(colour(obj23)) ~= wall, observation(shape(obj23)) ~= square, observation(nothing(obj23)) ~= no,
observation(x_pos(obj24)) ~= 8, observation(y_pos(obj24)) ~= 3, observation(x_size(obj24)) ~= 1, observation(y_size(obj24)) ~= 1, observation(colour(obj24)) ~= wall, observation(shape(obj24)) ~= square, observation(nothing(obj24)) ~= no,
observation(x_pos(obj25)) ~= 9, observation(y_pos(obj25)) ~= 3, observation(x_size(obj25)) ~= 1, observation(y_size(obj25)) ~= 1, observation(colour(obj25)) ~= wall, observation(shape(obj25)) ~= square, observation(nothing(obj25)) ~= no,
observation(x_pos(obj26)) ~= 10, observation(y_pos(obj26)) ~= 3, observation(x_size(obj26)) ~= 1, observation(y_size(obj26)) ~= 1, observation(colour(obj26)) ~= wall, observation(shape(obj26)) ~= square, observation(nothing(obj26)) ~= no,
observation(x_pos(obj27)) ~= 11, observation(y_pos(obj27)) ~= 3, observation(x_size(obj27)) ~= 1, observation(y_size(obj27)) ~= 1, observation(colour(obj27)) ~= wall, observation(shape(obj27)) ~= square, observation(nothing(obj27)) ~= no,
observation(x_pos(obj28)) ~= 12, observation(y_pos(obj28)) ~= 3, observation(x_size(obj28)) ~= 1, observation(y_size(obj28)) ~= 1, observation(colour(obj28)) ~= wall, observation(shape(obj28)) ~= square, observation(nothing(obj28)) ~= no,
observation(x_pos(obj29)) ~= 0, observation(y_pos(obj29)) ~= 2, observation(x_size(obj29)) ~= 1, observation(y_size(obj29)) ~= 1, observation(colour(obj29)) ~= wall, observation(shape(obj29)) ~= square, observation(nothing(obj29)) ~= no,
observation(x_pos(obj30)) ~= 8, observation(y_pos(obj30)) ~= 2, observation(x_size(obj30)) ~= 1, observation(y_size(obj30)) ~= 1, observation(colour(obj30)) ~= wall, observation(shape(obj30)) ~= square, observation(nothing(obj30)) ~= no,
observation(x_pos(obj31)) ~= 9, observation(y_pos(obj31)) ~= 2, observation(x_size(obj31)) ~= 1, observation(y_size(obj31)) ~= 1, observation(colour(obj31)) ~= wall, observation(shape(obj31)) ~= square, observation(nothing(obj31)) ~= no,
observation(x_pos(obj32)) ~= 12, observation(y_pos(obj32)) ~= 2, observation(x_size(obj32)) ~= 1, observation(y_size(obj32)) ~= 1, observation(colour(obj32)) ~= wall, observation(shape(obj32)) ~= square, observation(nothing(obj32)) ~= no,
observation(x_pos(obj33)) ~= 0, observation(y_pos(obj33)) ~= 1, observation(x_size(obj33)) ~= 1, observation(y_size(obj33)) ~= 1, observation(colour(obj33)) ~= wall, observation(shape(obj33)) ~= square, observation(nothing(obj33)) ~= no,
observation(x_pos(obj34)) ~= 5, observation(y_pos(obj34)) ~= 1, observation(x_size(obj34)) ~= 1, observation(y_size(obj34)) ~= 1, observation(colour(obj34)) ~= wall, observation(shape(obj34)) ~= square, observation(nothing(obj34)) ~= no,
observation(x_pos(obj35)) ~= 12, observation(y_pos(obj35)) ~= 1, observation(x_size(obj35)) ~= 1, observation(y_size(obj35)) ~= 1, observation(colour(obj35)) ~= wall, observation(shape(obj35)) ~= square, observation(nothing(obj35)) ~= no,
observation(x_pos(obj36)) ~= 0, observation(y_pos(obj36)) ~= 0, observation(x_size(obj36)) ~= 1, observation(y_size(obj36)) ~= 1, observation(colour(obj36)) ~= wall, observation(shape(obj36)) ~= square, observation(nothing(obj36)) ~= no,
observation(x_pos(obj37)) ~= 1, observation(y_pos(obj37)) ~= 0, observation(x_size(obj37)) ~= 1, observation(y_size(obj37)) ~= 1, observation(colour(obj37)) ~= wall, observation(shape(obj37)) ~= square, observation(nothing(obj37)) ~= no,
observation(x_pos(obj38)) ~= 2, observation(y_pos(obj38)) ~= 0, observation(x_size(obj38)) ~= 1, observation(y_size(obj38)) ~= 1, observation(colour(obj38)) ~= wall, observation(shape(obj38)) ~= square, observation(nothing(obj38)) ~= no,
observation(x_pos(obj39)) ~= 3, observation(y_pos(obj39)) ~= 0, observation(x_size(obj39)) ~= 1, observation(y_size(obj39)) ~= 1, observation(colour(obj39)) ~= wall, observation(shape(obj39)) ~= square, observation(nothing(obj39)) ~= no,
observation(x_pos(obj40)) ~= 4, observation(y_pos(obj40)) ~= 0, observation(x_size(obj40)) ~= 1, observation(y_size(obj40)) ~= 1, observation(colour(obj40)) ~= wall, observation(shape(obj40)) ~= square, observation(nothing(obj40)) ~= no,
observation(x_pos(obj41)) ~= 5, observation(y_pos(obj41)) ~= 0, observation(x_size(obj41)) ~= 1, observation(y_size(obj41)) ~= 1, observation(colour(obj41)) ~= wall, observation(shape(obj41)) ~= square, observation(nothing(obj41)) ~= no,
observation(x_pos(obj42)) ~= 6, observation(y_pos(obj42)) ~= 0, observation(x_size(obj42)) ~= 1, observation(y_size(obj42)) ~= 1, observation(colour(obj42)) ~= wall, observation(shape(obj42)) ~= square, observation(nothing(obj42)) ~= no,
observation(x_pos(obj43)) ~= 7, observation(y_pos(obj43)) ~= 0, observation(x_size(obj43)) ~= 1, observation(y_size(obj43)) ~= 1, observation(colour(obj43)) ~= wall, observation(shape(obj43)) ~= square, observation(nothing(obj43)) ~= no,
observation(x_pos(obj44)) ~= 8, observation(y_pos(obj44)) ~= 0, observation(x_size(obj44)) ~= 1, observation(y_size(obj44)) ~= 1, observation(colour(obj44)) ~= wall, observation(shape(obj44)) ~= square, observation(nothing(obj44)) ~= no,
observation(x_pos(obj45)) ~= 9, observation(y_pos(obj45)) ~= 0, observation(x_size(obj45)) ~= 1, observation(y_size(obj45)) ~= 1, observation(colour(obj45)) ~= wall, observation(shape(obj45)) ~= square, observation(nothing(obj45)) ~= no,
observation(x_pos(obj46)) ~= 10, observation(y_pos(obj46)) ~= 0, observation(x_size(obj46)) ~= 1, observation(y_size(obj46)) ~= 1, observation(colour(obj46)) ~= wall, observation(shape(obj46)) ~= square, observation(nothing(obj46)) ~= no,
observation(x_pos(obj47)) ~= 11, observation(y_pos(obj47)) ~= 0, observation(x_size(obj47)) ~= 1, observation(y_size(obj47)) ~= 1, observation(colour(obj47)) ~= wall, observation(shape(obj47)) ~= square, observation(nothing(obj47)) ~= no,
observation(x_pos(obj48)) ~= 12, observation(y_pos(obj48)) ~= 0, observation(x_size(obj48)) ~= 1, observation(y_size(obj48)) ~= 1, observation(colour(obj48)) ~= wall, observation(shape(obj48)) ~= square, observation(nothing(obj48)) ~= no,
observation(x_pos(obj49)) ~= 2, observation(y_pos(obj49)) ~= 1, observation(x_size(obj49)) ~= 1, observation(y_size(obj49)) ~= 1, observation(colour(obj49)) ~= hole, observation(shape(obj49)) ~= square, observation(nothing(obj49)) ~= no,
observation(x_pos(obj50)) ~= 10, observation(y_pos(obj50)) ~= 2, observation(x_size(obj50)) ~= 1, observation(y_size(obj50)) ~= 1, observation(colour(obj50)) ~= goal, observation(shape(obj50)) ~= square, observation(nothing(obj50)) ~= no,
observation(x_pos(obj51)) ~= 1, observation(y_pos(obj51)) ~= 5, observation(x_size(obj51)) ~= 1, observation(y_size(obj51)) ~= 1, observation(colour(obj51)) ~= agent, observation(shape(obj51)) ~= square, observation(nothing(obj51)) ~= no,
observation(nothing(no_object))~=yes],50,5,TotalR,T,5,STOP),print(BA).