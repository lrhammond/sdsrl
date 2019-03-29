% Prolog model of test

% Libraries
:- use_module(library(planning)).
:- use_module(library(lists)).

% Options
:- set_options(default).
:- set_query_propagation(true).
:- set_inference(backward(lazy)).
:- set_current2nextcopy(false).

% Parameters
Y:t+1 ~ val(X) <- observation(Y) ~= X.
observation(Y):t+1 ~ val(X) <- Y:t+1 ~= X.
getparam(params) :-
	bb_put(user:spant,0),
	setparam(
        % Enable abstraction
        false,
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
        0.0,
        % Probability to explore in the end (last sample)
        0.0,
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
builtin(x_pos(_)).
builtin(y_pos(_)).
builtin(x_size(_)).
builtin(y_size(_)).
builtin(colour(_)).
builtin(shape(_)).
builtin(nothing(_)).

% Actions
adm(action(A)):t <- member(A, [up,left,down,right,none]).

% Neighbours
nb1(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x - 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x + 1.
nb2(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y = X_pos_x,      y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x + 1.
nb3(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x + 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x + 1.
nb4(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x + 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y = Y_pos_x.
nb5(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x + 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x - 1.
nb6(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y = X_pos_x,      y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x - 1.
nb7(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x - 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y is Y_pos_x - 1.
nb8(X,Y):t <- x_pos(Y):t ~= X_pos_y, x_pos(X):t ~= X_pos_x, X_pos_y is X_pos_x - 1, y_pos(Y):t ~= Y_pos_y, y_pos(X):t ~= Y_pos_x, Y_pos_y = Y_pos_x.

% Nothing
nothing(X):t ~ val(Y) <- (member(X, [obj0,obj1,obj2,obj3,obj4,obj5,obj6,obj7,obj8,obj9,obj10,obj11,obj12,obj13,obj14,obj15,obj16,obj17,obj18,obj19]) -> Y = no ; Y = yes).

% Attribute Schemas
x_pos(X):t+1 ~ val(New) <- (nb6(X,NB6):t, colour(NB6):t ~= hole, action(RIGHT) -> right = New, x_pos(X):t ~= Curr, right is Curr + 1 ; x_pos(X):t ~= New).
x_pos(X):t+1 ~ val(New) <- (nb5(X,NB5):t, colour(NB5):t ~= goal, action(LEFT) -> left = New, x_pos(X):t ~= Curr, left is Curr - 1 ; x_pos(X):t ~= New).
y_pos(X):t+1 ~ val(New) <- (nb5(X,NB5):t, colour(NB5):t ~= goal, action(DOWN) -> below = New, y_pos(X):t ~= Curr, below is Curr - 1 ; y_pos(X):t ~= New).
y_pos(X):t+1 ~ val(New) <- (nb4(X,NB4):t, nothing(NB4):t ~= yes, nb6(X,NB6):t, nothing(NB6):t ~= yes, nb7(X,NB7):t, nothing(NB7):t ~= no, colour(X):t ~= agent, nb3(X,NB3):t, colour(NB3):t ~= wall, nb2(X,NB2):t, colour(NB2):t ~= wall, action(DOWN) -> below = New, y_pos(X):t ~= Curr, below is Curr - 1 ; y_pos(X):t ~= New).
y_pos(X):t+1 ~ val(New) <- (nb5(X,NB5):t, colour(NB5):t ~= goal, action(UP) -> above = New, y_pos(X):t ~= Curr, above is Curr + 1 ; y_pos(X):t ~= New).

% Reward Schemas
reward:0 ~ val(0).
reward:t+1 ~ val(Reward) <- (nb6(X,NB6):t, colour(NB6):t ~= wall, action(RIGHT) -> 10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb2(X,NB2):t, nothing(NB2):t ~= no, colour(X):t ~= wall, action(RIGHT) -> 10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb2(X,NB2):t, colour(NB2):t ~= agent, action(DOWN) -> -10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb2(X,NB2):t, colour(NB2):t ~= goal, action(LEFT) -> -10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb3(X,NB3):t, nothing(NB3):t ~= no, colour(X):t ~= wall, action(LEFT) -> -10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb5(X,NB5):t, colour(NB5):t ~= agent, nb4(X,NB4):t, nothing(NB4):t ~= yes, action(DOWN) -> -10 = Reward ; Reward = -1).
reward:t+1 ~ val(Reward) <- (nb5(X,NB5):t, nothing(NB5):t ~= no, nb7(X,NB7):t, nothing(NB7):t ~= no, nb2(X,NB2):t, nothing(NB2):t ~= yes, nb4(X,NB4):t, nothing(NB4):t ~= no, nb6(X,NB6):t, nothing(NB6):t ~= no, action(LEFT) -> -10 = Reward ; Reward = -1).