% Prolog model of MDP for test

% Libraries
    :- use_module(library(planning)).
    :- use_module(library(lists)).

    % Options
    :- set_options(default).
    :- set_query_propagation(true).
    :- set_inference(backward(lazy)).
    :- set_current2nextcopy(false).
    
    % Functions
    builtin(x_pos(_)).
    builtin(y_pos(_)).
    builtin(x_size(_)).
    builtin(y_size(_)).
    builtin(colour(_)).
    builtin(shape(_)).
    builtin(nothing(_)).

% Actions
adm(action(A):t <- member(A, [UP,LEFT,DOWN,RIGHT,nothing]).

% Neighbours
    nb1(X,Y):t <- x_pos(Y):t - x_pos(X):t = -1, y_pos(Y):t - y_pos(X):t = 1.
    nb2(X,Y):t <- x_pos(Y):t - x_pos(X):t = 0, y_pos(Y):t - y_pos(X):t =s 1.
    nb3(X,Y):t <- x_pos(Y):t - x_pos(X):t = 1, y_pos(Y):t - y_pos(X):t = 1.
    nb4(X,Y):t <- x_pos(Y):t - x_pos(X):t = 1, y_pos(Y):t - y_pos(X):t = 0.
    nb5(X,Y):t <- x_pos(Y):t - x_pos(X):t = 1, y_pos(Y):t - y_pos(X):t = -1.
    nb6(X,Y):t <- x_pos(Y):t - x_pos(X):t = 0, y_pos(Y):t - y_pos(X):t = -1.
    nb7(X,Y):t <- x_pos(Y):t - x_pos(X):t = -1, y_pos(Y):t - y_pos(X):t = -1.
    nb8(X,Y):t <- x_pos(Y):t - x_pos(X):t = -1, y_pos(Y):t - y_pos(X):t = 0.

% Attribute Schemas
x_pos(X):t+1 = right <- nb8(X,NB8):t, colour(NB8):t = wall, nb3(X,NB3):t, colour(NB3):t = wall, nb2(X,NB2):t, colour(NB2):t = wall, nb1(X,NB1):t, colour(NB1):t = wall, colour(X):t = agent, action(RIGHT), right is x_pos(X):t + 1.
y_pos(X):t+1 = below <- colour(X):t = agent, action(DOWN), below is y_pos(X):t - 1.
y_pos(X):t+1 = below <- nb7(X,NB7):t, nothing(NB7):t = yes, colour(X):t = agen, below is y_pos(X):t - 1.
y_pos(X):t+1 = above <- nb6(X,NB6):t, colour(NB6):t = hol, above is y_pos(X):t + 1.

% Reward Schemas
reward:0 = 0.

% Initialisation
x_pos(Obj0):0 = 0
y_pos(Obj0):0 = 0
x_size(Obj0):0 = 1
y_size(Obj0):0 = 1
colour(Obj0):0 = wall
shape(Obj0):0 = square
nothing(Obj0):0 = no
x_pos(Obj1):0 = 1
y_pos(Obj1):0 = 0
x_size(Obj1):0 = 1
y_size(Obj1):0 = 1
colour(Obj1):0 = wall
shape(Obj1):0 = square
nothing(Obj1):0 = no
x_pos(Obj2):0 = 2
y_pos(Obj2):0 = 0
x_size(Obj2):0 = 1
y_size(Obj2):0 = 1
colour(Obj2):0 = wall
shape(Obj2):0 = square
nothing(Obj2):0 = no
x_pos(Obj3):0 = 3
y_pos(Obj3):0 = 0
x_size(Obj3):0 = 1
y_size(Obj3):0 = 1
colour(Obj3):0 = wall
shape(Obj3):0 = square
nothing(Obj3):0 = no
x_pos(Obj4):0 = 4
y_pos(Obj4):0 = 0
x_size(Obj4):0 = 1
y_size(Obj4):0 = 1
colour(Obj4):0 = wall
shape(Obj4):0 = square
nothing(Obj4):0 = no
x_pos(Obj5):0 = 0
y_pos(Obj5):0 = 1
x_size(Obj5):0 = 1
y_size(Obj5):0 = 1
colour(Obj5):0 = wall
shape(Obj5):0 = square
nothing(Obj5):0 = no
x_pos(Obj6):0 = 4
y_pos(Obj6):0 = 1
x_size(Obj6):0 = 1
y_size(Obj6):0 = 1
colour(Obj6):0 = wall
shape(Obj6):0 = square
nothing(Obj6):0 = no
x_pos(Obj7):0 = 0
y_pos(Obj7):0 = 2
x_size(Obj7):0 = 1
y_size(Obj7):0 = 1
colour(Obj7):0 = wall
shape(Obj7):0 = square
nothing(Obj7):0 = no
x_pos(Obj8):0 = 3
y_pos(Obj8):0 = 2
x_size(Obj8):0 = 1
y_size(Obj8):0 = 1
colour(Obj8):0 = wall
shape(Obj8):0 = square
nothing(Obj8):0 = no
x_pos(Obj9):0 = 4
y_pos(Obj9):0 = 2
x_size(Obj9):0 = 1
y_size(Obj9):0 = 1
colour(Obj9):0 = wall
shape(Obj9):0 = square
nothing(Obj9):0 = no
x_pos(Obj10):0 = 0
y_pos(Obj10):0 = 3
x_size(Obj10):0 = 1
y_size(Obj10):0 = 1
colour(Obj10):0 = wall
shape(Obj10):0 = square
nothing(Obj10):0 = no
x_pos(Obj11):0 = 4
y_pos(Obj11):0 = 3
x_size(Obj11):0 = 1
y_size(Obj11):0 = 1
colour(Obj11):0 = wall
shape(Obj11):0 = square
nothing(Obj11):0 = no
x_pos(Obj12):0 = 0
y_pos(Obj12):0 = 4
x_size(Obj12):0 = 1
y_size(Obj12):0 = 1
colour(Obj12):0 = wall
shape(Obj12):0 = square
nothing(Obj12):0 = no
x_pos(Obj13):0 = 1
y_pos(Obj13):0 = 4
x_size(Obj13):0 = 1
y_size(Obj13):0 = 1
colour(Obj13):0 = wall
shape(Obj13):0 = square
nothing(Obj13):0 = no
x_pos(Obj14):0 = 2
y_pos(Obj14):0 = 4
x_size(Obj14):0 = 1
y_size(Obj14):0 = 1
colour(Obj14):0 = wall
shape(Obj14):0 = square
nothing(Obj14):0 = no
x_pos(Obj15):0 = 3
y_pos(Obj15):0 = 4
x_size(Obj15):0 = 1
y_size(Obj15):0 = 1
colour(Obj15):0 = wall
shape(Obj15):0 = square
nothing(Obj15):0 = no
x_pos(Obj16):0 = 4
y_pos(Obj16):0 = 4
x_size(Obj16):0 = 1
y_size(Obj16):0 = 1
colour(Obj16):0 = wall
shape(Obj16):0 = square
nothing(Obj16):0 = no
x_pos(Obj17):0 = 1
y_pos(Obj17):0 = 3
x_size(Obj17):0 = 1
y_size(Obj17):0 = 1
colour(Obj17):0 = hole
shape(Obj17):0 = square
nothing(Obj17):0 = no
x_pos(Obj18):0 = 3
y_pos(Obj18):0 = 3
x_size(Obj18):0 = 1
y_size(Obj18):0 = 1
colour(Obj18):0 = goal
shape(Obj18):0 = square
nothing(Obj18):0 = no
x_pos(Obj19):0 = 2
y_pos(Obj19):0 = 2
x_size(Obj19):0 = 1
y_size(Obj19):0 = 1
colour(Obj19):0 = agent
shape(Obj19):0 = square
nothing(Obj19):0 = no
