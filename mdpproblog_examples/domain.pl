




% network topology properties
% accTotal([],A,A).
% accTotal([_|T],A,X) :- B is A+1, accTotal(T,B,X).
% total(L,T) :- accTotal(L,0,T).
% total_connected(C,T) :-
%                  connected(C,L), % L = list of computers connected to computer C
%                  total(L,T).     % T = total number of computers in list L
%
% accAlive([],A,A).
% accAlive([H|T],A,X) :- running(H,0), B is A+1, accAlive(T,B,X).
% accAlive([H|T],A,X) :- not(running(H,0)), B is A, accAlive(T,B,X).
% alive(L,A) :- accAlive(L,0,A).
% total_running(C,R) :-
%                 connected(C,L), % L = list of computers connected to computer C
%                 alive(L,R).     % R = total number of running computers in list L

% state fluents
% state_fluent(running(C)) :- computer(C).
state_fluent(x_pos(Obj, X_pos)) :- is_obj(Obj), integer(X_pos).
state_fluent(y_pos(Obj, Y_pos)) :- is_obj(Obj), integer(Y_pos).
state_fluent(shape(Obj, Shape)) :- is_obj(Obj), is_shape(Shape).
% state_fluent(attributes(Obj, X, Y, S)) :- x_pos(Obj, X), y_pos(Obj, Y), shape(Obj, S).
% state_fluent(r1(Obj)) :- x_pos(Obj, X), X >= 2.

% actions
% action(reboot(C)) :- computer(C).
% action(reboot(none)).
action(right).
action(left).
action(up).
action(down).
action(none).

% transition model
1.00::x_pos(Obj, New, 1) :- x_pos(Obj, Old, 0), right, New is Old + 1.
1.00::x_pos(Obj, New, 1) :- x_pos(Obj, Old, 0), left, New is Old - 1.
1.00::y_pos(Obj, New, 1) :- y_pos(Obj, Old, 0), up, New is Old + 1.
1.00::y_pos(Obj, New, 1) :- y_pos(Obj, Old, 0), down, New is Old - 1.
% 1.00::shape(Obj, round, 1) :- shape(Obj, square, 0), down.

% utility attributes

% costs
utility(right, -1).
utility(left, -1).
utility(up, -1).
utility(down, -1).
utility(none, 0).

% rewards
utility(shape(Obj, round, 0), 10) :- is_obj(Obj).
