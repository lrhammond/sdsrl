% POIFECT

% Decisions
?::l(0); ?::r(0); ?::none(0).
?::l(1); ?::r(1); ?::none(1).
?::l(2); ?::r(2); ?::none(2).
?::l(3); ?::r(3); ?::none(3).
?::l(4); ?::r(4); ?::none(4).
% ?::l(5); ?::r(5); ?::none(5).
% ?::l(6); ?::r(6); ?::u(6); ?::d(6); ?::none(6).
% ?::l(7); ?::r(7); ?::u(7); ?::d(7); ?::none(7).
% ?::l(8); ?::r(8); ?::u(8); ?::d(8); ?::none(8).
% ?::l(9); ?::r(9); ?::u(9); ?::d(9); ?::none(9).


% Objects
att(obj0, 0, 0, wall, no, 0).
att(obj1, 1, 0, agent, no, 0).
att(obj2, 4, 0, wall, no, 0).

att(no_object, 2, 0, none, yes, 0).
att(no_object, 3, 0, none, yes, 0).
att(no_object, 5, 0, none, yes, 0).

% att(no_object, X, Y, None, yes, 0) :- not(Obj = no_object), not(att(Obj, X, Y, None, no, 0)).

% No object
% att(no_object, X_pos, Y_pos, X_size, Y_size, Colour, Shape, yes, T1) :- not(att(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, no, T0)).

% Attribute schemas
% att(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing, T0), T1 is T0 + 1, none(T0).
% att(Obj, New_X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), T1 is T0 + 1, New_X_pos is X_pos + 1, Nb4_X_pos is X_pos + 1, Nb4_Y_pos is Y_pos, att(Nb4, Nb4_X_pos, Nb4_Y_pos, X_size, Y_size, Colour, Shape, yes, T0), r(T0).
% att(Obj, New_X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), att(Nb8, Nb8_X_pos, Nb8_Y_pos, X_size, Y_size, Colour, Shape, yes, T0), T1 is T0 + 1, l(T0), New_X_pos is X_pos - 1, Nb8_X_pos is X_pos - 1, Nb8_Y_pos is Y_pos.
% att(Obj, X_pos, New_Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), att(Nb2, Nb2_X_pos, Nb2_Y_pos, X_size, Y_size, Colour, Shape, yes, T0), T1 is T0 + 1, u(T0), New_Y_pos is X_pos + 1, Nb2_X_pos is X_pos, Nb2_Y_pos is Y_pos + 1.
% att(Obj, X_pos, New_Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), att(Nb6, Nb6_X_pos, Nb6_Y_pos, X_size, Y_size, Colour, Shape, yes, T0), T1 is T0 + 1, d(T0), New_Y_pos is X_pos - 1, Nb6_X_pos is X_pos, Nb6_Y_pos is Y_pos - 1.

% att(Obj, New_X_pos, Y_pos, agent, N, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), T1 is T0 + 1, New_X_pos is X_pos + 1, Nb4_X_pos is X_pos + 1, Nb4_Y_pos is Y_pos, r(T0).
% att(Obj, New_X_pos, Y_pos, agent, N, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), T1 is T0 + 1, l(T0), New_X_pos is X_pos - 1, Nb8_X_pos is X_pos - 1, Nb8_Y_pos is Y_pos.


% att(Obj, New_X_pos, Y_pos, agent, N, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), T1 is T0 + 1, New_X_pos is X_pos + 1, r(T0), att(Nb, X_nb, Y_nb, Colour, yes, T0), X_nb is X_pos + 1, Y_nb is Y_pos.
% att(no_object, X_pos, Y_pos, _, yes, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), T1 is T0 + 1, New_X_pos is X_pos + 1, r(T0), att(Nb, X_nb, Y_nb, Colour, yes, T0), X_nb is X_pos + 1, Y_nb is Y_pos.

% att(no_object, X_pos, Y_pos, none, yes, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), r(T0), T1 is T0 + 1, New_X_pos is X_pos + 1, Nb4_X_pos is X_pos + 1, Nb4_Y_pos is Y_pos, att(Nb4, Nb4_X_pos, Nb4_Y_pos, Colour, yes, T0).
% att(no_object, X_pos, Y_pos, none, yes, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), l(T0), T1 is T0 + 1, New_X_pos is X_pos - 1, Nb8_X_pos is X_pos - 1, Nb8_Y_pos is Y_pos, att(_, Nb8_X_pos, Nb8_Y_pos, _, yes, T0).




att(Obj, New_X_pos, Y_pos, agent, N, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), r(T0), T1 is T0 + 1, New_X_pos is X_pos + 1, att(no_object, New_X_pos, Y_pos, none, yes, T0).
att(Obj, New_X_pos, Y_pos, agent, N, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), l(T0), T1 is T0 + 1, New_X_pos is X_pos - 1.



att(no_object, X_pos, Y_pos, none, yes, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), r(T0), T1 is T0 + 1, att(no_object, New_X_pos, Y_pos, none, yes, T0).
att(no_object, X_pos, Y_pos, none, yes, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), l(T0), T1 is T0 + 1.


att(Obj, X_pos, Y_pos, Colour, N, T1) :- att(Obj, X_pos, Y_pos, Colour, N, T0), T1 is T0 + 1, none(T0).

%
% clock(0).
%
% clock(T1) :- between(0, 5, T0), l(T0), T1 is T0 + 1.
% clock(T1) :- between(0, 5, T0), r(T0), T1 is T0 + 1.
% clock(T1) :- between(0, 5, T0), none(T0), T1 is T0 + 1.
% not(clock(T0)) :- l(T0).
% not(clock(T0)) :- r(T0).
% not(clock(T0)) :- none(T0).


% att(no_object, X_pos, Y_pos, _, yes, T1)


% att(Obj, New_X_pos, Y_pos, agent, N, T1) :- att(Obj, X_pos, Y_pos, agent, N, T0), T1 is T0 + 1, l(T0), New_X_pos is X_pos - 1.

% att(Obj, X_pos, New_Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), T1 is T0 + 1, u(T0), New_Y_pos is X_pos + 1, Nb2_X_pos is X_pos, Nb2_Y_pos is Y_pos + 1.
% att(Obj, X_pos, New_Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), T1 is T0 + 1, d(T0), New_Y_pos is X_pos - 1, Nb6_X_pos is X_pos, Nb6_Y_pos is Y_pos - 1.


% Reward schemas
r1(T) :- att(Obj, 3, 0, agent, N, T), Obj \= no_object.
r2(T) :- att(Obj, 4, 0, agent, N, T), Obj \= no_object.
no_r(T) :- not(r1(T)), not(r2(T)).

% Utility scores
utility(r1(0), 100).
utility(r1(1), 100).
utility(r1(2), 100).
utility(r1(3), 100).
utility(r1(4), 100).
utility(r1(5), 100).
% utility(r1(6), 10 * 0.9 ** 6).
% utility(r1(7), 10 * 0.9 ** 7).
% utility(r1(8), 10 * 0.9 ** 8).
% utility(r1(9), 10 * 0.9 ** 9).
utility(r2(0), 1000).
utility(r2(1), 1000).
utility(r2(2), 1000).
utility(r2(3), 1000).
utility(r2(4), 1000).
utility(r2(5), 1000).
% utility(r2(6), -10 * 0.9 ** 6).
% utility(r2(7), -10 * 0.9 ** 7).
% utility(r2(8), -10 * 0.9 ** 8).
% utility(r2(9), -10 * 0.9 ** 9).
utility(no_r(0), -1).
utility(no_r(1), -1).
utility(no_r(2), -1).
utility(no_r(3), -1).
utility(no_r(4), -1).
utility(no_r(5), -1).
% utility(no_r(6), -1 * 0.9 ** 6).
% utility(no_r(7), -1 * 0.9 ** 7).
% utility(no_r(8), -1 * 0.9 ** 8).
% utility(no_r(9), -1 * 0.9 ** 9).
