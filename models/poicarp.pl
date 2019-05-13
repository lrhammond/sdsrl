% POIFECT

% Decisions
?::l(0); ?::r(0); ?::u(0); ?::d(0); ?::none(0).
?::l(1); ?::r(1); ?::u(1); ?::d(1); ?::none(1).
?::l(2); ?::r(2); ?::u(2); ?::d(2); ?::none(2).
?::l(3); ?::r(3); ?::u(3); ?::d(3); ?::none(3).
?::l(4); ?::r(4); ?::u(4); ?::d(4); ?::none(4).
% ?::l(5); ?::r(5); ?::u(5); ?::d(5); ?::none(5).
% ?::l(6); ?::r(6); ?::u(6); ?::d(6); ?::none(6).
% ?::l(7); ?::r(7); ?::u(7); ?::d(7); ?::none(7).
% ?::l(8); ?::r(8); ?::u(8); ?::d(8); ?::none(8).
% ?::l(9); ?::r(9); ?::u(9); ?::d(9); ?::none(9).


% Objects
att(obj0, 0, 0, small, small, wall, square, no, 0).
att(obj1, 1, 0, small, small, wall, square, no, 0).
att(obj2, 2, 0, small, small, wall, square, no, 0).
att(obj3, 3, 0, small, small, wall, square, no, 0).
att(obj4, 4, 0, small, small, wall, square, no, 0).
att(obj5, 0, 1, small, small, wall, square, no, 0).
att(obj6, 4, 1, small, small, wall, square, no, 0).
att(obj7, 0, 2, small, small, wall, square, no, 0).
att(obj8, 3, 2, small, small, wall, square, no, 0).
att(obj9, 4, 2, small, small, wall, square, no, 0).
att(obj10, 0, 3, small, small, wall, square, no, 0).
att(obj11, 4, 3, small, small, wall, square, no, 0).
att(obj12, 0, 4, small, small, wall, square, no, 0).
att(obj13, 1, 4, small, small, wall, square, no, 0).
att(obj14, 2, 4, small, small, wall, square, no, 0).
att(obj15, 3, 4, small, small, wall, square, no, 0).
att(obj16, 4, 4, small, small, wall, square, no, 0).
att(obj17, 1, 3, small, small, hole, square, no, 0).
att(obj18, 3, 3, small, small, goal, square, no, 0).
att(obj19, 2, 1, small, small, agent, square, no, 0).

% No object
% att(no_object, X_pos, Y_pos, X_size, Y_size, Colour, Shape, yes, T1) :- not(att(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, no, T0)).

% Attribute schemas
% att(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, Colour, Shape, Nothing, T0), T1 is T0 + 1, none(T0).
% att(Obj, New_X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), T1 is T0 + 1, New_X_pos is X_pos + 1, Nb4_X_pos is X_pos + 1, Nb4_Y_pos is Y_pos, att(Nb4, Nb4_X_pos, Nb4_Y_pos, X_size, Y_size, Colour, Shape, yes, T0), r(T0).
% att(Obj, New_X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), att(Nb8, Nb8_X_pos, Nb8_Y_pos, X_size, Y_size, Colour, Shape, yes, T0), T1 is T0 + 1, l(T0), New_X_pos is X_pos - 1, Nb8_X_pos is X_pos - 1, Nb8_Y_pos is Y_pos.
% att(Obj, X_pos, New_Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), att(Nb2, Nb2_X_pos, Nb2_Y_pos, X_size, Y_size, Colour, Shape, yes, T0), T1 is T0 + 1, u(T0), New_Y_pos is X_pos + 1, Nb2_X_pos is X_pos, Nb2_Y_pos is Y_pos + 1.
% att(Obj, X_pos, New_Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), att(Nb6, Nb6_X_pos, Nb6_Y_pos, X_size, Y_size, Colour, Shape, yes, T0), T1 is T0 + 1, d(T0), New_Y_pos is X_pos - 1, Nb6_X_pos is X_pos, Nb6_Y_pos is Y_pos - 1.
att(Obj, New_X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), T1 is T0 + 1, New_X_pos is X_pos + 1, Nb4_X_pos is X_pos + 1, Nb4_Y_pos is Y_pos, r(T0).
att(Obj, New_X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), T1 is T0 + 1, l(T0), New_X_pos is X_pos - 1, Nb8_X_pos is X_pos - 1, Nb8_Y_pos is Y_pos.
att(Obj, X_pos, New_Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), T1 is T0 + 1, u(T0), New_Y_pos is X_pos + 1, Nb2_X_pos is X_pos, Nb2_Y_pos is Y_pos + 1.
att(Obj, X_pos, New_Y_pos, X_size, Y_size, agent, Shape, N, T1) :- att(Obj, X_pos, Y_pos, X_size, Y_size, agent, Shape, N, T0), T1 is T0 + 1, d(T0), New_Y_pos is X_pos - 1, Nb6_X_pos is X_pos, Nb6_Y_pos is Y_pos - 1.


% Reward schemas
r1(T) :- att(Obj, 3, 3, X_size, Y_size, agent, Shape, N, T).
r2(T) :- att(Obj, 1, 3, X_size, Y_size, agent, Shape, N, T).
no_r(T0) :- not(r1(T)), not(r2(T)).

% Utility scores
utility(r1(0), 10 * 0.9 ** 0).
utility(r1(1), 10 * 0.9 ** 1).
utility(r1(2), 10 * 0.9 ** 2).
utility(r1(3), 10 * 0.9 ** 3).
utility(r1(4), 10 * 0.9 ** 4).
utility(r1(5), 10 * 0.9 ** 5).
utility(r1(6), 10 * 0.9 ** 6).
utility(r1(7), 10 * 0.9 ** 7).
utility(r1(8), 10 * 0.9 ** 8).
utility(r1(9), 10 * 0.9 ** 9).
utility(r2(0), -10 * 0.9 ** 0).
utility(r2(1), -10 * 0.9 ** 1).
utility(r2(2), -10 * 0.9 ** 2).
utility(r2(3), -10 * 0.9 ** 3).
utility(r2(4), -10 * 0.9 ** 4).
utility(r2(5), -10 * 0.9 ** 5).
utility(r2(6), -10 * 0.9 ** 6).
utility(r2(7), -10 * 0.9 ** 7).
utility(r2(8), -10 * 0.9 ** 8).
utility(r2(9), -10 * 0.9 ** 9).
utility(no_r(0), -1 * 0.9 ** 0).
utility(no_r(1), -1 * 0.9 ** 1).
utility(no_r(2), -1 * 0.9 ** 2).
utility(no_r(3), -1 * 0.9 ** 3).
utility(no_r(4), -1 * 0.9 ** 4).
utility(no_r(5), -1 * 0.9 ** 5).
utility(no_r(6), -1 * 0.9 ** 6).
utility(no_r(7), -1 * 0.9 ** 7).
utility(no_r(8), -1 * 0.9 ** 8).
utility(no_r(9), -1 * 0.9 ** 9).
