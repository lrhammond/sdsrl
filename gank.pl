%
%
%
% att(o1, 0, 0, square, 0).
%
% att(Obj, X1, Y, S, T1) :- att(Obj, X, Y, S, T0),
%                           T1 is T0 + 1,
%                           X1 is X - 1,
%                           left(T0).
%
% att(Obj, X1, Y, S, T1) :- att(Obj, X, Y, S, T0),
%                           T1 is T0 + 1,
%                           X1 is X + 1,
%                           right(T0).
%
% isobj(o1).
%
% r :- isobj(Obj), att(Obj, 2, Y, S, T).
%
% utility(r, 10).
% utility(not(r), -1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

0.3::rain.
0.5::wind.
?::left_0.
?::right_0.
?::left_1.
?::right_1.
?::left_2.
?::right_2.
?::left_3.
?::right_3.
?::left_4.
?::right_4.
?::raincoat.
?::umbrella.

left(0) :- left_0.
right(0) :- right_0.
left(1) :- left_1.
right(1) :- right_1.
left(2) :- left_2.
right(2) :- right_2.
left(3) :- left_3.
right(3) :- right_3.
left(4) :- left_4.
right(4) :- right_4.

att(o1, 0, 0, square, 0).
att(o2, -1, 0, round, 0).

att(Obj, X1, Y, square, T1) :-  att(Obj, X, Y, square, T0),
                                T1 is T0 + 1,
                                X1 is X - 1,
                                % not(att(Obj2, X1, Y, round, T0)),
                                left(T0).

r(0) :- att(o1, -2, Y, S, T).
r(1) :- att(o1, -2, Y, S, T).
r(2) :- att(o1, -2, Y, S, T).
r(3) :- att(o1, -2, Y, S, T).

broken_umbrella :- left(X), rain, wind.
dry :- rain, raincoat.
dry :- rain, left(X), not broken_umbrella.
dry :- not(rain).

utility(r(0), 1000).
utility(r(1), 1000).
utility(r(2), 1000).
utility(r(3), 1000).
utility(raincoat, -20).
% utility(left_0, -2).
utility(dry, 60).
