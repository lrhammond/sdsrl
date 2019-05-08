?::left_0.
?::right_0.
?::left_1.
?::right_1.
?::left_2.
?::right_2.

left(0) :- left_0.
right(0) :- right_0.
left(1) :- left_1.
right(1) :- right_1.
left(2) :- left_2.
right(2) :- right_2.

att(o1, 0, 0, square, 0).

att(Obj, X1, Y, S, T1) :- att(Obj, X, Y, S, T0),
                          T1 is T0 + 1,
                          X1 is X - 1,
                          left(T0).

att(Obj, X1, Y, S, T1) :- att(Obj, X, Y, S, T0),
                          T1 is T0 + 1,
                          X1 is X + 1,
                          right(T0).

isobj(o1).

r :- isobj(Obj), att(Obj, 2, Y, S, T).

utility(r, 10).
utility(not(r), -1).


% 0.3::rain.
% 0.5::wind.
% ?::left_0.
% ?::raincoat.
%
% left(0) :- left_0.
% r :- left(X).
%
% broken_umbrella :- left(X), rain, wind.
% dry :- rain, raincoat.
% dry :- rain, left_0, not broken_umbrella.
% dry :- not(rain).
%
% utility(r, -1000).
% utility(raincoat, -20).
% utility(left_0, -2).
% utility(dry, 60).
