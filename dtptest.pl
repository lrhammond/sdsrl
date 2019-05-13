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

% :- use_module(library(tabling)).
% :- table(r//1).
%
0.3::rain.
0.5::wind.
% ?::left_0.
% ?::left_1.
% ?::left_2.
% ?::left_3.
% ?::left_4.
% ?::right_0.
% ?::right_1.
% ?::right_2.
% ?::right_3.
% ?::right_4.
% ?::none(0).
% ?::none(1).
% ?::none(2).
% ?::none(3).
% ?::none(4).

?::raincoat.
?::umbrella.

% left(0) :- left_0.
% right(0) :- right_0.
% left(1) :- left_1.
% right(1) :- right_1.
% left(2) :- left_2.
% right(2) :- right_2.
% left(3) :- left_3.
% right(3) :- right_3.
% left(4) :- left_4.
% right(4) :- right_4.
%
% ?::left(0); ?::right(0); ?::none(0).
% ?::left(1); ?::right(1); ?::none(1).
% ?::left(2); ?::right(2); ?::none(2).
% ?::left(3); ?::right(3); ?::none(3).
% ?::left(4); ?::right(4); ?::none(4).
% ?::left(5); ?::right(5); ?::none(5).

% :- dynamic r/1.

?::none(0); ?::left(0); ?::right(0).


?::none(1).
?::left(1).
?::right(1).
?::none(2).
?::left(2).
?::right(2).
?::none(2).
% ?::left(3).
?::right(3).
?::none(3).
?::left(4).
?::right(4).
?::none(4).

att(o1, 0, 0, square, 0).
att(o2, -1, 0, round, 0).

% clock(T1) :- clock(T0), T1 is T0 + 1, left(T0).
% clock(T1) :- clock(T0), T1 is T0 + 1, right(T0).


att(Obj, X1, Y, square, T1) :-  att(Obj, X, Y, square, T0),
                                T1 is T0 + 1,
                                X1 is X - 1,
                                % not(att(Obj2, X1, Y, round, T0)),
                                left(T0).

% att(Obj, X, Y, S, T1) :- att(Obj, X, Y, S, T0), T1 is T0 + 1, none(T0).

% r(0, 0).
% r(R1, T1) :- r(R0, T0), Curr is T0 + 1, R1 is R0 + .


r1(T) :- att(o1, -1, Y, S, T), R is 1000.
r3(T) :- att(o1, -3, Y, S, T), R is 100.
o(T) :- not(r1(T)), not(r3(T)).


% r(R0, T1) :- r(R0, T0), T1 is T0 + 1.

% (  X =< Y
%     -> Z = Y
%     ;  Z = X
%      )

% r(1) :- att(o1, -2, Y, S, 1).
% r(2) :- att(o1, -2, Y, S, 2).
% r(3) :- att(o1, -2, Y, S, 3).
% r(4) :- att(o1, -2, Y, S, 4).

%
% broken_umbrella :- left(X), rain, wind.
% dry :- rain, raincoat.
% dry :- rain, left(X), not broken_umbrella.
% dry :- not(rain).

broken_umbrella :- left(X).
% dry :- rain, raincoat.
% dry :- rain.
dry :- not(rain).

utility(r1(1), 100).
utility(r3(3), 1000).
utility(o(1), -500).
utility(o(2), -500).
% utility(r3(2), 1000).
% utility(r(2), 1000).
% utility(r(R, 4), R).
% utility(r(R, 4), R).
% utility(r(R, 4), R).
% utility(r(R, 4), R).
utility(raincoat, -20).
% utility(left_0, -2).
utility(dry, 60).
