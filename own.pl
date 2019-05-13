:- use_module(library(clpfd)).

1.00::x_pos(Obj, New, 1) :- x_pos(Obj, Old, 0), right, New is Old + 1.
1.00::x_pos(Obj, New, 1) :- x_pos(Obj, Old, 0), left, New is Old - 1.
1.00::y_pos(Obj, New, 1) :- y_pos(Obj, Old, 0), up, New is Old + 1.
1.00::y_pos(Obj, New, 1) :- y_pos(Obj, Old, 0), down, New is Old - 1.
