%%% -*- Mode: Prolog; -*-
% Copyright 2014, Davide Nitti, KU Leuven. All rights reserved.

%isprivate.
:- use_module('dcpf.pl').
:- use_module('random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(charsio)).

% Sparse Sampling Algorithm
start_sst(Discount,Obs,C,Depth,Delta,TotalReward,BestAction) :-
	init_particle(1),
	dcpf:step_particle([],Obs,[],1,Delta),
%	plotdata(1),
	step_sst(Discount,C,Depth,Delta,TotalReward,BestAction,RA).
/*	bb_get(dcpf:offset,Offset),I is Offset+1,
	eraseall(0),
	dcpf:plaincopyparticles(I,0),
	sst(C,0,Depth,Delta,TotalReward,BestAction,RA).
*/
step_sst(Discount,C,Depth,Delta,TotalReward,BestAction,RA) :-
	bb_get(dcpf:offset,Offset),I is Offset+1,
	eraseall(0),
	dcpf:plaincopyparticles(I,0),
	sst(Discount,C,0,Depth,Delta,TotalReward,BestAction,RA).

sst(Discount,C,Key,0,Delta,0,null,0) :- !.
sst(Discount,C,Key,Depth,Delta,TotalReward,BestAction,RA) :-
%	plotplanning(Key),
	term_to_atom(maxr(Key),MaxRKey),
	term_to_atom(maxaction(Key),MaxactionKey),
	term_to_atom(sumsst(Key),SumsstKey),
	abolish_all_tables,
%	findall(action(move(A,B)),(between(1,2,_),sample(gaussian([0,0],[0.004,0,0,0.004]),(A,B))),ListActions), % hardcoded action, to change
%	findall(action(move(A,B)),(member((X,Y),[(-1,0),(1,0),(0,-1),(0,1)]),A is X/10,B is Y/10),ListActions), % hardcoded action, to change
%	findall(action(move(A,B)),(member(X,[-1,1]),member(Y,[-1,1]),A is X/20,B is Y/20),ListActions), % hardcoded action, to change
%	dcpf:findall(action(A),(distributionalclause:proof_query_backward(Key,tempparticle,current(adm(action(A))))),ListActions),
	eraseall(tempparticle),
	distributionalclause:proof_query_backward(Key,tempparticle,findall_forward(A,current(adm(A)),ListActions)),
	%writeln(ListActions),
	!,

	(
		(dcpf:proof_query_backward(Key,current(stop))) ->
			(
				abolish_all_tables,
				eraseall(tmp),
				dcpf:proof_query_backward(Key,tmp,current(reward) ~= RA),
				TotalReward=RA,
				BestAction=null
				%writeln(stop)
			)
			;
			(
				bb_put(MaxRKey,(-inf)),
				(
				member(Action,ListActions), % for each action
				abolish_all_tables,
				eraseall(tmp),
				dcpf:assert_list(tmp,[Action]),
				dcpf:proof_query_backward(Key,tmp,current(reward) ~= R),

				%writeln(Key),printkeyp(tmp),nl,nl,printkeyp(Key),
				bb_put(SumsstKey,0.0),
				(
					between(1,C,Sample), % C samples for each action "Action"
					KeyNew is Key+1,
					(
						% sample next state
						(
						eraseall(KeyNew),
						eraseall(tempsst),
						dcpf:plaincopyparticles(Key,tempsst),

						retractall(user:deltaT(_)),
						assert(user:deltaT(Delta)),
						retractall(user:timestep(_)),
						assert(user:timestep(KeyNew)),
						dcpf:abolish_all_tables,
						dcpf:assert_list(tempsst,[Action]), % assert Actions
						% add here reward that depends on action
						% proof_query_backward(Key,current(reward) ~= RA),
						(
						inference(backward(_)) ->
							dcpf:inferencestep_particlefilter_backward2(tempsst)
						;
							writeln('error, only backward inference supported')%generate_sample_particlefilter(T,MaxP)
						),
						NewDepth is Depth-1,
						dcpf:copyparticles_core(tempsst,KeyNew), % t+1 -> t
						eraseall(tempsst),
						sst(Discount,C,KeyNew,NewDepth,Delta,RewardS,_,_), % recursive call
						bb_get(SumsstKey,SR),
						NewSR is SR + RewardS,
						bb_put(SumsstKey,NewSR)
						) -> true % execute as 1 block
					),

					fail;
					true
				),
				bb_get(SumsstKey,TotS),
				AvgR is R+Discount*TotS/C,
				(
				Key==0 ->
				writeln((Action,AvgR));
				true

				),
				bb_get(MaxRKey,MaxR),
				(
					AvgR>MaxR ->
					(
						bb_put(MaxRKey,AvgR),
						bb_put(MaxactionKey,[(Action,R)])
					)
					;
					(
					AvgR=MaxR ->
						(
						bb_get(MaxactionKey,ListBest),
						bb_put(MaxactionKey,[(Action,R)|ListBest])
						)
					;
						true
					)
				),
				fail;
				true
				),
				bb_delete(MaxactionKey,ListBestA),
				dcpf:sample(uniform(ListBestA),(BestAction,RA)),
				bb_delete(MaxRKey,TotalReward)
				%TotalReward is RA+Discount*FutureR
			)
	),!.
