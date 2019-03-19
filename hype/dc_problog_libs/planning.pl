%%% -*- Mode: Prolog; -*-
% Copyright 2014, Davide Nitti, KU Leuven. All rights reserved.
%isprivate.
:- use_module('dcpf.pl').
:- use_module('random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(sst).
% fixme delta default=1!! correct this
user:builtin(noise) :- !.

findsoftmaxaction(WHeu,Epsilon,PBRW,Abstract,Depth,MaxDepth,Discount,Episode,T,ActionList,Bestaction,AverageBestAction,VarBestAction,SelectedAction,AverageBestAction,VarBestAction,[],0) :-
	bb_put(maxr,(-inf)),
	bb_put(bestaction,[]),
	bb_put(exploreaction,[]),

	findall(NR:(Action,Average,Var),
	(member(Action,ActionList),evaluateaction(WHeu,PBRW,Abstract,Depth,MaxDepth,Discount,Episode,T,Action,NR,Average,Var,Explore)),List),

	distributionalclause:findmax(Max:(Bestaction,AverageBestAction,VarBestAction),List),
	distributionalclause:findmin(Min:(_,AverageWorstAction,_),List),
%	writeln((Max,Min)),
	findall(Exp:(Action,Average,Var),(member(NR:(Action,Average,Var),List),Exp is e^((NR-Min)/max(0.1,(Max-Min))/(Epsilon*10+0.1))),List2),
	distributionalclause:samplepos(propfinite(List2),(SelectedAction,AverageSelectedAction,VarSelectedAction),_),
%	writeln((SelectedAction,Bestaction,List2)),
%	writeln((Max:(Bestaction,AverageBestAction,VarBestAction),List)),
	!.

findbestaction(WHeu,PBRW,Abstract,Depth,MaxDepth,Discount,Episode,T,ActionList,Bestaction,AverageBestAction,VarBestAction,RandomAction,AverageRandomAction,VarRandomAction,ExplorationAction,LE) :-
	bb_put(maxr,(-inf)), % max reward
	bb_put(bestaction,[]),
	bb_put(exploreaction,[]),

	dcpf:sample(uniform(ActionList),RandomAction),
	(
		member(Action,ActionList),
		(

		evaluateaction(WHeu,PBRW,Abstract,Depth,MaxDepth,Discount,Episode,T,Action,NR,Average,Var,Explore),
		(
		Action==RandomAction ->
			bb_put(averagerandomaction,(Average,Var))
			;
			true
		),
%		writeln((NR,Average)),
		(Explore==true ->
		(
			bb_get(exploreaction,OldACT),
			bb_put(exploreaction,[(Action,Average)|OldACT])
		)
		;
		(
		bb_get(maxr,MR),
		(
			NR-MR>0.00000001 -> 	% NR>MR ->
			(
				bb_put(maxr,NR),
				bb_put(bestaction,[(Action,Average,Var)])
			)
			;
			(
				abs(NR-MR)<0.00000001 ->  	% NR==MR ->
				(
				bb_get(bestaction,ACT),
				bb_put(bestaction,[(Action,Average,Var)|ACT])
				)
				;
				true
			)
		)
		)
		)
		),fail;true
	),
	bb_delete(bestaction,BestactionList),
	bb_delete(exploreaction,ExploreactionList),

	(
	ExploreactionList==[] ->
		ExplorationAction=[]
		;
		dcpf:sample(uniform(ExploreactionList),(ExplorationAction,AvgExp))
	),
	(
	BestactionList==[] ->
		(
			Bestaction=ExplorationAction,
			AverageBestAction=AvgExp,
			VarBestAction=(+inf)
		)
		;
		dcpf:sample(uniform(BestactionList),(Bestaction,AverageBestAction,VarBestAction))
	),
%	writeln((Bestaction,ExplorationAction)),
	bb_delete(maxr,_),
	length(ExploreactionList,LE),
	bb_delete(averagerandomaction,(AverageRandomAction,VarRandomAction)),
%	writeln((RandomAction,AverageRandomAction)),
	!.

% Depth is not the max depth! Q(s,a)
evaluateaction(WHeu,PBRW,Abstract,Depth,MaxDepth,Discount,Episode,T,Action,NormR,Average,VarWis,Explore) :-
	Limit is Episode-1,
	bb_put(averagereward,0.0),
	bb_put(sumweight,0.0),
	bb_put(currentQ,0),
	bb_put(numsamples,0),
	bb_get(user:limit_previous_episodes,LPE),
	bb_get(user:max_horizon_span,MHSpan),
	bb_get(user:decay,Alpha),
	bb_get(user:pruning,Pruning),
	bb_get(user:spant,Spant),
	WeightDepth is 1,%max(0,1- (0.5+1*(MaxDepth-Depth)/max(MaxDepth-1,1))),
%	AA is round(LPE*WeightDepth),
%writeln(WeightDepth),
	Considered is min(round(LPE*WeightDepth+PBRW*0),Limit),
%	writeln((LPE,Considered,Depth,AA)),
	term_to_atom(maxl(Depth),KeyMaxLikelihood), % Key MaxLikelihood
	(bb_get(KeyMaxLikelihood,MaxLikelihood) -> true;MaxLikelihood=0),

	term_to_atom(p(Depth,Episode),CurrentKey),
	(% remove actions in the current Key state
		recorded(CurrentKey,action(_),RR),
		erase(RR)%,
		%fail
		;true
	),
	recorda(CurrentKey,Action,RefAction), % add action to evaluate in the current Key state
	%printkeyp(CurrentKey),
	eraseall(tempparticle4),
	abolish_all_tables,
	% FIXME warning!! the facts to prove the reward are temporarely! with abstraction it might not work
	(distributionalclause:query_proof(CurrentKey,current(reward) ~= _) -> (writeln('error: reward already defined'),halt);true),
	(distributionalclause:proof_query_backward(CurrentKey,tempparticle4,current(reward) ~= R) -> true; (writeln('error: reward not defined'),printkeyp(CurrentKey),fail) ),
	%term_to_atom(p(Depth),KeyMinProp), % key min proposal likelihood q
%	user:maxV(Depth,MaxVD), % upper bound for each Depth
	(distributionalclause:proof_query_backward(CurrentKey,tempparticle4,current(maxV(Depth,MaxVD)))-> true;(writeln('error: maxV not defined'),halt)),
%	writeln(MaxVD:Action),
	WHeu1 is max(0,WHeu),
	bb_get(user:discretepruning,DomainPruning),

	(Depth>1 ->
	(
	%findall(Weight:V,(
	forall((
		between(1,Considered,CI),
		bb_get(sumweight,OldSum),
%		LimitSearch is OldSum/(SelectW/PropLikelihood*MaxLikelihood*(Considered-CI)),
%		LimitSearch2 is (1-(1-Alpha)^(Considered-1))/Alpha,
%		LimitSearch3 is OldSum - ( (1-(1-Alpha)^(Considered-1))/Alpha-(1-(1-Alpha)^(CI-1))/Alpha ) + 1/10^10,
		%(Pruning==(+inf);MaxLikelihood==0;CI<10;SelectW/PropLikelihood*MaxLikelihood*(Considered-CI)>0.01*OldSum),
		pruning(Pruning,OldSum,_,CI),%(Pruning==(+inf);OldSum<Pruning;CI<10;LimitSearch3<0;OldSum/LimitSearch3>2),
		%writeln(OldSum),
		I is Episode-CI, % for each previous episode I (the last Considered eisodes)
		SelectW is (1-Alpha)^(max(0,Limit-I-PBRW*0)), % equivalent to alpha MC (Reinforcement learning: An introduction pag. 34)
		term_to_atom(p(0,I),TempKey), % info about stored episode at position 0
		(recorded(TempKey,terminated(Terminated,DD,ST),_) -> true;(Terminated=false) ),
		(Terminated==true ->
		(
			%Depth>=ST-1,
			%Span is round(MaxDepth)
			%Spant is 0, % max span for terminated episodes
			LowT is max(DD-1,Depth-Spant),% LowT is DD-1,%max(0,Depth-Span),
			MaxTT is min(MaxDepth,DD+Depth+1),
			HiT is min(MaxTT,Depth+Spant),% HiT is MaxDepth,
			IsTerminated is 1%max(1,MaxDepth-(DD-1))
			%writeln(terminated(Terminated,DD,ST,LowT,HiT))
		)
		;
		(
			%Span=0
			LowT is max(0,Depth-MHSpan),
			HiT is min(MaxDepth,Depth+MHSpan),
			IsTerminated=1
		)
		),
%		MinCheck is  max(1,Episode-Considered),

		%writeln((DD,TempKey,Terminated,Span)),
		%LowT is max(0,DD-Span),
		%HiT is min(MaxDepth,DD+Span),
		%(Terminated==true -> writeln((Terminated,LowT,HiT,Depth));true),
		between(LowT,HiT,TT), %T=TT,% infinite/finite horizon FIXME Depth is not the max depth!

%		(random<0.99^(Limit-I)),
		term_to_atom(p(TT,I),Key), % stored episode

%		eraseold(I,Episode,Key,TempKey,100),
		recorded(Key,v(next,V),RefV) % implicitly check if the point is active
		),
		((

%		recorded(Key,vheu(next,VHeu),_),
		% q(s_t+1) in the paper
		((recorded(Key,pointer(next,KeyP),_),recorded(KeyP,proposallikelihood(_,_,_),_))-> Key1=KeyP;Key1=Key),
		%writeln(3),
		(recorded(Key1,proposallikelihood(NumValues,SumPropLikelihood,PiProp),_) ->
		PropLikelihood is SumPropLikelihood/NumValues%*PiProp
		;
		(writeln('proposallikelihood not defined'),writeln(Key1),printkeyp(Key1),nl,writeln(Key),printkeyp(Key),writeln(CurrentKey),writeln(I is Episode-CI),halt)
		),
		%writeln(d(Depth,CI)),
		%writeln(proposallikelihood(Key,PropLikelihood,CI,NumValues)),
		%(random<1/PropLikelihood),




		(Depth==TT,recorded(Key,nextstatelikelihood(Action,_),RefNSL)->erase(RefNSL);true),
		(
		Abstract == true ->
		(compute_weight(CurrentKey,Key,Weight1,V,Action,TT,SelectW,P,PropLikelihood) ->
			true
			;
			(\+recorded(Key,nextstatelikelihood(Action,_),_),recorda(Key,nextstatelikelihood(Action,0),_),fail )
		)
		;
		(
		(recorded(Key,pointer(next,KeyP),_),recorded(KeyP,nextstatelikelihood(Action,P),_))->
		(
			%writeln(Weight1 is SelectW*P/PropLikelihood),
			Weight1 is SelectW*P/PropLikelihood
			/*
			(compute_weight_full(CurrentKey,Key,Weight11,V,Action,TT,SelectW,P11,PropLikelihood)-> true;Weight11=0),
			Difftemp is (P-P11)/P,
			(abs(Difftemp)>0.0001 ->
			(writeln(action(Action)),writeln(Key),printkeyp(Key),nl,writeln(KeyP),printkeyp(KeyP),writeln(diffp(P,P11)),halt)
			;true),
			writeln(diff(Weight1,Difftemp))*/
			%writeln(Weight1 is SelectW*P/PropLikelihood)
		)
		;
		(compute_weight_full(CurrentKey,Key,Weight1,V,Action,TT,SelectW,P,PropLikelihood,DomainPruning) ->
			(
				/*(compute_weight_full2(CurrentKey,Key,Weight11,V,Action,TT,SelectW,P11,PropLikelihood,DomainPruning)-> true;Weight11=0),
				Difftemp is (Weight1-Weight11)/Weight1,
				(abs(Difftemp)>0.0001 -> trace;true),
				writeln(diff(Weight1,Weight11,Difftemp)),
				*/

				true
			)
			;
			(\+recorded(Key,nextstatelikelihood(Action,_),_),recorda(Key,nextstatelikelihood(Action,0),_),fail )
		)
		)
		),
		(
		(Depth==TT,\+recorded(Key,nextstatelikelihood(Action,_),_)) ->
			(recorda(Key,nextstatelikelihood(Action,P),_)) % store p(s_t+1|s_t,a_t)
		;
			true
		),
		%writeln(Action:Weight1:V),
		Weight1>0,
		bb_get(user:proposal,Useproposal),
		((Useproposal==true,recorded(Key,vweight(next,Vweight),_)) -> true;Vweight=1),
		%OptimisticHeu is e^(V/MaxVD*WHeu1*500),
		%writeln(OptimisticHeu),
		Weight is Weight1/IsTerminated*Vweight,%*OptimisticHeu,
		Weight>0,
		NewSum is OldSum+Weight,
		bb_put(sumweight,NewSum),
		bb_get(currentQ,OldQ),
		%writeln(OldQ+Weight/NewSum*(V-OldQ)),
%		(WHeu>0->
%		NewQ is OldQ+Weight/NewSum*(VHeu-OldQ) % incremental weighted average
%		;
		NewQ is OldQ+Weight/NewSum*(V-OldQ),
%		),
		bb_put(currentQ,NewQ)

		);true)),%ListWV),

	%writeln(ListWV),
	% heuristic V
/*
	abolish_all_tables,
	%trace,
	distributionalclause:proof_query_backward(CurrentKey,current(heuristicV(Heuristic))),
	WHeu is 0.99,%^(Limit-1),
	%writeln(WHeu:Heuristic),
*/
	Depth2 is Depth-1,
%	user:maxV(Depth2,MaxV),
	(distributionalclause:proof_query_backward(CurrentKey,tempparticle4,current(maxV(Depth2,MaxV)))-> true;(writeln('error: maxV not defined'),halt)),
/*
	(
	(false,compute_weight_togoal(CurrentKey,WeightToGoal,VToGoal,Action)) ->
	(
		writeln(compute_weight_togoal(CurrentKey,WeightToGoal,VToGoal,Action)),
		writeln(ListWV),
		WeightToGoal2 is WeightToGoal,
		weightedaverage([WeightToGoal2:VToGoal|ListWV],Average1,SumW)
	)
	;
		true % weightedaverage([WHeu1:MaxV|ListWV],Average1,SumW)%,weightedaverage(ListWV,Average1,SumW) %
	), %weightedaverage([WHeu:Heuristic|ListWV],Average,SumW),
	% length(ListWV,LW),
*/
	bb_get(currentQ,Average2),
	bb_get(sumweight,SumW2),
	SumW is (SumW2+WHeu1),
	%writeln(SumW),
	(SumW>0 ->
		(
		Average3 is Average2+WHeu1/SumW*(MaxV-Average2)%,
		%writeln(Average3 is Average2+WHeu1/SumW*(MaxV-Average2))
		)
		;
		Average3=0
	),
	Average1 is Average3
	%(\+ground(Average) -> Average=0;true),
%	writeln(length(Depth,SumW,LW,Depth,Average1)),
%	writeln(weightedaverage([WHeu:MaxV|ListWV],Average1,SumW)),
%	writeln(SumW+WHeu+WHeu1),
	)
	;
	(SumW=0,Average1=0)
	),

	(
	((SumW+WHeu>=0,ground(Average1));Depth==1)
	->
	(
		%writeln(d2(Depth)),

		Average is R+Average1*Discount,
		%(variance_wis(ListWV,Average,VarIS,VarWis) -> true;(VarWis=10000,VarIS=VarWis,writeln('error variance_wis'))),
		% Depth==MaxDepth
		bb_get(user:ucbv,UCBV),
		((UCBV==true,Depth>1) ->
		(
			(variance_wis(ListWV,Average,VarIS,VarWis,VarW) ->
			(
				Explore=false,
				UB is sqrt(VarW)*WHeu1*20,
				NormR is Average+UB
			)
			;
				(VarWis=(+inf),NormR=MaxVD,UB=(+inf),Explore=false)%,writeln('error variance_wis')) %
		) %1*sqrt(VarWis*log(Episode))%+1*sqrt(log(MaxDepth*Episode)/SumW) %      sqrt(2*VarIS*log(MaxDepth*Episode)*LW/SumW) +10*log(MaxDepth*Episode)/SumW
 		)
 		;
 		(
	 		VarWis=0, % placeholder!
	 		NormR is Average,
	 		UB=0
 		)
 		)


	)
	;
	(%writeln(length(Depth,SumW,LW,Depth,Average1)),

		Explore=true,
	%	writeln(explore(CurrentKey,Depth,SumW)),
		%user:maxV(Depth,Average),
		Average is R+MaxV*Discount,
		VarWis=(+inf),
		NormR is Average,%user:maxV(Depth,NormR),
		UB=(+inf)
		%writeln(weightedaverage([WHeu:MaxV|ListWV],Average1,SumW))
		% heuristic V
		%abolish_all_tables,
		%distributionalclause:proof_query_backward(CurrentKey,current(heuristicV(NormR)))
		%writeln(NormR)
		%,
		%writeln(sumw(SumW))
	)
	),
	(\+ground(VarWis) -> VarWis=(+inf);true),
%	writeln(meanweights(NormR,VarWis)),
	%writeln(NormR is (FinalR+1)/(Sum+1)),

	(Depth>MaxDepth-1 ->
		(
		%writeln(time(T)),
		%printkey(CurrentKey),
		format("d ~d avg ~3f ~15+ bound ~4g ~15+ ub ~4g ~15+ n ~4g ",[Depth,Average,NormR,UB,SumW]),write(Action),nl
	%	writeln((varis(VarIS),varwis(VarWis)))

		)
		;true
	),
	erase(RefAction),
	/*(
		recorded(CurrentKey,current(reward) ~= _,RRew),
		erase(RRew)%,
		%fail
		;true
	),
	(
		recorded(CurrentKey,current(reward(_)) ~= _,RRew2),
		erase(RRew),
		fail
		;true
	),*/
	!.

pruning(Pruning,OldSum,LimitSearch3,CI) :- !,
	( Pruning==(+inf);OldSum<Pruning;CI<10),!.%;LimitSearch3<0;OldSum/LimitSearch3>2),!.

% problem with the q function, q might grow for duplicates
checkduplicates(D,Min,Episode,Alpha) :-
	term_to_atom(p(D,Episode),Key),
	Episode1 is Episode-1,

	forall(between(Min,Episode1,I),
	(
		term_to_atom(p(D,I),Key2),
%		writeln(Key2),
		(
		(recorded(Key,v(next,V),RefV),recorded(Key2,v(next,V2),RefV2),same(Key,Key2)) ->
		(/*
%			recorded(Key,v(next,V),RefV),
			(recorded(Key,vweight(next,Vweight1),RefVW) -> erase(RefVW); Vweight1=1),
			(recorded(Key2,vweight(next,Vweight2),RefVW2) -> erase(RefVW2); Vweight2=1),
			erase(RefV),
			erase(RefV2),
			Vweight is Vweight1+Vweight2*(1-Alpha)^(Episode-I),
			NewV is (Vweight1*V+V2*Vweight2*(1-Alpha)^(Episode-I))/Vweight,
			writeln((Key,Key2,NewV is (Vweight1*V+V2*Vweight2*(1-Alpha)^(Episode-I))/Vweight)),
			recorda(Key,v(next,NewV),_),
			recorda(Key,vweight(next,Vweight),_)
			*/
			% use pointers to avoid weight computation
			(recorded(Key2,pointer(next,KeyP),RefKeyP)->
			(
				(recorded(KeyP,pointer(next,_),RefKeyP2)-> erase(RefKeyP2);true),
				recorda(KeyP,pointer(next,Key),_),
				%writeln(recorda(KeyP,pointer(next,Key),_)),
				erase(RefKeyP)
			)
			;
			true
			),
			recorda(Key2,pointer(next,Key),_)%,
			%writeln(recorda(Key2,pointer(next,Key),_))

			/*
			%problem with the q function, q might grow for duplicates
			% update q(s_t+1): if a discrete state is duplicate, sum, used with user:proposal=false
			recorded(Key,proposallikelihood(Number,Proposallikelihood,Pi),RefProp),
			recorded(Key2,proposallikelihood(Number2,Proposallikelihood2,Pi2),RefProp2),
			NewProp is Proposallikelihood+Proposallikelihood2,
			erase(RefProp),
			recorda(Key,proposallikelihood(1,NewProp,Pi),_),
			writeln(newprop(NewProp))
			*/
		%	recorda(Key2,v(next,0),_),
		%	printkeyp(Key),nl,printkeyp(Key2)
		)
		;
		true
		)
	)),
	abolish_all_tables,!.

% valid only for full interpretations! for partial interpretations what is false needs to be checked
same(Key,Key2) :-
	forall((recorded(Key,next(Var2),_),Var2\=greedy(_)),recorded(Key2,next(Var2),_)),
	forall(recorded(Key,next(Var)~=Val,_),recorded(Key2,next(Var)~=Val,_)),!.
%	printkeyp(Key),
%	printkeyp(Key2).

eraseold(I,CurrentEpisode,Key,TempKey,N) :-
	((CurrentEpisode-I)>N,recorded(Key,v(next,V),RR),
	(recorded(TempKey,terminated(Terminated,_,_),_) -> true;Terminated=false),% (writeln('error terminated'),printkeyp(Key),halt)),
	(Terminated==false ->
	(
	erase(RR)
	);
	true %writeln(Key)
	));true,!.

/*
c_weight(Abstract,CurrentKey,Key,Weight,V,Action,TT,SelectW,P,PropLikelihood) :-
%	writeln(start_cw),
	(
	Abstract == true ->
	(compute_weight(CurrentKey,Key,Weight,V,Action,TT,SelectW,P,PropLikelihood) ->
		true
		;
		(\+recorded(Key,nextstatelikelihood(Action,_),_),recorda(Key,nextstatelikelihood(Action,0),_),Weight=0,P=0 )
	)
	;
	(compute_weight_full(CurrentKey,Key,Weight,V,Action,TT,SelectW,P,PropLikelihood) ->
		(
		true %compute_weight_full(CurrentKey,Key,Weight2,V,Action,TT,SelectW,P2,PropLikelihood),
		%writeln((Weight,Weight2,P,P2))
		)
		;
		(%writeln(cw2),\+recorded(Key,nextstatelikelihood(Action,_),_),
		%writeln(cw3),recorda(Key,nextstatelikelihood(Action,0),_),writeln(cw4),
		%st(Key,Action),
		Weight=0,P=0 )
	)
	),%writeln(end_cw),
	!.

st(Key,Action) :-
	%writeln(cw2),
	\+recorded(Key,nextstatelikelihood(Action,_),_),%writeln(cw3(recorda(Key,nextstatelikelihood(Action,0),_))),
	recorda(Key,nextstatelikelihood(Action,0),_).%writeln(cw4).
*/

compute_weight(CurrentKey,Key,Weight,V,Action,TT,SelectW,P,Proposal) :-
	!,

/*	findall(next(Var)~=Val,recorded(Key,next(Var)~=Val,_),L1),
	findall(next(Var2),(recorded(Key,next(Var2),_),Var2\=greedy(_)),L2),
	append([L1,L2],L),
*/
	(
		true -> % L\==[] ->
		(
		eraseall(tempparticle),
		eraseall(tempparticle2),


		abolish_all_tables,
	%	writeln(L3),
	%	trace,
	%	eval_weight_planning3(CurrentKey,tempparticle,L3,MeanV),
	%	writeln(MeanV),

		recorded(Key,proof(next,Proof),_),%trace,
		assert_next_abstract(CurrentKey,tempparticle2,Proof,Proof1),
		/*
		writeln(stored(Proof)),
		writeln(stored1(Proof1)),

		printkeyp(CurrentKey),nl,
		printkeyp(tempparticle2),
		*/
		%trace,

		(eval_weight_planning_abstract(CurrentKey,tempparticle2,Proof,P1) ->
%		(eval_weight_planning(CurrentKey,tempparticle,L3,P1) ->
		(
		/*
			% for explicit negation
			% not next(NewVar) in CurrentKey => not next(NewVar) in Key
			forall(
			distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar)),
			(
			memberchk(next(NewVar),L) ->
			true
			;
			(
			distributionalclause:proof_query_backward(Key,tempparticle2,next(NewVar))  ->
			true;
			(% printkeyp(Key),nl,nl,printkeyp(CurrentKey),nl,writeln(next(NewVar)),trace,
			fail)
			)
			)) -> P=P1 ; P=0*/
			P=P1
		)
		;
			P=0
		),
	%	writeln(lkl(P,Proposal)),
		/*get(_),*/
		%recorda(Key,nextstatelikelihood(Action,P)),


		/*
		eraseall(tempparticle),abolish_all_tables,
		(eval_weight_planning2(CurrentKey,tempparticle,L3,P2) ->
		true;
		P2=0
		),
		writeln((P,P2)),*/


		%erase_next(CurrentKey,L3),
%		printkey(CurrentKey),nl,

/*
		nl,nl,
		writeln((CurrentKey,Key,L3,P)),
		printkey(CurrentKey),nl,nl,
		printkey(Key),*/

		%recorded(CurrentKey,Action,Ref),
		%erase(Ref),
		%writeln('--- New Current episode ---'),
		%printkey(CurrentKey),
		recorded(Key,likelihood(_,Likelihood),_),

		Weight is SelectW*P/Proposal % * SelectW %/Likelihood^(1/(TT+1)/2)  equivalent to alpha MC (Reinforcement learning: An introduction pag. 34)
		%recorded(Key,v(next,V),_)
		)
	),
	Weight>0,
%	writeln((Key,Proof)),
%	writeln(p(P,V,Action)),
%	printkey(CurrentKey),
	!.

% check DomainPruning
compute_weight_full2(CurrentKey,Key,Weight,V,Action,TT,SelectW,P,Proposal,DomainPruning) :-
	eraseall(tempparticle),
	%eraseall(tempparticle2),
	%dcpf:plaincopyparticles(CurrentKey,tempparticle),
	%(recorded(CurrentKey,CC,_),recorda(tempparticle,CC,_),fail;true),
	abolish_all_tables,
	findall(next(Var)~=Val,recorded(Key,next(Var)~=Val,_),L1),
	findall(next(Var2),(recorded(Key,next(Var2),_),Var2\=greedy(_)),L2),
	append([L1,L2],L),
	!,
	%recorded(Key,temp(listnext(L)),_),
	%printkeyp(Key),
	%writeln((L,ListNext)),
	/*
	forall((recorded(Key,next(Var2),_),Var2\=greedy(_)),

	distributionalclause:proof_query_backward(CurrentKey,tempparticle2,next(Var2))),*/



	(
		L\=[]->
		(
			%duplicate_term(L,GPosEvidence),
			bb_put(distributionalclause:wevidence,1.0),
			dcpf:test_to_list(PosEvidence1,L),
			% GPosEvidence has to be ground in variable name
			% to update
			%writeln('proof_query_backward_lw changed!'),


			bb_put(distributionalclause:q,L),
			distributionalclause:proof_query_backward_lw(CurrentKey,tempparticle,PosEvidence1,_),
			bb_delete(distributionalclause:wevidence,P1)
		)
		;
			P1=1
	),
	!,P1>0,
	(
		(
		(DomainPruning==relfalse;DomainPruning==reltrue),
		(
			(distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar)),
			\+memberchk(next(NewVar),L2)
			)
			;
			(
			distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar2)~=ValNewVar2),
			\+memberchk(next(NewVar2)~=ValNewVar2,L1)
			)
		)) ->
			(P=0/*,writeln(L),printkeyp(tempparticle),writeln(error1),halt*/) ; P=P1
	),
%	writeln(p(P1,P)),
	Weight is SelectW*P/Proposal,
%	writeln(Weight is SelectW*P/Proposal),
	Weight>0,
	!.

compute_weight_full(CurrentKey,Key,Weight,V,Action,TT,SelectW,P,Proposal,DomainPruning) :-
	!,
	eraseall(tempparticle),
	findall(next(Var)~=Val,(recorded(Key,next(Var)~=Val,_),recorda(tempparticle,next(Var)~=Val,_)),L1),
	findall(next(Var2),(recorded(Key,next(Var2),_),Var2\=greedy(_),recorda(tempparticle,next(Var2),_)),L2),
	append([L1,L2],L),
	%recorded(Key,temp(listnext(L)),_),
%	printkeyp(Key),nl,
%	recorded(CurrentKey,listcurrent(Lc),_),
%	writeln((Key,L)),
%	abolish_all_tables,
%	(distributionalclause:query_proof(Key,current(_) ~= _) -> (writeln(Key),printkeyp(Key),halt);true),
%	abolish_all_tables,
	(
		L\==[] ->
		(

		%eraseall(tempparticle2),
		%assert_next(CurrentKey,tempparticle,L,L3),
		L3=L,

		%(L\=L3 -> writeln((L,L3));true),
		%writeln(compute_weight_full),
		%writeln(4.1),
		abolish_all_tables,
	%	writeln(L3),
	%	trace,
	%	eval_weight_planning3(CurrentKey,tempparticle,L3,MeanV),
	%	writeln(MeanV),

%		recorded(Key,proof(next,Proof),_),%trace,
%		(eval_weight_planning_abstract(CurrentKey,tempparticle2,Proof,P1) ->
		%writeln(4.2),
		((eval_weight_planning(CurrentKey,tempparticle,L3,P1),P1>0) ->
		(

		(
		(DomainPruning==relfalse;DomainPruning==reltrue),
		% This does not work if there are clauses h_t <- body_t!!!
		(
			(%fail,
			distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar)),

			%writeln(distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar))),
			%\+memberchk(next(NewVar),L)
			\+recorded(Key,next(NewVar),_)
			)
			;
			(%fail,
			distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar2)~=ValNewVar2),
			\+recorded(Key,next(NewVar2)~=ValNewVar2,_)
			%\+memberchk(next(NewVar2)~=ValNewVar2,L)
			)
		)
		/*(% This does not work if there are clauses h_t <- body_t!!!
			inference(backward(_)) ->
				(
				dcpf:inferencestep_particlefilter_backward2(tempparticle),
				%printkeyp(tempparticle),
				(
				(recorded(tempparticle,next(NewVar),_),\+memberchk(next(NewVar),L))
				;
				(recorded(tempparticle,next(NewVar2)~=ValNewVar2,_),\+memberchk(next(NewVar2)~=ValNewVar2,L))
				)
				)
			;
				writeln('error, only backward inference supported')%generate_sample_particlefilter(T,MaxP)
		)*/
		) ->
			(P=0/*,writeln(L),printkeyp(Key),nl,printkeyp(CurrentKey),nl,printkeyp(tempparticle),writeln('error compute_weight_full'),halt*/) ; P=P1 %printkeyp(Key),nl,nl,printkeyp(CurrentKey),writeln(tempparticle),printkeyp(tempparticle),nl,writeln((next(NewVar),next(NewVar2)~=ValNewVar2)),trace
		/*
			% for explicit negation
			% not next(NewVar) in CurrentKey => not next(NewVar) in Key
		%printkeyp(tempparticle),
		(forall( % add the continuous variables! next(NewVar)~=Val
			distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar)),
			(
			memberchk(next(NewVar),L) ->
			true
			;
			(	% necessary if there are clauses h_t <- body_t, but they need to copied to h_t+1 <- body_t+1!! todo!
				distributionalclause:proof_query_backward(Key,tempparticle2,next(NewVar))  ->
				(printkeyp(Key),nl,nl,printkeyp(CurrentKey),nl,writeln(next(NewVar)),trace)
				;
				( %printkeyp(Key),nl,nl,printkeyp(CurrentKey),nl,writeln(next(NewVar,P1)),%trace,
				fail
				)
			)
			)),
			forall( % add the continuous variables! next(NewVar)~=Val
			distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar)~=ValNewVar),
			(
			memberchk(next(NewVar)~=ValNewVar,L) ->
			true
			;
			(
				distributionalclause:proof_query_backward(Key,tempparticle2,next(NewVar)~=ValNewVar)  ->
				(printkeyp(Key),nl,nl,printkeyp(CurrentKey),nl,writeln(next(NewVar)~=ValNewVar),trace)
				;
				( %printkeyp(Key),nl,nl,printkeyp(CurrentKey),nl,writeln(next(NewVar,P1)),%trace,
				fail
				)
			)
		))) ->
			P=P1 ; P=0

			%P=P1
*/
		)
		;
			P=0
		),
		%recorda(Key,nextstatelikelihood(Action,P)),


		/*
		eraseall(tempparticle),abolish_all_tables,
		(eval_weight_planning2(CurrentKey,tempparticle,L3,P2) ->
		true;
		P2=0
		),
		writeln((P,P2)),*/


		%erase_next(CurrentKey,L3),
%		printkey(CurrentKey),nl,

/*
		nl,nl,
		writeln((CurrentKey,Key,L3,P)),
		printkey(CurrentKey),nl,nl,
		printkey(Key),*/

		%recorded(CurrentKey,Action,Ref),
		%erase(Ref),
		%writeln('--- New Current episode ---'),
		%printkey(CurrentKey),
		%recorded(Key,likelihood(_,Likelihood),_),
%		writeln(SelectW*P/Proposal),
%		WeightLog is log(SelectW)+log(P)-log(Proposal)+50,
%		Weight is exp(WeightLog)
		Weight is SelectW*P/Proposal % * SelectW %/Likelihood^(1/(TT+1)/2)  equivalent to alpha MC (Reinforcement learning: An introduction pag. 34)
		%recorded(Key,v(next,V),_)
		)
	),
	%writeln(4.4),
%	writeln(storedkey(Key)),
%	printkeyp(Key),nl,
%	writeln(currentkey(CurrentKey)),nl,printkeyp(CurrentKey),writeln(tempparticle),printkeyp(tempparticle),
%	writeln(Weight is SelectW*P/Proposal),
%	writeln('-----------------------------------------------------------'),nl,
	Weight>0,
%	writeln((Key,Proof)),
%	writeln(p(P,V,Action)),
%	printkey(CurrentKey),

	!.

% check that temp(listnext(L) is not more stored
compute_weight_full1(CurrentKey,Key,Weight,V,Action,TT,SelectW,P,Proposal) :-
	!,
	/*findall(next(Var)~=Val,recorded(Key,next(Var)~=Val,_),L1),
	findall(next(Var2),(recorded(Key,next(Var2),_),Var2\=greedy(_)),L2),
	append([L1,L2],L),*/
	recorded(Key,temp(listnext(L)),_),% FIXME check that temp(listnext(L) is not more stored
	recorded(CurrentKey,listcurrent(Lc),_),
	bb_put(currentkey,CurrentKey),
	(
		L\==[] ->
		(
		eraseall(tempparticle),
		eraseall(tempparticle2),
		assert_next(CurrentKey,tempparticle,L,L3),
		%writeln(compute_weight_full),
		abolish_all_tables,

%		recorded(Key,proof(next,Proof),_),%trace,
%		(eval_weight_planning_abstract(CurrentKey,tempparticle2,Proof,P1) ->
		((eval_weight_planning4([Lc|Action],tempparticle,L3,P1),P1>0) ->
		(

		(
		fail,
		% This does not work if there are clauses h_t <- body_t!!!
		(distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar)),
		%writeln(distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar))),
		\+memberchk(next(NewVar),L)
		) ;
		(%writeln(ok1),
		distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar2)~=ValNewVar2),
		%writeln(distributionalclause:proof_query_backward(CurrentKey,tempparticle,next(NewVar2)~=ValNewVar2)),
		\+memberchk(next(NewVar2)~=ValNewVar2,L)
		)

		) ->
			(P=0,writeln(L),printkeyp(tempparticle),halt) ; P=P1 %printkeyp(Key),nl,nl,printkeyp(CurrentKey),writeln(tempparticle),printkeyp(tempparticle),nl,writeln((next(NewVar),next(NewVar2)~=ValNewVar2)),trace

		)
		;
			P=0
		),

		Weight is SelectW*P/Proposal % * SelectW %/Likelihood^(1/(TT+1)/2)  equivalent to alpha MC (Reinforcement learning: An introduction pag. 34)
		%recorded(Key,v(next,V),_)
		)
	),
%	writeln(storedkey(Key)),
%	printkeyp(Key),nl,
%	writeln(currentkey(CurrentKey)),nl,printkeyp(CurrentKey),writeln(tempparticle),printkeyp(tempparticle),
%	writeln(Weight is SelectW*P/Proposal),
%	writeln('-----------------------------------------------------------'),nl,
	Weight>0,
%	writeln((Key,Proof)),
%	writeln(p(P,V,Action)),
%	printkey(CurrentKey),
	!.
compute_weight_togoal(CurrentKey,Weight,V,Action) :-
	!,
	(
		recorded(CurrentKey,action(_),RR),
		erase(RR)%,
		%fail
		;true
	),

	recorda(CurrentKey,Action,_),
	!,
	eraseall(tempparticle),
	abolish_all_tables,
	distributionalclause:proof_query_backward_eval(CurrentKey,tempparticle,stop ~= true,P),
	%writeln(stop ~= true),
	distributionalclause:proof_query_backward(CurrentKey,tempparticle,reward ~= V),
	recorded(CurrentKey,Action,Ref),
	erase(Ref),
	Weight is P,

	Weight>0,!.

eval_weight_planning2(Key1,Key2,[],1.0) :- !.

eval_weight_planning2(Key1,Key2,[Var~=Val|T],P) :-
%	debug,trace,
	eval_weight_planning2(Key1,Key2,T,PT),
	abolish_all_tables,
	user:distributionalclause(Var,D,Body,_),
	distributionalclause:proof_query_backward(Key1,Key2,Body),!, % 1 sample so far
	likelihood_weighting(Val,D,PH),!,%writeln((H~=Val,PH)),
	P is PT*PH,!.

eval_weight_planning3(Key1,Key2,[],[]) :- !.

eval_weight_planning3(Key1,Key2,[Var~=Val|T],[(M,V)|PT]) :-
%	debug,trace,
	eval_weight_planning3(Key1,Key2,T,PT),
	abolish_all_tables,
	user:distributionalclause(Var,D,Body,_),
	distributionalclause:proof_query_backward(Key1,Key2,Body),!, % 1 sample so far
	D=indepGaussians([ ([X],[Vx]), ([Y],[Vy]), ([Z],[Vz]) ]),
	M=[X,Y,Z],
	V=[Vx,Vy,Vz],!.


assert_next(K,Temp,[],[]) :- !.

assert_next(K,Temp,[H|T],[H|T2]) :-
	\+recorded(K,H,_),
	recorda(Temp,H,_),
	assert_next(K,Temp,T,T2),
	!.

assert_next(K,Temp,[H|T],T2) :-
	recorded(K,H,_),
	assert_next(K,Temp,T,T2),
	!.

assert_next_abstract(K,Temp,\+H,true) :- !. % FIXME: negation seems not handled

assert_next_abstract(K,Temp,(H,T),(H2,T2)) :-
	!,
	assert_next_abstract(K,Temp,H,H2),
	assert_next_abstract(K,Temp,T,T2),!.

assert_next_abstract(K,Temp,H,H) :-
	\+recorded(K,H,_),
	recorda(Temp,H,_),!.

assert_next_abstract(K,Temp,H,true) :-
	recorded(K,H,_),writeln(assert_next_abstract(K,Temp,H,true)),printkeyp(K),nl,nl,printkeyp(Temp),halt,
	!.
/*
erase_next(K,[]) :- !.
erase_next(K,[H|T]) :-
	(recorded(K,H,R)->
	erase(R),writeln(recorded(K,H,R));true
	),
	erase_next(K,T),
	!.
*/
% use check_evidence_backward instead, but evidence needs to be asserted...
eval_weight_planning(Key,Temp,[],1.0) :- !.
% use check_evidence_backward instead, but evidence needs to be asserted...
eval_weight_planning(Key,Temp,[Var|T],P) :-

	(recorded(Temp,Var,RR),erase(RR);true),
	(
	distributionalclause:proof_query_backward_eval(Key,Temp,Var,PH) ->
	true;
	PH=0
	),
%	printkeyp(Key),writeln(var(Var)),writeln(temp),
%	printkeyp(Temp),nl,
%	recorda(Temp,Var,_),
%	printkeyp(Temp),nl,
	(
		PH>0 ->
		(
			(eval_weight_planning(Key,Temp,T,PT) ->
			true;
			PT=0
			),
			P is PT*PH
		)
		;
		P=0
	),!.

% use check_evidence_backward instead, but evidence needs to be asserted...
eval_weight_planning4(LC,Temp,List,P) :- recorded(eval_weight_planning4,l(LC,List,P),_),!.
eval_weight_planning4(LC,Temp,[],1.0) :- !.
% use check_evidence_backward instead, but evidence needs to be asserted...
eval_weight_planning4(LC,Temp,[Var|T],P) :-
	bb_get(currentkey,Key),
	(recorded(Temp,Var,RR),erase(RR);true),
	(
	distributionalclause:proof_query_backward_eval(Key,Temp,Var,PH) ->
	true;
	PH=0
	),
%	printkeyp(Key),writeln(var(Var)),writeln(temp),
%	printkeyp(Temp),nl,
%	recorda(Temp,Var,_),
%	printkeyp(Temp),nl,
	(
		PH>0 ->
		(
			(eval_weight_planning4(LC,Temp,T,PT) ->
			true;
			PT=0
			),
			P is PT*PH
		)
		;
		P=0
	),
	recorda(eval_weight_planning4,l(LC,[Var|T],P),_),
	!.


eval_weight_planning_abstract(Key,Temp,true,1.0) :-
	!.

eval_weight_planning_abstract(Key,Temp,(A,B),P) :-
	!,
	eval_weight_planning_abstract(Key,Temp,A,P1),
	P1>0,
	eval_weight_planning_abstract(Key,Temp,B,P2),
	P is P1*P2.

eval_weight_planning_abstract(Key,Temp,\+Var,PH) :-
	!,
%	(recorded(Temp,Var,RR),erase(RR);true),
	(
		distributionalclause:proof_query_backward(Key,Temp,Var) ->
		PH=0;
		PH=1
	).

eval_weight_planning_abstract(Key,Temp,Var,PH) :-
	(recorded(Temp,Var,RR),erase(RR);true),
	(
	distributionalclause:proof_query_backward_eval(Key,Temp,Var,PH) ->
	true;
	PH=0
	),
%	printkeyp(Key),writeln(var(Var)),writeln(temp),printkeyp(Temp),
%	recorda(Temp,Var,_),
	!.

eval_likelihood_planning_abstract(Key,Temp,true,1.0) :-
	!.
eval_likelihood_planning_abstract(Key,Temp,(A,B),P) :-
	!,
	eval_likelihood_planning_abstract(Key,Temp,A,P1),
	P1>0,
	eval_likelihood_planning_abstract(Key,Temp,B,P2),
	P is P1*P2.

eval_likelihood_planning_abstract(Key,Temp,\+Var,PH) :-
	!,
%	(recorded(Temp,Var,RR),erase(RR);true),
	(
		distributionalclause:proof_query_backward(Key,Temp,Var) ->
		PH=0;
		PH=1
	).

eval_likelihood_planning_abstract(Key,Temp,Var,PH) :-
	(recorded(Key,Var,RR),erase(RR);true),
	(
	distributionalclause:proof_query_backward_eval(Key,Temp,Var,PH) ->
	true;
	PH=0
	),
	(ground(RR) -> recorda(Key,Var,_);(writeln(eval_likelihood_planning_abstract(Key,Temp,Var,PH)),trace)),
%	printkeyp(Key),writeln(var(Var)),writeln(temp),printkeyp(Temp),
%	recorda(Temp,Var,_),
	!.

% use check_evidence_backward instead, but evidence needs to be asserted...
eval_weight_planning1(Key,Temp,[],1.0) :- !.
% use check_evidence_backward instead, but evidence needs to be asserted...
eval_weight_planning1(Key,Temp,[Var|T],P) :-

	(recorded(Key,Var,RR),erase(RR);true),
	(
	distributionalclause:proof_query_backward_eval(Key,Temp,Var,PH) ->
	true;
	PH=0
	),
	(ground(RR) -> recorda(Key,Var,_);(recorda(Temp,Var,_),nl,printkeyp(Temp),nl,trace)), % probable duplicate of Var in Temp distributionalclause:proof_query_backward_eval stores var!
%	writeln(var(Var)),
	(
		PH>0 ->
		(
			(eval_weight_planning1(Key,Temp,T,PT) ->
			true;
			PT=0
			),
			P is PT*PH
		)
		;
		P=0
	),!.


generate_episodeTD(WHeu,PBRW,Abstract,Lambda,Epsilon,Episode,0,MaxDepth,Delta,0.0,0.0,false,Key2Explore,0,BAction,1) :- !.

generate_episodeTD(WHeu,PBRW,Abstract,Lambda,Epsilon1,Episode,Depth,MaxDepth,Delta,TotalReward,EReward,Terminated,Key2Explore,Qaction,BAction,PropLpolicy) :-
	Depth>0,
	WeightDepth is 0.5+0.5*(MaxDepth-Depth)/max(MaxDepth-1,1),
	(
	MaxDepth==Depth ->
	(Epsilon is Epsilon1*1.5/*,writeln(epsilon(Epsilon))*/)
	;
	Epsilon is Epsilon1%/WeightDepth/1.5
	),%*Depth/MaxDepth*2,
%	writeln(epsilon(Epsilon)),
	bb_get(user:discount,Discount),
	ST is MaxDepth-Depth,
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
%	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	%write('timestep '),write(T),nl,
	abolish_all_tables,
	%write('particle '),write(I),nl,
%	trace,
	NewDepth is Depth-1,
	term_to_atom(p(Depth,Episode),KeyPrec),
	%printkeyp(KeyPrec),
	bb_get(user:storingstrategy,Storing),
	((Storing==max) -> EpsilonNoHeu= 3;EpsilonNoHeu= 1),
	distributionalclause:proof_query_backward(KeyPrec,findall_forward(AA,current(adm(AA)),ActionList)),
	%writeln(1),
	abolish_all_tables,
%	distributionalclause:proof_query_backward(KeyPrec,findall_forward(WW1,current(adm(WW1)),ActionList2)),
	(
	ActionList==[] ->
	(writeln('ActionList empty!'),printkeyp(KeyPrec))
	;true
	),
/*	(
	ActionList\=ActionList2 ->
	(writeln('error findall_forward'),halt)
	;true
	),*/

	%writeln(ActionList),
	%writeln(KeyPrec),
	%printkeyp(KeyPrec),
%	debug,trace,debug,
%	(distributionalclause:proof_query_backward(KeyPrec,current(reward) ~= R) -> true; (writeln('error: reward not defined'),fail) ),
	%printkeyp(KeyPrec),nl,nl,
%trace,%abolish_all_tables,

	(
	distributionalclause:proof_query_backward(KeyPrec,current(stop)) ->
		(
			nl,writeln('STOP'),nl,%,debug,trace
			STOP=true,
			BAction=null,
			Action=null,
			PropLpolicy=1,
			PropLpolicyNext=1,
			Terminated=true,
			bb_put(terminated,true),
			term_to_atom(p(0,Episode),TempKey),
			recorda(TempKey,terminated(true,Depth,ST),_),
			abolish_all_tables,
			(distributionalclause:proof_query_backward(KeyPrec,current(reward) ~= R) -> true; (writeln('error: reward not defined'),printkeyp(KeyPrec),fail) ),
			TotalReward=R,
%			TotalRewardHeu=R,
			EReward=R,
			V=0.0,
			Likelihood=1,
			DeltaReward=(+inf),
			VarVAction=0,


			writeln(step(ST)),
			(Episode>50 ->
			true %system('aplay pop.wav 2> /dev/null &')
			;
			true
			),
			(
			Abstract==true ->
			(
				distributionalclause:partialproof3(KeyPrec,(current(reward)~= _),Listproof),
				recorda(KeyPrec,proof(current,Listproof),_)
			)
			;
			true
			),
			/*findall(current(Var1)~=Val1,recorded(KeyPrec,current(Var1) ~=Val1,_),L11),
			findall(current(Var12),(recorded(KeyPrec,current(Var12),_),Var12\=greedy(_)),L12),
			append([L11,L12],Lc),
			recorda(KeyPrec,temp(listcurrent(Lc)),_),*/
			printkeyp(KeyPrec)
		)
		;
		(
			STOP=false,
			term_to_atom(p(NewDepth,Episode),KeyNext),
			eraseall(KeyNext),
			bb_get(user:strategy,Strategy),
			%(Depth==1 -> printkeyp(KeyPrec);true),
			/*findall(current(Var1)~=Val1,recorded(KeyPrec,current(Var1) ~=Val1,_),L11),
			findall(current(Var12),(recorded(KeyPrec,current(Var12),_),Var12\=greedy(_)),L12),
			append([L11,L12],Lc),
			recorda(KeyPrec,temp(listcurrent(Lc)),_),*/
			%writeln(recorda(KeyPrec,listcurrent(Lc))),
			(Strategy==softmax ->
				findsoftmaxaction(WHeu,Epsilon,PBRW,Abstract,Depth,MaxDepth,Discount,Episode,Prec,ActionList,BAction,Qaction,VarVAction,RandomAction,AvgRandom,VarRandom,ExplorationAction,LExpl)
			;

				findbestaction(WHeu,PBRW,Abstract,Depth,MaxDepth,Discount,Episode,Prec,ActionList,BAction,Qaction,VarVAction,RandomAction,AvgRandom,VarRandom,ExplorationAction,LExpl)
			),
			bb_get(user:ucbv,UCBV),
			%writeln(findbestaction(PBRW,Abstract,Depth,MaxDepth,Discount,Episode,Prec,ActionList,BAction,Qaction,VarVAction,ExplorationAction,LExpl)),
			(
			(random<1-Epsilon,Strategy\=softmax) -> % epsilon-greedy  ,Depth\=MaxDepth
				(

				(
				((random<Epsilon*EpsilonNoHeu;UCBV==true),ExplorationAction\=[]) ->
					(Action=ExplorationAction ,writeln(explore(ST,ExplorationAction,Qaction)),Greedy=false,Pi is (1-Epsilon)*Epsilon*EpsilonNoHeu )
				;
					(Action=BAction, Greedy=true,Pi is 1-Epsilon)
				),
				%writeln(Action),
				SelectedQaction=Qaction,
				VarVSelectedAction=VarVAction,
				recorda(KeyPrec,next(greedy(Greedy)),_),
				%Greedy=true,

				(
				( LExpl>0,\+ground(Key2Explore) ) ->
					(
						Key2Explore=(Depth,KeyPrec),
						eraseall(1),
						dcpf:plaincopyparticles(KeyPrec,1)
					)
					;
					true
				)

				)
			;
				(% exploration
				Pi is Epsilon,
				%findbestaction(Depth,MaxDepth,Episode,Prec,ActionList,BAction,Vaction,_,ExplorationAction),
				%Vaction=0,
				%SelectedVaction=0,
				%dcpf:sample(uniform(ActionList),Action),
				Action=RandomAction,
				SelectedQaction=AvgRandom,
				VarVSelectedAction=VarRandom,
				/*((random<0.8,ExplorationAction\=[]) ->
					(Action=ExplorationAction,writeln((ST,ExplorationAction)) )
				;
					dcpf:sample(uniform(ActionList),Action)
				),*/

				%evaluateaction(PBRW,Abstract,Depth,MaxDepth,Discount,Episode,Prec,Action,NR,SelectedQaction,VarVSelectedAction,_),
				%writeln(greedyfalse),
				Greedy=false,
				(BAction==Action ->
				recorda(KeyPrec,next(greedy(true)),_)
				;
				recorda(KeyPrec,next(greedy(false)),_)
				)
				)
			),
			%writeln(greedy(Greedy)),
			%(SelectedQaction==0 -> writeln(toexp(KeyPrec,Action));true),
			%writeln(before0),writeln(KeyPrec),printkeyp(KeyPrec),nl,
			(
			recorded(KeyPrec,action(_),RR),
			erase(RR)%,
			%fail
			;true
			),
			%writeln(2),
			abolish_all_tables,
			recorda(KeyPrec,Action,_),
			writeln((KeyPrec,Action)),
			%writeln(before),writeln(KeyPrec),printkeyp(KeyPrec),nl,
			%(MaxDepth<Depth+2 -> (writeln(bestaction(BAction,Qaction)),writeln(selectedaction(Action)));true),
			(distributionalclause:query_proof(KeyPrec,current(reward) ~= _) -> (writeln('error: reward already defined'),halt);true),
			(distributionalclause:proof_query_backward(KeyPrec,current(reward) ~= R) -> true; (writeln('error: reward not defined'),printkeyp(KeyPrec),fail) ),
			%writeln(3),
			abolish_all_tables,
			retractall(user:noise),
			(
			inference(backward(_)) ->
				dcpf:inferencestep_particlefilter_backward2(KeyPrec)
			;
				writeln('error, only backward inference supported')%generate_sample_particlefilter(T,MaxP)
			),
			assert(user:noise),
			%writeln(4),
			abolish_all_tables,
			%writeln(KeyPrec),printkeyp(KeyPrec),nl,
			eraseall(KeyNext),
			dcpf:copyparticles_core(KeyPrec,KeyNext),
			%writeln(prec1(KeyPrec)),printkeyp(KeyPrec),nl,nl,
			%writeln(next1(KeyNext)),printkeyp(KeyNext),nl,nl,
			% Heuristic
			(
			Depth== 0 ->
			(

			(
			eraseall(tempparticle),
			distributionalclause:proof_query_backward(KeyPrec,tempparticle,current(heuristicV(HeuristicPrec))),
			eraseall(tempparticle),
			%writeln(5),
			abolish_all_tables,
			distributionalclause:proof_query_backward(KeyNext,tempparticle,current(heuristicV(HeuristicNext))),
			HeuristicTerm is HeuristicNext*1^(Episode), %max(0,T-5)^0.5*(HeuristicNext-HeuristicPrec)*HeuristicNext*0.9^(Episode),
			(write(HeuristicTerm),write(' ') ;true)
			;HeuristicTerm=0)
			)
			;
			HeuristicTerm=0
			),


			%printkeyp(KeyPrec),nl,

			%
			%writeln(generate_episodeTD(Epsilon,Episode,NewDepth,MaxDepth,Delta,V,EReward2,Terminated,Key2Explore)),
			generate_episodeTD(WHeu,PBRW,Abstract,Lambda,Epsilon1,Episode,NewDepth,MaxDepth,Delta,V,EReward2,Terminated,Key2Explore,_,_,PropLpolicyNext),
			PropLpolicy is PropLpolicyNext*Pi,
			%writeln(generate_episodeTD(Epsilon,Episode,NewDepth,MaxDepth,Delta,V,EReward2,Terminated,Key2Explore)),

			%writeln(depth(Depth)),
			%printkey(KeyPrec),nl,
			%(Terminated==true -> spy distributionalclause:regressionproof/4;true),
			(
			NewDepth==0 ->
			(
				% printkeyp(KeyPrec),nl,

				% distributionalclause:partialproof(KeyPrec,(current(reward)~= _),Listproof),
				(
				Abstract==true ->
				(
					distributionalclause:partialproof3(KeyPrec,(current(reward)~= _),Listproof),

					writeln(partialproof3(Listproof)),nl,nl,
					recorda(KeyPrec,proof(current,Listproof),_)
					%removecurrent(KeyNext)
					%printkey(KeyPrec),nl
				)
				;
					true
				)
			)
			;
			(
				%writeln(prec2(KeyPrec)),printkeyp(KeyPrec),nl,nl,
				%writeln(next2(KeyNext)),printkeyp(KeyNext),nl,nl,
				(
				Abstract==true ->
				(
					recorded(KeyPrec,proof(next,NextProof),_),
					writeln(proof(next,NextProof)),
					%distributionalclause:current2next(KeyPrec,NextProof,NextProof2),
					%distributionalclause:cleanformula2(NextProof2,NextProof3),

					(
					distributionalclause:regressionproof3(KeyPrec,(current(reward)~= _),NextProof,Listproof) -> true;
					(writeln(error_regressionproof3(KeyPrec,(current(reward)~= _),NextProof,Listproof)),nl,trace,distributionalclause:regressionproof3(KeyPrec,(current(reward)~= _),NextProof,Listproof);halt)
					),

					recorda(KeyPrec,proof(current,Listproof),_)%,
					%writeln(regressionproof3(KeyPrec,(current(reward)~= _),NextProof,Listproof)),nl
				)
				;
					true
				),
				removecurrent(KeyNext)%,
				%writeln(next3(KeyNext)),
				%abolish_all_tables,
				%(distributionalclause:query_proof(KeyNext,current(_) ~= _) -> (halt);true),
				%abolish_all_tables
				%printkeyp(KeyNext),nl

			)
			),

			%((VarVAction==(+inf);Greedy==true) ->  % ,VarVAction<(inf) Greedy==true

			%),
			%OptimisticV is (R+V)*Lambda + (1-Lambda)*Qaction, % max(V,Vaction), %V*Lambda + (1-Lambda)*Vaction,% max(V,Vaction), % V*Lambda + (1-Lambda)*Vaction,% max(V,Vaction),
			%writeln((Lambda,Qaction,VarVAction,Greedy)),
			%TotalReward is Discount*(V*Lambda + (1-Lambda)*Vaction )+R, %Discount*V+R,
			%TotalReward is (R+Discount*V)*Lambda + (1-Lambda)*Qaction, %Discount*(OptimisticV)+HeuristicTerm,

			(Storing==max ->
			(
				(VarVAction==(+inf);VarVAction==(inf)) ->
				TotalReward is (R+Discount*V)*Lambda + (1-Lambda)* max(Qaction,(R+Discount*V))
				;
				(TotalReward is max(Qaction,(R+Discount*V)))

				%TotalReward is (R+Discount*V)*Lambda + (1-Lambda)*max(Qaction,(R+Discount*V))
			)
			;
				(Storing==noheuristics ->
				(
					(VarVAction==(+inf);VarVAction==(inf)) ->
					TotalReward is (R+Discount*V)
					;
					TotalReward is (R+Discount*V)*Lambda + (1-Lambda)*Qaction
				)
				;
					Storing==max2 ->
					(
					%TotalReward is ((R+Discount*V)*0.5 +  max(Qaction,(R+Discount*V))/(VarVAction+0.00001))/(1/(VarVAction+0.00001)+0.5)
					(VarVAction==(+inf);VarVAction==(inf)) ->
						TotalReward is (R+Discount*V)
					;
						TotalReward is (R+Discount*V)*(1-Lambda) + Lambda*max(Qaction,(R+Discount*V)) % max2
					)
					;
					TotalReward is (R+Discount*V)*Lambda + (1-Lambda)*Qaction
				)
			),
%			TotalRewardHeu is (R+Discount*V)*0.5 + (1-0.5)*Qaction,
			%writeln(tot(TotalReward,TotalReward2)),
			EReward is R+Discount*EReward2,
			%writeln((R,TotalReward,EReward,V,EReward2)),
			DeltaReward is ((R+Discount*V)-Qaction),
			%writeln((mc-td,V,Vaction,Diff)),
			% compute likelihood p(next|current)
			eraseall(tempparticle),
			abolish_all_tables,
			(
				Abstract==true ->
				(
				(
					ground(NextProof) ->
					eval_likelihood_planning_abstract(KeyPrec,tempparticle,NextProof,Likelihood)
					;
					Likelihood=1
				)
				)
				;
				(
					findall(next(Var)~=Val,recorded(KeyPrec,next(Var) ~=Val,_),L1),
					findall(next(Var2),(recorded(KeyPrec,next(Var2),_),Var2\=greedy(_)),L2),
					append([L1,L2],L),
					%recorda(KeyPrec,temp(listnext(L)),_),
					eval_weight_planning1(KeyPrec,tempparticle,L,Likelihood),
					(Likelihood==0 -> halt;true)
					/*writeln(beforel(Likelihood)),printkeyp(KeyPrec),nl,

					findall(next(Var)~=Val,(recorded(KeyPrec,next(Var) ~=Val,Ref1),erase(Ref1)),L1),
					findall(next(Var2),(recorded(KeyPrec,next(Var2),Ref2),Var2\=greedy(_),erase(Ref2)),L2),
					append([L1,L2],L),
					recorda(KeyPrec,temp(listnext(L)),_),
					%forall(recorded(KeyPrec,next(Var) ~=Val,Ref1),erase(Ref1)),
					%forall((recorded(KeyPrec,next(Var2),Ref2),Var2\=greedy(_)),erase(Ref2)),

					abolish_all_tables,
					%writeln(likelihood(Likelihood)),

					duplicate_term(L,GPosEvidence),
					bb_put(distributionalclause:wevidence,1.0),
					dcpf:test_to_list(PosEvidence1,L),
					distributionalclause:proof_query_backward_lw(KeyPrec,PosEvidence1,GPosEvidence),
					bb_delete(distributionalclause:wevidence,Likelihood)%,
					%nl,writeln(l2(PTemp)),printkeyp(KeyPrec),nl*/
				)
			)



			%writeln(NextProof),printkey(KeyPrec),nl,
			%


			%writeln(after),printkey(KeyPrec),nl,
			%writeln((KeyPrec,NextProof,Likelihood))

		)
	),

	%removecurrent(KeyPrec),

	recorda(KeyPrec,v(current,TotalReward),_),
%	recorda(KeyPrec,terminated(current,Terminated),_),
	PrecPrec is Depth+1,


	(
	(PrecPrec=<MaxDepth+1) -> %,(abs(DeltaReward)>sqrt(VarVAction);sqrt(VarVAction)> -10.00)) ->
	(
		term_to_atom(p(PrecPrec,Episode),KeyPrecPrec),
		recorda(KeyPrecPrec,v(next,TotalReward),_),
%		recorda(KeyPrecPrec,vheu(next,TotalRewardHeu),_),
		recorda(KeyPrecPrec,proposallikelihood(1,1,PropLpolicy),_),
		(
		Abstract==true ->
		(
			distributionalclause:current2next(KeyPrec,Listproof,ListproofNext),
			%writeln(current2next(KeyPrec,Listproof,ListproofNext)),
			recorda(KeyPrecPrec,proof(next,ListproofNext),_)
		)
		;
		true
		)
	)
	;
		(PrecPrec=<MaxDepth,writeln(skipped((R+Discount*V)-SelectedQaction,DeltaReward,VarVAction));true)
	),
	recorda(KeyPrec,likelihood(Action,Likelihood),_),
	(\+ground(likelihood(Action,Likelihood))->(writeln(likelihood(Action,Likelihood)),halt);true),
	term_to_atom(maxl(Depth),KeyMaxLikelihood), % Key MaxLikelihood
	(bb_get(KeyMaxLikelihood,MaxLikelihood) ->
			(
				Likelihood>MaxLikelihood ->
				bb_put(KeyMaxLikelihood,Likelihood)
				;
				true
			)
			;
			bb_put(KeyMaxLikelihood,Likelihood)
	),
	%writeln(likelihood(Action,Likelihood)),
	%recorda(KeyPrec,proposallikelihood(1,Likelihood),_),
	term_to_atom(p(Depth),KeyMinProp), % min value for q(s_t+1) for depth (next)
	EpisodePrec is Episode-1,
	LimInfW is max(1,EpisodePrec-15),
	bb_get(user:proposal,Useproposal),
	% computation of proposal distribution q(s_t+1)
	(
	Useproposal==true ->
	(
	(forall((between(LimInfW,EpisodePrec,NumEp),term_to_atom(p(Depth,NumEp),Key3),recorded(Key3,nextstatelikelihood(Action2,LNext),RefNextState) ),
	(
		erase(RefNextState),

		(
		Action2==Action ->
		(
			recorded(Key3,proposallikelihood(Number,Proposallikelihood,Pi3),RefProp),
			NewProp is Proposallikelihood+LNext,
			%writeln((Key3,NewProp is Proposallikelihood+LNext)),
			erase(RefProp),
			NewNumber is Number+1, % number not correct if nextstatelikelihood is not recorded!
			recorda(Key3,proposallikelihood(NewNumber,NewProp,Pi3),_),

			NewPropLikelihood is NewProp/NewNumber,
			(bb_get(KeyMinProp,MinProp) ->
			(
				NewPropLikelihood<MinProp ->
				bb_put(KeyMinProp,NewPropLikelihood)
				;
				true
			)
			;
			bb_put(KeyMinProp,NewPropLikelihood)
			)

		)
		;
			true
		)
	) );true),

	(
		Abstract==true ->
		(
			findall(Likelihood2,(
				between(LimInfW,EpisodePrec,NumEp),
				eraseall(tempparticle3),
				term_to_atom(p(PrecPrec,NumEp),Key4),
				term_to_atom(p(Depth,NumEp),Key44),
				recorded(Key4,v(next,_),_),
				dcpf:copyparticles_core_clean(Key4,tempparticle3),

				eraseall(tempparticle),
				recorded(Key44,likelihood(ActionL,_),_), % fixed
				recorda(tempparticle3,ActionL,_),
				%(\+ground(ActionL)->(writeln(likelihood(ActionL,_)),halt);true),
				%printkeyp2(Key4),nl,
				%printkeyp2(tempparticle3),nl,
				%writeln(assert_next_abstract(tempparticle3,tempparticle,NextProof,NextProof1)),
				assert_next_abstract(tempparticle3,tempparticle,NextProof,NextProof1),
				abolish_all_tables,
				(eval_weight_planning_abstract(tempparticle3,tempparticle,NextProof1,P1) ->
				Likelihood2=P1;Likelihood2=0
				)
				),LProp),
			%writeln(LProp),
			sum_list([Likelihood|LProp],Sumprop),
			length([Likelihood|LProp],NumProp),
			recorda(KeyPrec,proposallikelihood(NumProp,Sumprop,PropLpolicyNext),_)

		)
		;
		(

			findall(Likelihood2,(
				between(LimInfW,EpisodePrec,NumEp),
				eraseall(tempparticle3),
				term_to_atom(p(PrecPrec,NumEp),Key4),
				term_to_atom(p(Depth,NumEp),Key44),
				dcpf:copyparticles_core_clean(Key4,tempparticle3),

				%writeln(Key4),printkeyp(Key4),nl,
				eraseall(tempparticle),
				recorded(Key44,likelihood(ActionL,_),_), % was Key4 it should be Key44
				recorda(tempparticle3,ActionL,_),
				%printkeyp(tempparticle3),nl,
				abolish_all_tables,
				eval_weight_planning(tempparticle3,tempparticle,L,Likelihood2)
				),LProp),
			%writeln(LProp),
			TempLikelihood is Likelihood*0.1, % fixme! reduce influence likelihood in the computation of q
			sum_list([TempLikelihood|LProp],Sumprop),
			length([Likelihood|LProp],NumProp1),
			NumProp is NumProp1-1+0.1,
			(recorded(KeyPrec,proposallikelihood(1,1,_),RefPropLike) -> erase(RefPropLike);true),
			recorda(KeyPrec,proposallikelihood(NumProp,Sumprop,PropLpolicyNext),_)
			%printkeyp(KeyPrec),
			%writeln((NumProp,Sumprop)),nl,nl,nl
		)
	),
	(recorded(KeyPrec,proposallikelihood(AA,BB,CD),_), CC is BB/AA*CD),
	(bb_get(KeyMinProp,MinProp1) ->
			(
				CC<MinProp1 ->
				bb_put(KeyMinProp,CC)
				;
				true
			)
			;
			bb_put(KeyMinProp,CC)
	),
	bb_get(KeyMinProp,MinProp2),
	bb_get(KeyMaxLikelihood,MaxLikelihood1)
	)
	;
	recorda(KeyPrec,proposallikelihood(1,1,1),_)
	),

	%writeln(proposallikelihood(KeyPrec,MinProp2,CC,MaxLikelihood1)),
	%
%	writeln((KeyPrec,v(current,TotalReward),v(next,V))),
/*	(Depth==1 ->
		removecurrent(KeyPrec)
		;
		true
	),*/


	bb_get(user:discretepruning,DomainPruning),
	TMPMod is Episode mod 4,
	(
	(TMPMod == 0,(DomainPruning==proptrue;DomainPruning==reltrue))  ->
	(
		bb_get(user:limit_previous_episodes,LPE),
		MinCheck is max(1,Episode-LPE),
		bb_get(user:decay,Alpha),
		checkduplicates(Depth,MinCheck,Episode,Alpha),
		writeln(checkduplicates(Depth,MinCheck,Episode,Alpha))
	)
	;
	true
	),%printkeyp(KeyPrec),
	!.


removecurrent(Old) :-
	(
		recorded(Old,Fact,K),
		(Fact=current(_);Fact=current(_)~=_;Fact=action(_)),
		erase(K),
		fail;
		true
	).
clean_episode(E,N) :-
	(
		between(0,N,T),
		term_to_atom(p(T,E),Key),
		eraseall(Key),
		fail;
		true
	).





plan(StartEpisode,PBRW,Abstract,Init,Goal,NumEp,MaxD,Avg,FinalT,BAction) :-
	bb_put(terminated,false),
	bb_put(avgreward,0.0),
	bb_put(numterminated,0),
	bb_put(numavg,0),
	bb_put(movingavg,0),
	bb_put(explore,false),
	bb_put(listactions,[]),
	N is NumEp+StartEpisode,
	bb_get(user:epsilon_min,EpsilonMin),
	bb_get(user:epsilon_max,EpsilonMax),
	bb_get(user:lambda_final,Lambda_Final),
	bb_get(user:lambda_init,Lambda_Init),
	bb_get(user:wheuristic_final,WHeuFinal),
	bb_get(user:wheuristic_init,WHeuInit),
%	bb_get(user:bestaction,StrategyBestAction),
	forall(between(StartEpisode,N,E),
	(
		abolish_all_tables,
		writeln('--------------------------------------------------------------------------------------'),
		%Epsilon is 0.05,%0.15*(N-E)/N, %Epsilon is 0.05,%max(0,1/E^0.5)+0.01,
%		(
%		E=<N-40 ->
		Epsilon is max(0,EpsilonMin+(EpsilonMax-EpsilonMin)*(N-E)/(N-StartEpisode+1)), %max(0,1/E^0.5)
%		;
%		Epsilon is EpsilonMin
%		),
		Lambda is min(max(0, Lambda_Final+(Lambda_Init-Lambda_Final)*(N-E)/(N-StartEpisode+1) ),1),
		WHeu is WHeuFinal+(WHeuInit-WHeuFinal)*(N-E)/(N-StartEpisode+1),
		D is min(MaxD,round(E*MaxD/N)+1),
%		trace,
		( (random<0.0,bb_get(explore,(DDD,KK)),DDD>1) ->

			(
			writeln(exploration),
			DD is DDD,
			writeln('index 1 will not work!'),
			term_to_atom(p(DD,E),Key),dcpf:plaincopyparticles(1,Key),% index 1 will not work!
			generate_episodeTD(Epsilon,E,DD,DD,1,V,R,_,Key2Explore,_),
			bb_put(explore,false)
			)


			;
			(
			DD is MaxD,% min(MaxD,round(E/50)),%max(10,MaxD-round(E/50)),
			%writeln(sample_episode(WHeu,PBRW,Abstract,Lambda,Init,Goal,Epsilon,E,DD,R,Key2Explore,Term,BAction,Steps)),
			sample_episode(WHeu,PBRW,Abstract,Lambda,Init,Goal,Epsilon,E,DD,R,Key2Explore,Term,BAction,Steps),
			(ground(Key2Explore) -> bb_put(explore,Key2Explore);bb_put(explore,false))
			)
		),
		bb_get(avgreward,OldR),
		bb_get(numterminated,OldTerm),
		bb_get(numavg,NumEpAvg),
		bb_get(movingavg,OldMovAvg),
		NewMovAvg is OldMovAvg+0.15*(R-OldMovAvg),
		bb_put(movingavg,NewMovAvg),
		writeln(movingavg(NewMovAvg)),
		(
		E>N-30 ->
		(
			bb_get(listactions,OldListBactions),
			dcpf:sum_list_multidim(OldListBactions,[1:BAction],NewListBactions),
			bb_put(listactions,NewListBactions),
			NewR is OldR + R,
			NewNumEpAvg is NumEpAvg+1,
			bb_put(numavg,NewNumEpAvg),
			bb_put(avgreward,NewR),
			bb_put(lastbestaction,BAction),
			(Term==true ->
				(
				NT is OldTerm+1,
				bb_put(numterminated,NT)
				)
				;
				true
			)
		)
		;
		true
		),
		writeln((E,DD,Epsilon,WHeu,NewR))
	)%,fail;true
	),
	bb_delete(avgreward,FinalR),
	bb_delete(numterminated,FinalT),
	bb_delete(numavg,FinalNumEpAvg),
	bb_delete(lastbestaction,BAction1),
	bb_delete(listactions,ListBactions),
	dcpf:findmax(NN:Mostused,ListBactions),
	bb_get(user:execaction,ExecAction),
	(ExecAction==best -> BAction=BAction1;BAction=Mostused),
	Avg is FinalR/FinalNumEpAvg,
	writeln(avgR(Avg)),
	writeln(terminated(FinalT,ListBactions,most(Mostused),best(BAction1))),
	writeln('--------------------------------------------------------------------------------------'),!.

sample_fromgoal :-
	abolish_all_tables,
	dcpf:bb_get(offset,Offset1),
	Key1 is Offset1+1,
	distributionalclause:proof_query_backward(Key1,findall_forward(AA,current(adm(AA)),ActionList)),
	(
	ActionList==[]->
	(writeln(noaction),printkeyp(Key1),trace)
	;
	true
	),
	dcpf:sample(uniform(ActionList),Act),
	writeln((ActionList,Act)),
	dcpf:step_particle([Act],[],[],1,1),!.


sample_episode(WHeu,PBRW,Abstract,Lambda,Init,Goal,Epsilon,E,D,EReward,Key2Explore,Term,BAction,ST) :-
	!,
	DD is D+1,
	clean_episode(E,DD),
	%init_particle(1),
	(E<PBRW ->
	(
		writeln('not implemented!!'),
		init_particle(Goal,1),

		Times is round(E/10)+1,

		forall(between(1,Times,CC),sample_fromgoal),!,

		printp(1),
		(
		between(1,100,Rep),
%		writeln(rep(Rep,ActionList)),
		sample_fromgoal,
		abolish_all_tables,
		dcpf:bb_get(offset,Offset2),
		Key2 is Offset2+1,
		\+distributionalclause:proof_query_backward(Key2,current(stop))
		),!,
		printkeyp(Key2),
		writeln(rep(Rep)),
		dcpf:bb_get(dcpf:offset,Offset),I is Offset+1
	)
	;
	(
		I=Init
		%init_particle(Init,1) % observation(on(5,4))~= true,
%		step_particle([],[],1)
	)
	),
	retractall(user:timestep(_)),
	assert(user:timestep(0)),
	timestep(T),
	term_to_atom(p(D,E),Key),
	dcpf:plaincopyparticles(I,Key),
	term_to_atom(p(DD,E),KeyInit),
	dcpf:plaincopyparticles2(I,KeyInit),
	abolish_all_tables,
%	writeln(time(T)),

%	printkeyp(Key),
	%findall(action(move(A,B)),(member(A,[1,2,3]),member(B,[1,2,3,table]),A\=B),ListActions),
	%findall(action(move(A,B)),(between(1,5,Trials),sample(gaussian([0,0],[0.004,0,0,0.004]),(A,B))),ListActions),

	(generate_episodeTD(WHeu,PBRW,Abstract,Lambda,Epsilon,E,D,D,1,V,EReward,Term,Key2Explore,ExpectedV,BAction,_)),
	% plotepisode(E,D),
	bb_get(user:limit_previous_episodes,LPE),
	MinE is max(1,E-LPE),
	% plotV(MinE,E,DD),
	term_to_atom(p(0,E),TempKey),
	(recorded(TempKey,terminated(_,_,ST),_) -> true;ST=D),
	writeln(reward(EReward,PBRW,Abstract,Lambda,V,ExpectedV)),
	!.

simpleplan(PBRW,Abstract,Init,Goal,Nepisodes,MaxD,Avg,FinalT,BAction) :-
	init_particle(Init,1),
	printp(1),
	bb_put(currentepisode,1),
	bb_put(rewardrealplan,0.0),
	dcpf:bb_get(dcpf:offset,Offset),I is Offset+1,
	eraseall(realplan),
	dcpf:plaincopyparticles(I,realplan),
	writeln(plan(1,PBRW,Abstract,realplan,Goal,Nepisodes,MaxD,Avg,FinalT,BAction)),
	plan(1,PBRW,Abstract,realplan,Goal,Nepisodes,MaxD,Avg,FinalT,BAction),
	writeln(plan(1,PBRW,Abstract,realplan,Goal,Nepisodes,MaxD,Avg,FinalT,BAction)).
%	writeparam.

% todo add discount! now 1 as default to compute the total reward
resamplingplan(PBRW,Abstract,Init,Goal,N,MaxD,TotalR,FinalT,MaxDSearch,From,End) :-
	init_particle(Init,1),
	printp(1),
	bb_put(currentepisode,From),
	bb_put(rewardrealplan,0.0),
	MaxD2 is MaxD-1,
	bb_get(user:ratiofirststep,Ratio),
	bb_put(planstop,false),
	bb_get(user:discount,Discount),
	forall((between(0,MaxD2,T),bb_get(planstop,false)),
	(
		eraseall(realplan),
		dcpf:bb_get(dcpf:offset,Offset1),I1 is Offset1+1,
		dcpf:plaincopyparticles(I1,realplan),

		CurrentD is min(MaxDSearch,MaxD-T), %%
		bb_get(currentepisode,StartEpisode),
		(T==0 ->
			Rt is Ratio;Rt is 1
		),
		Nepisodes is round(Rt*N/(MaxD2+Ratio)),%round(0.5*N/(MaxD+1)/(T+1)^2+N/(MaxD+1)+1),%round(0.1*N/(T+1)^2+4/5*N/MaxD)+1, %round(N/MaxD),%round(0.5*N/(T+1)^2)+1,%N-T*2,%
		writeln(ep(Nepisodes,T)),
		%writeln(Nepisodes is round(0.5*N/(MaxD+1)/(T+1)^2+N/(MaxD+1))+1),
		writeln(plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,_,BAction)),nl,nl,
		plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,_,BAction),
		writeln(plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,_,BAction)),nl,nl,
		NewStart is StartEpisode+Nepisodes+1,
		bb_put(currentepisode,NewStart),

		dcpf:bb_get(dcpf:offset,Offset),
		I is Offset+1,
		writeln(execute(BAction)),

		eraseall(I),
		dcpf:plaincopyparticles(realplan,I),

		recorda(realplan,BAction,_),
		abolish_all_tables,
		distributionalclause:proof_query_backward(realplan,current(reward) ~= R),

		printkeyp(realplan),
		writeln(rewardstate(R)),
		bb_get(rewardrealplan,OldTR),
		NewTR is OldTR+R,%*Discount^T, % no discount!
		bb_put(rewardrealplan,NewTR),
		%printp(1),nl,

		(distributionalclause:proof_query_backward(realplan,current(stop))-> bb_put(planstop,true);step_particle([BAction],[],1)),
		nl,writeln(afteraction),
		%printkeyp(1),
		%nl,
		%printkeyp(2),
		printp(1),nl,
		writeln(numep(Nepisodes,NewStart))
	)%,fail;true
	),
	bb_get(planstop,FinalT),
	bb_delete(currentepisode,End),
	bb_get(rewardrealplan,TotalR),
	writeln(rewardrealplan(TotalR)).

executedplan_start :-
	writeln(executedplan_start),
	getparam(Instance),
	init_particle(1),
	bb_put(currentepisode,1),
	bb_put(rewardrealplan,0.0),
	bb_put(exectime,-1),
	(bb_delete(execbestaction,_);true).
/*
	PBRW=0,
	Goal=[],
	MaxD2 is MaxD-1,
	init_particle(Init,1),
	printp(1),
	bb_put(currentepisode,1),
	bb_put(rewardrealplan,0.0),
	bb_put(exectime,0),
	T=0,
	bb_get(user:ratiofirststep,Ratio),
	bb_get(user:discount,Discount),
	%((between(0,MaxD2,T),bb_get(planstop,false)),
	eraseall(realplan),
	dcpf:bb_get(dcpf:offset,Offset1),I1 is Offset1+1,
	dcpf:plaincopyparticles(I1,realplan),

	CurrentD is min(MaxDSearch,MaxD-T), %%
	bb_get(currentepisode,StartEpisode),
	(T==0 ->
		Rt is Ratio;Rt is 1
	),
	Nepisodes is round(Rt*N/(MaxD2+Ratio)),%round(0.5*N/(MaxD+1)/(T+1)^2+N/(MaxD+1)+1),%round(0.1*N/(T+1)^2+4/5*N/MaxD)+1, %round(N/MaxD),%round(0.5*N/(T+1)^2)+1,%N-T*2,%
	writeln(ep(Nepisodes,T)),
	%writeln(Nepisodes is round(0.5*N/(MaxD+1)/(T+1)^2+N/(MaxD+1))+1),
	writeln(plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,_,BAction)),nl,nl,
	plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,_,BAction),
	writeln(plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,_,BAction)),nl,nl,
	NewStart is StartEpisode+Nepisodes+1,
	bb_put(currentepisode,NewStart),

	dcpf:bb_get(dcpf:offset,Offset),
	I is Offset+1,
	writeln(execute(BAction)),

	eraseall(I),
	dcpf:plaincopyparticles(realplan,I),

	recorda(realplan,BAction,_),
	abolish_all_tables,
	distributionalclause:proof_query_backward(realplan,current(reward) ~= R),

	printkeyp(realplan),
	writeln(rewardstate(R)),
	bb_get(rewardrealplan,OldTR),
	TotalR is OldTR+R*Discount^T, % no discount!
	bb_put(rewardrealplan,TotalR),
	%printp(1),nl,

	(distributionalclause:proof_query_backward(realplan,current(stop))-> STOP=true;STOP=false),
	bb_put(execbestaction,BAction).
*/
executedplan_step(BAction,Abstract,Init,N,MaxD,TotalR,T,MaxDSearch,STOP) :-
	retractall(user:noise),
	PBRW=0,
	Goal=[],
	MaxD2 is MaxD-1,
	bb_get(exectime,OldT),
	T is OldT+1,
	bb_put(exectime,T),
	between(0,MaxD2,T),
	(bb_get(execbestaction,BA)->
	step_particle([],Init,1)
	;
	step_particle([],Init,1)
	),
	printp(1),
	bb_get(user:ratiofirststep,Ratio),
	bb_get(user:discount,Discount),
	%((between(0,MaxD2,T),bb_get(planstop,false)),
	eraseall(realplan),
	dcpf:bb_get(dcpf:offset,Offset1),I1 is Offset1+1,
	dcpf:plaincopyparticles(I1,realplan),

	CurrentD is min(MaxDSearch,MaxD-T), %%
	bb_get(currentepisode,StartEpisode),
	(T==0 ->
		Rt is Ratio;Rt is 1
	),
	Nepisodes is round(Rt*N/(MaxD2+Ratio)),%round(0.5*N/(MaxD+1)/(T+1)^2+N/(MaxD+1)+1),%round(0.1*N/(T+1)^2+4/5*N/MaxD)+1, %round(N/MaxD),%round(0.5*N/(T+1)^2)+1,%N-T*2,%
	writeln(ep(Nepisodes,T)),
	%writeln(Nepisodes is round(0.5*N/(MaxD+1)/(T+1)^2+N/(MaxD+1))+1),
	writeln(plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,_,BAction)),nl,nl,
	plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,_,BAction),
	writeln(plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,_,BAction)),nl,nl,
	NewStart is StartEpisode+Nepisodes+1,
	bb_put(currentepisode,NewStart),

	dcpf:bb_get(dcpf:offset,Offset),
	I is Offset+1,
	writeln(execute(BAction)),

	eraseall(I),
	dcpf:plaincopyparticles(realplan,I),

	recorda(realplan,BAction,_),
	abolish_all_tables,
	distributionalclause:proof_query_backward(realplan,current(reward) ~= R),

	printkeyp(realplan),
	writeln(rewardstate(R)),
	bb_get(rewardrealplan,OldTR),
	TotalR is OldTR+R*Discount^T,
	bb_put(rewardrealplan,TotalR),
	%printp(1),nl,

	(distributionalclause:proof_query_backward(realplan,current(stop))-> STOP=1;STOP=0),
	bb_put(execbestaction,BAction),
	%parse_action(BAction,ActionType,IDA,ActionParam1,ActionParam2),
	!.


resamplingplan2(PBRW,Abstract,Init,Goal,N,MaxD,TotalR,FinalT,MaxDSearch,From) :-
	bb_get(user:discount,Discount),
	init_particle(Init,1),
	printp(1),
	bb_put(currentepisode,From),
	bb_put(rewardrealplan,0.0),
	MaxD2 is MaxD-1,
	Temp1 is MaxD+2,clean_episode(From,Temp1),
	(between(0,MaxD2,T),
	(
		eraseall(realplan),
		dcpf:bb_get(dcpf:offset,Offset1),I1 is Offset1+1,
		dcpf:plaincopyparticles(I1,realplan),

		CurrentD is min(MaxDSearch,MaxD-T), %%
		%bb_get(currentepisode,StartEpisode),
		Nepisodes is 1,%round(0.5*N/(MaxD+1)/(T+1)^2+N/(MaxD+1)+1),%round(0.1*N/(T+1)^2+4/5*N/MaxD)+1, %round(N/MaxD),%round(0.5*N/(T+1)^2)+1,%N-T*2,%
		writeln(ep(Nepisodes,T)),
		%writeln(Nepisodes is round(0.5*N/(MaxD+1)/(T+1)^2+N/(MaxD+1))+1),
		%writeln(plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,FinalT,BAction)),nl,nl,
		%plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,FinalT,BAction),
		%writeln(plan(StartEpisode,PBRW,Abstract,realplan,Goal,Nepisodes,CurrentD,Avg,FinalT,BAction)),nl,nl,

		abolish_all_tables,
		term_to_atom(p(CurrentD,From),KeyPrec),
		eraseall(KeyPrec),
		dcpf:plaincopyparticles(realplan,KeyPrec),
		distributionalclause:proof_query_backward(KeyPrec,findall_forward(AA,current(adm(AA)),ActionList)),
		abolish_all_tables,
		writeln(findbestfromhere),printkeyp(KeyPrec),nl,
		findbestaction(-0.0001,PBRW,Abstract,CurrentD,CurrentD,Discount,From,_,ActionList,BAction,Qaction,VarVAction,_,_,_,_,_),
		writeln(findbestaction(-0.0001,PBRW,Abstract,CurrentD,CurrentD,Discount,From,_,ActionList,BAction,Qaction,VarVAction,_,_,_,_,_)),


		%NewStart is StartEpisode+Nepisodes,
		%bb_put(currentepisode,NewStart),

		dcpf:bb_get(dcpf:offset,Offset),
		I is Offset+1,
		writeln(execute(BAction)),

		eraseall(I),
		dcpf:plaincopyparticles(realplan,I),
		%printp(1),nl,
		recorda(realplan,BAction,_),
		abolish_all_tables,
		distributionalclause:proof_query_backward(realplan,current(reward) ~= R),
		printkeyp(realplan),
		writeln(rewardstate(R)),
		bb_get(rewardrealplan,OldTR),
		NewTR is OldTR+R,%*Discount^T, % no discount!
		bb_put(rewardrealplan,NewTR),

		step_particle([BAction],[],1),
		nl,writeln(afteraction),
		printp(1),
		nl,
		writeln(numep(Nepisodes,NewStart))
	),fail;true
	),
	bb_get(rewardrealplan,TotalR),
	writeln(rewardrealplan(TotalR)).

writetofile2(Namefile,Instance,Avg,Score,STD,C,RealD,UsedD,Time,L) :-
	open(Namefile,'append',S),
	write(S,(Instance,Avg,Score,STD,C,RealD,UsedD,Time,L)),nl(S),
	close(S).

writetofile(Namefile,Instance,Avg,Score,STD,N,RealD,UsedD,Time,Note) :-
	open(Namefile,'append',S),
%	write(S,'Avg,N,RealD,UsedD,Time,Epsilon_min,Epsilon_max,Limit_previous_episodes,Max_horizon_span,Lambda_Init,Lambda_Final,Ucbv,Decay,Strategy,Pruning,WHeuInit,WHeuFinal'),nl(S),
	bb_get(user:abstraction,Abstract),
	bb_get(user:epsilon_min,Emin),
	bb_get(user:epsilon_max,Emax),
	bb_get(user:limit_previous_episodes,LPE),
	bb_get(user:max_horizon_span,MHS),
	bb_get(user:lambda_final,Lambda_Final),
	bb_get(user:lambda_init,Lambda_Init),
	bb_get(user:ucbv,Ucbv),
	bb_get(user:decay,Decay),
	bb_get(user:strategy,Strategy),
	bb_get(user:pruning,Pruning),
	bb_get(user:wheuristic_init,WHeuInit),
	bb_get(user:wheuristic_final,WHeuFinal),
	bb_get(user:execaction,ExecAction),
	bb_get(user:discretepruning,Domain),
	bb_get(user:storingstrategy,Storing),
	bb_get(user:proposal,Useproposal),
	bb_get(user:ratiofirststep,Ratio),
	bb_get(user:spant,Spant),
	write(S,(Instance,Avg,Score,STD,N,RealD,UsedD,Time,Emin,Emax,LPE,MHS,Lambda_Init,Lambda_Final,Abstract,Decay,Strategy,Pruning,WHeuInit,WHeuFinal,ExecAction,Domain,Storing,Useproposal,Ratio,Ucbv,Spant,Note)),nl(S),
	close(S).

writeparam :-
	bb_get(user:abstraction,Abstract),
	writeln(abstraction(Abstract)),
	bb_get(user:ratiofirststep,Ratio),
	writeln(ratiofirststep(Ratio)),
	bb_get(user:proposal,Useproposal),
	writeln(proposal(Useproposal)),
	bb_get(user:storingstrategy,Storing),
	writeln(storingstrategy(Storing)),
	bb_get(user:execaction,ExecAction),
	writeln(execaction(ExecAction)),
	bb_get(user:discretepruning,Domain),
	writeln(discretepruning(Domain)),
	bb_get(user:discount,Discount),
	writeln(discount(Discount)),
	bb_get(user:epsilon_min,Emin),
	bb_get(user:epsilon_max,Emax),
	writeln(epsilon_min(Emin)),
	writeln(epsilon_max(Emax)),
	bb_get(user:limit_previous_episodes,LPE),
	writeln(limit_previous_episodes(LPE)),
	bb_get(user:max_horizon_span,MHS),
	writeln(max_horizon_span(MHS)),
	bb_get(user:lambda_final,LMin),
	bb_get(user:lambda_init,LMax),
	writeln(user:lambda_final(LMin)),
	writeln(user:lambda_init(LMax)),
	bb_get(user:ucbv,UCBV),
	writeln(ucbv(UCBV)),
	bb_get(user:decay,Alpha),
	writeln(decay(Alpha)),
	bb_get(user:strategy,Strategy),
	writeln(strategy(Strategy)),
	bb_get(user:pruning,Pruning),
	writeln(pruning(Pruning)),
	bb_get(user:wheuristic_init,WHeuInit),
	writeln(user:wheuristic_init(WHeuInit)),
	bb_get(user:wheuristic_final,WHeuFinal),
	writeln(user:wheuristic_final(WHeuFinal)).

setparam(Abstract,Ratio,Useproposal,Storing,ExecAction,Domain,Discount,Epsilon_min,Epsilon_max,Limit_previous_episodes,Max_horizon_span,Lambda_Init,Lambda_Final,Ucbv,Decay,Strategy,Pruning,WHeuInit,WHeuFinal) :-
	bb_put(user:abstraction,Abstract),
	bb_put(user:ratiofirststep,Ratio),
	bb_put(user:proposal,Useproposal),
	bb_put(user:storingstrategy,Storing),
	bb_put(user:execaction,ExecAction),
	bb_put(user:discretepruning,Domain),
	bb_put(user:discount,Discount),
	bb_put(user:epsilon_min,Epsilon_min),
	bb_put(user:epsilon_max,Epsilon_max),
	bb_put(user:limit_previous_episodes,Limit_previous_episodes),
	bb_put(user:max_horizon_span,Max_horizon_span),
	bb_put(user:lambda_final,Lambda_Final),
	bb_put(user:lambda_init,Lambda_Init),
	bb_put(user:ucbv,Ucbv),
	bb_put(user:decay,Decay),
	bb_put(user:strategy,Strategy),
	bb_put(user:pruning,Pruning),
	bb_put(user:wheuristic_final,WHeuFinal),
	bb_put(user:wheuristic_init,WHeuInit).




exec1_sst(Init,C,UsedD,Sum1,MaxD) :-
	MaxD1 is MaxD-1,
	initsst(Init,1),
	bb_get(user:discount,Discount),
	bb_put(planstop,false),
	findall(R,(between(0,MaxD1,T1),D is MaxD-T1,UsedD1 is min(UsedD,D),bb_get(planstop,false),execsst(C,UsedD1,BestAction,TotalReward,R),RD is R*Discount^T1,writeln(execsst(C,UsedD1,BestAction,TotalReward,R,RD))),L1),
	sum_list(L1,Sum1),!.

fullplan_sst(File,Init,AVGFinal,Instance,MaxD,Times,UsedD,C,Notes) :-
%	UsedD=5,
%	C=1,
	bb_put(numterminatedexplans,0),
	getparam(Instance),
	statistics(runtime,_),
	findall(Sum1,(between(1,Times,X),exec1_sst(Init,C,UsedD,Sum1,MaxD),
		bb_get(numterminatedexplans,OldNumT),
		bb_get(planstop,T2),
		(T2==true -> NewNumT is OldNumT+1;NewNumT is OldNumT),
		bb_put(numterminatedexplans,NewNumT)),L),
	sum_list(L,Sum),
	length(L,Length),
	AVGFinal is Sum/Length,
	variance(L,AVGFinal,Var),

	findall(Score,(member(Elem,L),score(Instance,Elem,Score)),L2),
	sum_list(L2,SumScore),
	length(L2,LengthScore),
	AVGScore is SumScore/LengthScore,
	variance(L2,AVGScore,VarScore),
	Conf95 is 1.96*sqrt(VarScore)/sqrt(LengthScore),
	statistics(runtime,[_,Time]),
	T is round(Time/10)/100/Length,
	writeln(avgseconds(T)),
	score(Instance,AVGFinal,Score1),
	bb_get(numterminatedexplans,FinalTerm),
	writetofile2(File,Instance,AVGFinal,AVGScore,Conf95,C,MaxD,UsedD,T,(L,FinalTerm,Notes)),
	writeln((L,AVGFinal,AVGScore,Score1,Conf95,VarScore,LengthScore,FinalTerm)),
	!.


fullplan(File,Init,AVG,Instance,D,Times1,Notes) :-
	statistics(runtime,_),
	par(Instance,N,UsedD,Startp),
%	Init=[],
	bb_get(user:abstraction,Abstract),
	resamplingplan(0,Abstract,Init,[],N,D,AVG,T1,UsedD,Startp,Endp),
	writeln(resamplingplan(0,Abstract,Init,[],N,D,AVG,T1,UsedD,Startp,Endp)),
	(T1==true -> bb_put(numterminatedexplans,1);bb_put(numterminatedexplans,0)),
%	setparam(1,0,0,200,0,0.9,0.9,false,0.015,egreedy,200,0.00000001,-0.0001),
	getparam2(Instance,D2),
%	D2 is D*2,
	bb_put(currentepisode,Endp),
	Times is Times1-1,
%	Notes1='withoutrestart+100',
	Notes1='withrestart',
	findall(AVG2,
		(between(1,Times,X),
		bb_get(currentepisode,Start2),
		Start3 is Startp+100*Times,
		Start4 is 1, %Start3,% Start3,%Start2,%
		resamplingplan(0,Abstract,Init,[],D2,D,AVG2,T2,UsedD,Start4,End),
		writeln(resamplingplan(times(Times),0,Abstract,Init,[],D2,D,AVG2,T2,UsedD,Start4,End)),
		EndEpisode is Start2+End-Start4,
		bb_put(currentepisode,EndEpisode),
		bb_get(numterminatedexplans,OldNumT),
		(T2==true -> NewNumT is OldNumT+1;NewNumT is OldNumT),
		bb_put(numterminatedexplans,NewNumT)
	),L),
	statistics(runtime,[_,Time]),
	writeparam,nl,
	bb_get(currentepisode,TotalN),
	sum_list([AVG|L],Sum),
	length([AVG|L],Length),
	AVGFinal is Sum/Length,
	variance([AVG|L],AVGFinal,Var),
	STD is sqrt(Var),
	getparam(Instance),
	writeparam,

	findall(Score,(member(Elem,[AVG|L]),score(Instance,Elem,Score)),L2),
	sum_list(L2,SumScore),
	length(L2,LengthScore),
	AVGScore is SumScore/LengthScore,
	variance(L2,AVGScore,VarScore),
	Conf95 is 1.96*sqrt(VarScore)/sqrt(LengthScore),

	bb_get(numterminatedexplans,Terminated),
	T is round(Time/10)/100/Length,
	writeln(seconds(T)),
	score(Instance,AVGFinal,Score2),
	writeln(([AVG|L],AVGFinal,Score2,AVGScore+Conf95,Length,VarScore,Terminated)),
	(host_name(Host)->true;Host=unknown),
	writetofile(File,Instance,AVGFinal,AVGScore,Conf95,Startp+N+D2*Times=TotalN,D,UsedD,T,([AVG|L],Terminated,Host,Notes,Notes1)),
	!.

fullplan3(File,Init,AVG,Instance,D,Times) :-
	statistics(runtime,_),
	par(Instance,N,UsedD,Startp),
%	Init=[],
%	resamplingplan(0,false,Init,[],N,D,AVG,T1,UsedD,Startp,Endp),
%	writeln(resamplingplan(0,false,Init,[],N,D,AVG,T1,UsedD,Startp,Endp)),

%	setparam(1,0,0,200,0,0.9,0.9,false,0.015,egreedy,200,0.00000001,-0.0001),
%	getparam2(Instance,D2),
%	D2 is D*2,
	bb_put(currentepisode,Startp),
%	Times is 5,
	findall(AVG2,
		(between(1,Times,X),
		bb_get(currentepisode,Start2),
		resamplingplan(0,false,Init,[],N,D,AVG2,T2,UsedD,Startp,End),
		writeln(resamplingplan(0,false,Init,[],N,D,AVG2,T2,UsedD,Startp,End)),
		bb_put(currentepisode,End)
	),L),
	statistics(runtime,[_,Time]),
	writeparam,nl,
	bb_get(currentepisode,TotalN),
	sum_list(L,Sum),
	length(L,Length),
	AVGFinal is Sum/Length,
	variance(L,AVGFinal,Var),
	STD is sqrt(Var),
	getparam(Instance),
	writeparam,

	findall(Score,(member(Elem,L),score(Instance,Elem,Score)),L2),
	sum_list(L2,SumScore),
	length(L2,LengthScore),
	AVGScore is SumScore/LengthScore,
	variance(L2,AVGScore,VarScore),
	Conf95 is 1.96*sqrt(VarScore)/sqrt(LengthScore),


	T is round(Time/10)/100/Length,
	writeln(seconds(T)),
	score(Instance,AVGFinal,Score2),
	writetofile(File,Instance,AVGFinal,AVGScore,Conf95,Startp+N*Times=TotalN,D,UsedD,T,L),
	writeln(fullplan3(L,AVGFinal,Score2,AVGScore+Conf95)),
	!.


fullplan2(File,Init,AVG,Instance,D) :-
	par(Instance,N,UsedD,Startp),
	statistics(runtime,_),
%	Init=[],
	resamplingplan(0,false,Init,[],N,D,AVG,T1,UsedD,Startp,Endp),
	writeln(resamplingplan(0,false,Init,[],N,D,AVG,T1,UsedD,Startp,Endp)),

%	setparam(1,0,0,200,0,0.9,0.9,false,0.002,egreedy,false,-0.0001,-0.0001),
	getparam2(Instance,_),
	findall(AVG2,
		(between(0,4,X),
		Start2 is Endp,%+N+0*X,
		resamplingplan2(0,false,Init,[],0,D,AVG2,T2,UsedD,Start2),
		writeln(resamplingplan2(0,false,Init,[],0,D,AVG2,T2,UsedD,Start2))
	),L),
	writeparam,
	sum_list([AVG|L],Sum),
	length([AVG|L],Length),
	AVGFinal is Sum/Length,
	variance([AVG|L],AVGFinal,Var),
	STD is sqrt(Var),
	getparam(Instance),
	writeparam,
	statistics(runtime,[_,Time]),
	T is round(Time/10)/100,
	writeln(seconds(T)),
	score(Instance,AVGFinal,Score),
	writetofile(File,Instance,AVGFinal,Score,STD,N,D,UsedD,T,' '),
	writeln(([AVG|L],AVGFinal)),
	!.


incrementald(Init,N,SD,D,End) :-
	NH is round(N/(D-SD))+1,
	forall(between(SD,D,Hor),
	(
%	Init=[],%[observation(alive(1,1)) ~= true,observation(alive(1,3)) ~= true,observation(alive(2,1)) ~= true,observation(alive(2,2)) ~= true],
	init_particle(Init,1),
	dcpf:bb_get(dcpf:offset,Offset),I is Offset+1,
	eraseall(realplan),
	dcpf:plaincopyparticles(I,realplan),
	Startp is NH*(Hor-SD)+1,
	plan(Startp,0,false,realplan,[],NH,Hor,AVG,T1,BAction),
	writeln(plan(Startp,0,false,realplan,[],NH,Hor,AVG,T1,BAction))
	)),
	End is NH*(D+1-SD)+1,
	writeparam.



execsst(C,D,BestAction,TotalReward,R) :-
	bb_get(user:discount,Discount),
	step_sst(Discount,C,D,1,TotalReward,BestAction,R),
	(
	BestAction\=null ->
	(
	bb_get(dcpf:offset,Offset),I is Offset+1,eraseall(I),dcpf:plaincopyparticles(0,I),
	dcpf:step_particle([BestAction],[],[],1,1),printp(1)
	)
	;
	bb_put(planstop,true)
	).

initsst(Obs,Delta) :-
	init_particle(Obs,1),printp(1).
/*
% old
generate_episode(Epsilon,Episode,ActionList,0,Delta,0.0) :- !.

generate_episode(Epsilon,Episode,ActionList,Depth,Delta,TotalReward) :-
	Depth>0,
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
%	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	%write('timestep '),write(T),nl,
	abolish_all_tables,
	%write('particle '),write(I),nl,

	term_to_atom(p(Prec,Episode),KeyPrec),
	distributionalclause:proof_query_backward(KeyPrec,current(reward) ~= R),
	(
	distributionalclause:proof_query_backward(KeyPrec,current(stop)) ->
		(
			TotalReward=R,
			V=0.0,
			Likelihood=1
		)
		;
		(
			term_to_atom(p(T,Episode),KeyNext),
			eraseall(KeyNext),
			(
			random<1-Epsilon -> % epsilon-greedy -1/Episode-0.02
				findbestaction(Depth,Episode,Prec,ActionList,Action,Vaction,VarVAction)
			;
				(
				dcpf:sample(uniform(ActionList),ActionE),
				Action=[ActionE]
				)
			),
			dcpf:assert_list(KeyPrec,Action), % assert Best action
			(
			inference(backward(_)) ->
				dcpf:inferencestep_particlefilter_backward2(KeyPrec)
			;
				writeln('error, only backward inference supported')%generate_sample_particlefilter(T,MaxP)
			),
			NewDepth is Depth-1,
			eraseall(KeyNext),
			dcpf:copyparticles_core(KeyPrec,KeyNext),
			generate_episode(Epsilon,Episode,ActionList,NewDepth,Delta,V),
			Discount is 0.99,
			TotalReward is Discount*V+R,
			% compute likelihood p(next|current)
			findall(Var~=Val,recorded(KeyPrec,next(Var) ~=Val,_),L),
			eraseall(tempparticle),
			eval_weight_planning(KeyPrec,tempparticle,L,Likelihood)
		)
	),

	removecurrent(KeyPrec),

	recorda(KeyPrec,v(current,TotalReward),_),
	recorda(KeyPrec,v(next,V),_),
	recorda(KeyPrec,likelihood(Likelihood),_),
	%writeln((KeyPrec,v(current,TotalReward),v(next,V))),
	!.
*/
