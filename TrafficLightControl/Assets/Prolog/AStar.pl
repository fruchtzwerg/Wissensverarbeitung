% Autor: Hannes Boers ,Tilo Zuelske nach Buchvorlage von Ivan Bratko
% Datum: 14.11.2016

:-dynamic h/2.
:-dynamic s/3.
:-dynamic goal/1.


%Definition des Zieles
goal(t).

liste([arc(e,f,5,7),arc(s,e,2,6),arc(s,a,2,6),arc(a,b,2,5),arc(b,c,2,4),arc(c,d,3,4),arc(d,t,3,3),arc(f,g,2,4),arc(g,t,2,2),arc(t,g,2,0)]).

setNewGraph(List):-
              retractall(h(_,_))
              ,
              retractall(s(_,_,_))
              ,
              createGraph(List).
                                    
createGraph([]).
createGraph([arc(N,N1,Cost,HeuristicValue)|Tail]):-
                                     assert(s(N,N1,Cost))
                                     %,
                                     %not(h(N,_))
                                     %,
                                     %assert(h(N,HeuristicValue))
                                     %,
                                     %createGraph(Tail).
                                     ,
                                     not(h(N,_)) ->
                                     assert(h(N,HeuristicValue))
                                     ,
                                     createGraph(Tail)
                                     ;
                                     createGraph(Tail).


getPath(Start,Goal,Path):-
                      retract(goal(_))
                      ,
                      assert(goal(Goal))
                      ,
                      bestfirst(Start,Path).
                                     
%arcs
%s(e,f,5).
%s(s,e,2).
%s(s,a,2).
%s(a,b,2).
%s(b,c,2).
%s(c,d,3).
%s(d,t,3).
%s(f,g,2).
%s(g,t,2).

%Funktionswerte der Heuristik
%h(e,7).
%h(a,5).
%h(b,4).
%h(c,4).
%h(f,4).
%h(g,2).
%h(d,3).
%h(t,0).

% bestfirst(Start, Solution): Solution is a path from Start to a goal
bestfirst(Start, Solution):- expand([],l(Start,0/0),9999,_,yes,Solution),!.   %Assume 9999 is > any f-value

%expand(Path,Tree,Bound,Tree1,Solved,Solution):
%Path is path between start node of search ans subtree Tree,
%Tree1 is Tree expanded within Bound
%if goal found then Solution is solution path and Solved = yes

%Case 1: goal leaf-node, construct a solution path
expand(P,l(N,_),_,_,yes,[N|P]):- goal(N).

%Case 2: leaf-node, f-value less than Bound
%Generate successors and expand them within Bound
expand( P, l(N,F/G), Bound, Tree1, Solved, Sol)  :-
  F  =<  Bound,
  (  bagof( M/C, ( s(N,M,C), \+ member(M,P) ), Succ),  %not member
     !,                                    % Node N has successors
     succlist( G, Succ, Ts),               % Make subtrees Ts
     bestf( Ts, F1),                       % f-value of best successor
     expand( P, t(N,F1/G,Ts), Bound, Tree1, Solved, Sol)
     ;
     Solved = never                        % N has no successors - dead end
  ) .
                                         
%Case 3: non-leaf, f-value less than Bound
%Expand the most promising subtree, depending on
%results, procedure continue will decide how to proceed
expand(P,t(N,F/G,[T|Ts]),Bound,Tree1,Solved,Sol):-
                                                F=<Bound
                                                ,
                                                bestf(Ts,BF)
                                                ,
                                                min(Bound,BF,Bound1) %Bound1 = min(Bound,BF)
                                                ,
                                                expand([N|P],T,Bound1,T1,Solved1,Sol)
                                                ,
                                                continue(P,t(N,F/G,[T1|Ts]),Bound,Tree1,Solved1,Solved,Sol).

%Case 4: non-leaf with empty subtrees
%This is a dead end which will never be solved
expand(_,t(_,_,[]),_,_,never,_):-!.

%Case 5: value greater than bound
%Tree may not grow
expand(_,Tree,Bound,Tree,no,_):-f(Tree,F),F>Bound.

%continue(Path,Tree,Bound,NewTree,SubtreeSolved,TreeSolved,Solution)
continue(_,_,_,_,yes,yes,Sol).

continue(P,t(N,F/G,[T1|Ts]),Bound,Tree1,no,Solved,Sol):-
                                                 insert(T1,Ts,NTs)
                                                 ,
                                                 bestf(NTs,F1)
                                                 ,
                                                 expand(P,t(N,F1/G,NTs),Bound,Tree1,Solved,Sol).
                                                 
continue(P,t(N,F/G,[_|Ts]),Bound,Tree1,never,Solved,Sol):-
                                                 bestf(Ts,F1)
                                                 ,
                                                 expand(P,t(N,F1/G,Ts),Bound,Tree1,Solved,Sol).
                                                 
%succlist(G0,[Node1/Cost1,...],[l(BestNode,BestF/G),...]):
%make list of search leaves orderedn by their f-values
succlist(_,[],[]).

succlist(G0,[N/C|NCs],Ts):-
                         G is G0 + C
                         ,
                         h(N,H)  %Heuristic term h(N)
                         ,
                         F is G + H
                         ,
                         succlist(G0,NCs,Ts1)
                         ,
                         insert(l(N,F/G),Ts1,Ts).
                         
% Insert T into list of trees Ts preserving oder with respect to f-values
insert(T,Ts,[T|Ts]):-
                   f(T,F)
                   ,
                   bestf(Ts,F1)
                   ,
                   F=<F1
                   ,
                   !.
                   
insert(T,[T1|Ts],[T1|Ts1]):-insert(T,Ts,Ts1).

%Extract f-value
f(l(_,F/_),F).  %f-value of a leaf
f(t(_,F/_,_),F).  %f-value of atree

bestf([T|_],F):-f(T,F).  %best f-value of a list of trees
bestf([],9999).  %No trees: bad f-value

min( X, Y, X)  :-
  X  =<  Y, !.

min( X, Y, Y).


                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         