% Autor: Hannes Boers ,Tilo Zuelske nach Buchvorlage von Ivan Bratko
% Datum: 14.11.2016
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Konfiguration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%dynamische Pr�dikate
:-dynamic h/2.
:-dynamic s/3.
:-dynamic goal/1.

%Ausgabe der Listen ohne ... Abk�rzung
:-set_prolog_flag(answer_write_options,
                   [ quoted(true),
                     portray(true),
                     spacing(next_argument)
                   ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Definition des Zieles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
goal(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Das setzen eines neuen Graphen/Baumes von C# aus
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setNewGraph(List):-
              retractall(heuristicFunction(_,_))
              ,
              retractall(defineNodeAndArc(_,_,_))
              ,
              createGraph(List).

%Pr�dikat dient Modifizierung der Wissensbank, hierbei werden die von C#
%�bergebenen Arc Daten in zwei f�r den A* Algorithmus verwendbare Pr�dikate
%aufgesplittet und in der Wissenbank hinterlegt.
%Abbruchbedingung
createGraph([]).
createGraph([arc(N,N1,Cost,HeuristicValue)|Tail]):-
                                     assert(defineNodeAndArc(N,N1,Cost))
                                     ,
                                     not(heuristicFunction(N,_)) ->
                                     assert(heuristicFunction(N,HeuristicValue))
                                     ,
                                     createGraph(Tail)
                                     ;
                                     createGraph(Tail).

%Pr�dikat dient der Anfrage des besten Pfades, aus C# heraus.
%Dabei sind Namen der Start und Zielknoten anzugeben.
%Als Ergebnis erh�lt C# die Wegbeschreibung von Knoten Start zu Ziel.
getPath(Start,Goal,Path):-
                      retractall(goal(_))
                      ,
                      assert(goal(Goal))
                      ,
                      bestfirst(Start,Path).
%Test
updateNode(Node,Node1,NewCost):-
                      retract(defineNodeAndArc(Node,Node1,_))
                      ,
                      assert(defineNodeAndArc(Node,Node1,NewCost)).


% bestfirst(Start, Solution): Solution is a path from Start to a goal
bestfirst(Start, Solution):- expand([],leaf(Start,0/0),9999,_,yes,Solution),!.   %Assume 9999 is > any f-value

%expand(Path,Tree,Bound,Tree1,Solved,Solution):
%Path is path between start node of search and subtree Tree,
%Tree1 is Tree expanded within Bound
%if goal found then Solution is solution path and Solved = yes

%Case 1: goal leaf-node, construct a solution path
expand(P,leaf(N,_),_,_,yes,[N|P]):- goal(N).

%Case 2: leaf-node, f-value less than Bound
%Generate successors and expand them within Bound
expand( P, leaf(N,F/G), Bound, Tree1, Solved, Sol)  :-
  F  =<  Bound,
  (  bagof( M/C, (defineNodeAndArc(N,M,C), \+ member(M,P) ), Succ),  %not member
     !,                                    % Node N has successors
     succlist( G, Succ, Ts),               % Make subtrees Ts
     bestf( Ts, F1),                       % f-value of best successor
     (expand( P, tree(N,F1/G,Ts), Bound, Tree1, Solved, Sol)
     ;
     Solved = never)                        % N has no successors - dead end
  ) .

%Case 3: non-leaf, f-value less than Bound
%Expand the most promising subtree, depending on
%results, procedure continue will decide how to proceed
expand(P,tree(N,F/G,[T|Ts]),Bound,Tree1,Solved,Sol):-
                                                F=<Bound
                                                ,
                                                bestf(Ts,BF)
                                                ,
                                                min(Bound,BF,Bound1) %Bound1 = min(Bound,BF)
                                                ,
                                                expand([N|P],T,Bound1,T1,Solved1,Sol)
                                                ,
                                                continue(P,tree(N,F/G,[T1|Ts]),Bound,Tree1,Solved1,Solved,Sol).

%Case 4: non-leaf with empty subtrees
%This is a dead end which will never be solved
expand(_,tree(_,_,[]),_,_,never,_):-!.

%Case 5: value greater than bound
%Tree may not grow
expand(_,Tree,Bound,Tree,no,_):-f(Tree,F),F>Bound.

%continue(Path,Tree,Bound,NewTree,SubtreeSolved,TreeSolved,Solution)
continue(_,_,_,_,yes,yes,Sol).

continue(P,tree(N,F/G,[T1|Ts]),Bound,Tree1,no,Solved,Sol):-
                                                 insert(T1,Ts,NTs)
                                                 ,
                                                 bestf(NTs,F1)
                                                 ,
                                                 expand(P,tree(N,F1/G,NTs),Bound,Tree1,Solved,Sol).

continue(P,tree(N,F/G,[_|Ts]),Bound,Tree1,never,Solved,Sol):-
                                                 bestf(Ts,F1)
                                                 ,
                                                 expand(P,tree(N,F1/G,Ts),Bound,Tree1,Solved,Sol).

%succlist(G0,[Node1/Cost1,...],[l(BestNode,BestF/G),...]):
%make list of search leaves orderedn by their f-values
succlist(_,[],[]).

succlist(G0,[N/C|NCs],Ts):-
                         G is G0 + C
                         ,
                         heuristicFunction(N,H)  %Heuristic term h(N)
                         ,
                         F is G + H
                         ,
                         succlist(G0,NCs,Ts1)
                         ,
                         insert(leaf(N,F/G),Ts1,Ts).

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
f(leaf(_,F/_),F).  %f-value of a leaf
f(tree(_,F/_,_),F).  %f-value of atree

bestf([T|_],F):-f(T,F).  %best f-value of a list of trees
bestf([],9999).  %No trees: bad f-value

min( X, Y, X)  :-
  X  =<  Y, !.

min( X, Y, Y).
