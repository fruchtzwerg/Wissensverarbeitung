% Autor: Hannes Boers ,Tilo Zuelske nach Buchvorlage von Ivan Bratko
% Datum: 14.11.2016

:-dynamic h/2.
:-dynamic s/3.
:-dynamic goal/1.

:-set_prolog_flag(answer_write_options,
                   [ quoted(true),
                     portray(true),
                     spacing(next_argument)
                   ]).


liste([arc(l1_start,l1_bahnhofstr,5,350),
       arc(l1_bahnhofstr,l1_sparkasse,5,207),
       arc(l1_sparkasse,l1_bushalte,5,317),
       arc(l1_bushalte,l1_jungfernstieg,5,534),
       arc(l1_jungfernstieg,l1_barther_a,5,308),
       arc(l1_barther_a,l1_barther_b,5,366),
       arc(l1_barther_b,l1_barther_b,5,0),
       arc(l1_jungfernstieg,l1_pflegeheim,5,460),
       arc(l1_pflegeheim,l1_pflegeheim,5,0),
       arc(l2_start,l2_bahnhofstr_a,5,355),
       arc(l2_bahnhofstr_a,l2_bahnhofstr_b,5,201),
       arc(l2_bahnhofstr_b,l2_bahnhofstr_end,5,355),
       arc(l2_bahnhofstr_end,l2_bahnhofstr_end,5,0),
       arc(l2_start,l2_bahnhofstr_straight,5,349),
       arc(l2_bahnhofstr_straight,l2_sparkasse,5,306),
       arc(l2_sparkasse,l2_bus,5,753),
       arc(l2_bus,l2_hbf_straight_a,5,276),
       arc(l2_hbf_straight_a,l2_hbf_straight_b,5,197),
       arc(l2_hbf_straight_b,l2_bahn,5,261),
       arc(l2_bahn,l2_end,5,407),
       arc(l2_end,l2_end,5,0),
       arc(l2_bus,l2_hbf_left,5,275),
       arc(l2_hbf_left,l2_hbf_park,5,242),
       arc(l2_hbf_park,l2_hbf_park,5,0),
       arc(l2_sparkasse,l1_bushalte,5,225),
       arc(l3_start,l3_bahn,5,297),
       arc(l3_bahn,l3_hbf_a,5,362),
       arc(l3_hbf_a,l3_hbf_b,5,235),
       arc(l3_hbf_b,l3_hbf_c,5,255),
       arc(l3_hbf_c,l3_bus,5,428),
       arc(l3_bus,l3_bus_left,5,284),
       arc(l3_bus_left,l2_sparkasse,5,45),
       arc(l3_bus,l3_bahnhofstr,5,374),
       arc(l3_bahnhofstr,l3_end,5,583),
       arc(l3_end,l3_end,5,0),
       arc(l4_start,l4_bahn,5,326),
       arc(l4_bahn,l4_hbf_a,5,324),arc(l4_hbf_a,l4_hbf_b,5,232),arc(l4_hbf_b,l4_hbf_c,5,262),arc(l4_hbf_c,l4_bahnhofstr_a,5,784),arc(l4_bahnhofstr_a,l4_bahnhofstr_b,5,288),arc(l4_bahnhofstr_b,l4_end,5,298),arc(l4_end,l4_end,5,0),arc(l4_bahnhofstr_a,l2_bahnhofstr_b,5,167),arc(l4_hbf_a,l2_hbf_park,5,189),arc(barther_start,barther_matratzen,5,471),arc(barther_matratzen,l2_hbf_straight_b,5,152),arc(barther_matratzen,l3_hbf_b,5,237),arc(barther_matratzen,l4_hbf_b,5,258),arc(jungfernstieg_start,jungfernstieg_ampel,5,315),arc(jungfernstieg_ampel,l1_barther_a,5,179),arc(jungfernstieg_ampel,l2_hbf_straight_a,5,200),arc(jungfernstieg_ampel,l3_hbf_c,5,285),arc(jungfernstieg_ampel,l4_hbf_c,5,315),arc(jungfernstieg_ampel,l2_hbf_left,5,220),arc(bahnhofstr_start,bahnhofstr_right,5,335),arc(bahnhofstr_right,l4_bahnhofstr_b,5,166),arc(bahnhofstr_start,bahnhofstr_left,5,337),arc(bahnhofstr_left,l2_sparkasse,5,311),arc(bahnhofstr_left,l1_sparkasse,5,270),arc(hbf_start,l3_hbf_c,5,183),arc(hbf_start,l4_hbf_c,5,175)]).

%Definition des Zieles
goal(N).

%Das setzen eines neuen Graphen/Baumes von C# aus
setNewGraph(List):-
              retractall(h(_,_))
              ,
              retractall(s(_,_,_))
              ,
              createGraph(List).
%Prädikat dient Modifizierung der Wissensbank, hierbei werden die von C#
%übergebenen Arc Daten in zwei für den A* Algorithmus verwendbare Prädikate
%aufgesplittet und in der Wissenbank hinterlegt.

%Abbruchbedingung
createGraph([]).
createGraph([arc(N,N1,Cost,HeuristicValue)|Tail]):-
                                     assert(s(N,N1,Cost))
                                     ,
                                     not(h(N,_)) ->
                                     assert(h(N,HeuristicValue))
                                     ,
                                     createGraph(Tail)
                                     ;
                                     createGraph(Tail).

%Prädikat dient der Anfrage des besten Pfades, aus C# heraus.
%Dabei sind Namen der Start und Zielknoten anzugeben.
%Als Ergebnis erhält C# die Wegbeschreibung von Knoten Start zu Ziel.
getPath(Start,Goal,Path):-
                      retractall(goal(_))
                      ,
                      assert(goal(Goal))
                      ,
                      bestfirst(Start,Path).
%Test
updateNode(Node,Node1,NewCost):-
                      retract(s(Node,Node1,_))
                      ,
                      assert(s(Node,Node1,NewCost)).


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
     (expand( P, t(N,F1/G,Ts), Bound, Tree1, Solved, Sol)
     ;
     Solved = never)                        % N has no successors - dead end
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


                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         