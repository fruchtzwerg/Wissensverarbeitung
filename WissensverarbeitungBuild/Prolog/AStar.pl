% Autor: Hannes Boers ,Tilo Zuelske nach Buchvorlage von Ivan Bratko
% Datum: 14.11.2016
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Konfiguration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%dynamische Praedikate
:-dynamic h/2.
:-dynamic s/3.
:-dynamic goal/1.

%Ausgabe der Listen ohne "..." Abkuerzung
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
%bevor das geschieht, wird die Heuristik und die Graphendefinitionen erstmal geloescht
%setNewGraph(Liste der Graphen und Heuristiken)

% example arc list [arc(l1_start,l1_bahnhofstr,1,350),arc(l1_bahnhofstr,l1_sparkasse,1,207),arc(l1_sparkasse,l1_bushalte,1,317),arc(l1_bushalte,l1_jungfernstieg,1,534),arc(l1_jungfernstieg,l1_barther_a,1,308),arc(l1_barther_a,l1_barther_b,1,366),arc(l1_barther_b,l1_barther_end,1,206),arc(l1_barther_end,l1_barther_end,1,0),arc(l1_jungfernstieg,l1_pflegeheim_a,1,198),arc(l1_pflegeheim_a,l1_pflegeheim_b,1,296),arc(l1_pflegeheim_b,l1_pflegeheim_b,1,0),arc(l1_bushalte,l1_bus_start,1,93),arc(l1_bus_start,l1_bus_end,1,435),arc(l1_bus_end,l1_barther_a,1,317),arc(l1_bus_end,l1_pflegeheim_a,1,186),arc(l2_start,l2_bahnhofstr_a,1,355),arc(l2_bahnhofstr_a,l2_bahnhofstr_b,1,201),arc(l2_bahnhofstr_b,l2_bahnhofstr_end,1,355),arc(l2_bahnhofstr_end,l2_bahnhofstr_end,1,0),arc(l2_start,l2_bahnhofstr_straight,1,349),arc(l2_bahnhofstr_straight,l2_sparkasse,1,306),arc(l2_sparkasse,l2_bus,1,753),arc(l2_bus,l2_hbf_straight_a,1,276),arc(l2_hbf_straight_a,l2_hbf_straight_b,1,197),arc(l2_hbf_straight_b,l2_bahn,1,261),arc(l2_bahn,l2_end,1,407),arc(l2_end,l2_end,1,0),arc(l2_bus,l2_hbf_left,1,275),arc(l2_hbf_left,l2_hbf_park,1,242),arc(l2_hbf_park,l2_hbf_park,1,0),arc(l2_bus,l2_u,10,136),arc(l2_u,l3_hbf_c,1,163),arc(l3_hbf_c,l3_bus,2,428),arc(l3_bus,l3_bus_left,10,284),arc(l3_bus_left,l2_sparkasse,1,45),arc(l3_bus,l3_bahnhofstr,1,374),arc(l3_bahnhofstr,l3_end,1,583),arc(l3_end,l3_end,1,0),arc(l2_u,l4_hbf_c,1,184),arc(l4_hbf_c,l4_bus,1,139),arc(l4_bus,l4_bahnhofstr_a,1,645),arc(l4_bahnhofstr_a,l4_bahnhofstr_b,1,288),arc(l4_bahnhofstr_b,l4_end,1,298),arc(l4_end,l4_end,1,0),arc(l4_bahnhofstr_a,l2_bahnhofstr_b,1,167),arc(l4_bus,l4_bus_start,1,140),arc(l4_bus_start,l4_bus_end,1,492),arc(l4_bus_end,l2_bahnhofstr_b,1,163),arc(l4_bus_end,l4_bahnhofstr_b,1,306),arc(l2_sparkasse,l1_bushalte,1,225),arc(l3_start,l3_bahn,1,297),arc(l3_bahn,l3_hbf_a,1,362),arc(l3_hbf_a,l3_hbf_b,1,235),arc(l3_hbf_b,l3_hbf_c,1,255),arc(l4_start,l4_bahn,1,326),arc(l4_bahn,l4_hbf_a,1,324),arc(l4_hbf_a,l4_hbf_b,1,232),arc(l4_hbf_b,l4_hbf_c,1,263),arc(l4_hbf_a,l2_hbf_park,1,189),arc(barther_start,barther_matratzen,1,471),arc(barther_matratzen,l2_hbf_straight_b,1,152),arc(barther_matratzen,l3_hbf_b,1,237),arc(barther_matratzen,l4_hbf_b,1,258),arc(jungfernstieg_start,jungfernstieg_ampel,1,315),arc(jungfernstieg_ampel,l1_barther_a,1,179),arc(jungfernstieg_ampel,l2_hbf_straight_a,1,200),arc(jungfernstieg_ampel,l3_hbf_c,1,285),arc(jungfernstieg_ampel,l4_hbf_c,1,313),arc(jungfernstieg_ampel,l2_hbf_left,1,220),arc(bahnhofstr_start,bahnhofstr_right,1,335),arc(bahnhofstr_right,l4_bahnhofstr_b,1,166),arc(bahnhofstr_start,bahnhofstr_left,1,337),arc(bahnhofstr_left,l2_sparkasse,1,311),arc(bahnhofstr_left,l1_sparkasse,1,270),arc(hbf_start,l3_hbf_c,1,183),arc(hbf_start,l4_hbf_c,1,178)]

setNewGraph(List):-
              retractall(heuristicFunction(_,_)),
              retractall(node(_,_,_)),
              createGraph(List).

%Praedikat dient Modifizierung der Wissensbank, hierbei werden die von C#
%Uebergebenen Arc Daten in zwei fuer den A* Algorithmus verwendbare Praedikate
%aufgesplittet und in der Wissenbank hinterlegt.
%createGraph([arc(Next,Node2,Kosten zwischen den Node,Heuristischer Wert von Next)|Tail])
%Abbruchbedingung
createGraph([]).
createGraph([arc(Node,Next,Cost,HeuristicValue)|Tail]):-
                                     assert(node(Node,Next,Cost)),
                                     not(heuristicFunction(Node,_)) ->
                                     assert(heuristicFunction(Node,HeuristicValue)),
                                     createGraph(Tail)
                                     ;
                                     createGraph(Tail).

%Praedikat dient der Anfrage des besten Pfades, aus C# heraus.
%Dabei sind Namen der Start und Zielknoten anzugeben.
%Als Ergebnis erhaelt C# die Wegbeschreibung von Knoten Start zu Ziel.
%getPath(Start,Ziel,Weg zum Start vom Ziel)
path(Start,Goal,Path):-
                      retractall(goal(_)),
                      assert(goal(Goal)),
                      bestFirst(Start,Path).
%Test
%falls sich die heuristischen Werte aendern oder ein Knoten hinzu kommt,
%kann man diese jetzt updaten
%updateNode(Next,Node2,neueKosten zwischen Next und Node2)
updateNode(Node,Next,NewCost):-
                      retract(node(Node,Next,_)),
                      assert(node(Node,Next,NewCost)).


% bestFirst(Start, Path): Path is a path from Start to a goal
bestFirst(Start, Path):- expand([], leaf(Start, 0/0), 999, _, yes, Path),!.   %Assume 9999 is > any f-value

%expand(Path,Tree,Bound,SubtreeNew,Solved,Path):
%Path is path between start node of search and subtree Tree,
%SubtreeNew is Tree expanded within Bound
%if goal found then Path is solution path and Solved = yes

%Case 1: goal is leaf-node, construct a solution path
expand(Path,leaf(Node,_),_,_,yes,[Node|Path]):- goal(Node).

%Case 2: leaf-node, f-value less than Bound
%Generate successors and expand them within Bound
expand(Path, leaf(Node,F/CostSum), Bound, SubtreeNew, Solved, Solution)  :-
  F =< Bound,
  (bagof(Next/Cost, (node(Node,Next,Cost), \+ member(Next,Path) ), Successors),  %not member
     !,                                    % Node N has successors
     orderedSuccessorList(CostSum, Successors, Subtree),               % Make subtrees Subtree
     bestF(Subtree, FNew),                       % f-value of best successor
     (expand(Path, tree(Node,FNew/CostSum,Subtree), Bound, SubtreeNew, Solved, Solution)
     ;
     Solved = never)                        % N has no successors - dead end
  ).

%Case 3: non-leaf, f-value less than Bound
%Expand the most promising subtree, depending on
%results, procedure continue will decide how to proceed
expand(Path,tree(Node,F/CostSum,[Head|Tail]),Bound,Subtree,Solved,Solution):-
                                                F=<Bound,
                                                bestF(Tail,BestF),
                                                min(Bound,BestF,Min),
                                                expand([Node|Path],Head,Min,SubtreeNew,SolvedNew,Solution),
                                                continue(Path,tree(Node,F/CostSum,[SubtreeNew|Tail]),Bound,Subtree,SolvedNew,Solved,Solution).

%Case 4: non-leaf with empty subtrees
%This is a dead end which will never be solved
expand(_,tree(_,_,[]),_,_,never,_):-!.

%Case 5: value greater than bound
%Tree may not grow
expand(_,Tree,Bound,Tree,no,_):-f(Tree, F),
                                F>Bound.

%continue(Path,Tree,Bound,NewTree,SubtreeSolved,TreeSolved,Path)
continue(_,_,_,_,yes,yes,Solution).

continue(Path,tree(Node,F/CostSum,[Subtree|Tail]),Bound,SubtreeNew,no,Solved,Solution):-
                                                 insert(Subtree,Tail,SubtreesNew),
                                                 bestF(SubtreesNew,FNew),
                                                 expand(Path,tree(Node,FNew/CostSum,SubtreesNew),Bound,SubtreeNew,Solved,Solution).

continue(Path,tree(Node,F/CostSum,[_|Tail]),Bound,SubtreeNew,never,Solved,Solution):-
                                                 bestF(Tail,FNew),
                                                 expand(Path,tree(Node,FNew/CostSum,Tail),Bound,SubtreeNew,Solved,Solution).

%orderedSuccessorList(CostSum,[Next/Cost1,...],[l(BestNode,BestF/CostSumNew),...]):
%make list of search leaves ordered by their f-values
orderedSuccessorList(_,[],[]).

orderedSuccessorList(CostSum,[Node/Cost|Tail],Subtrees):-
                         CostSumNew is CostSum + Cost,
                         heuristicFunction(Node,H),  %Heuristic term h(N),
                         F is CostSumNew + H,
                         orderedSuccessorList(CostSum,Tail,SubtreesNew),
                         insert(leaf(Node,F/CostSumNew),SubtreesNew,Subtrees).

% Insert T into list of trees Tail preserving order with respect to f-values
insert(Subtree,Tail,[Subtree|Tail]):-
                   f(Subtree,F),
                   bestF(Tail,FNew),
                   F=<FNew,
                   !.

insert(Tree,[Subtree|Tail],[Subtree|Tail1]):-insert(Tree,Tail,Tail1).

%Extract f-value
f(leaf(_,F/_),F).  %f-value of a leaf
f(tree(_,F/_,_),F).  %f-value of atree

bestF([Tree|_],F):-f(Tree,F).  %best f-value of a list of trees
bestF([],999).  %No trees: bad f-value

min(X, Y, X)  :-
  X  =<  Y, !.

min(X, Y, Y).
