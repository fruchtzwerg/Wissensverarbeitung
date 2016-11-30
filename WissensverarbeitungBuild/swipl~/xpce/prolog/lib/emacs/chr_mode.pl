/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 1985-2014, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(emacs_chr_mode, []).
:- use_module(library(pce)).
:- use_module(prolog_mode).
:- use_module(library(operators)).	% push/pop operators
:- use_module(library(chr)).		% get CHR operators.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module deals with colourisation of  .chr files. CHR introduces many
operators and requires different rules for colouring objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	      CHR MODE		*
		 *******************************/

:- emacs_begin_mode(chr, prolog,
		    "Mode for editing Constraint Handling Rules (CHR) documents",
		    % BINDINGS
		    [
		    ],
		    % SYNTAX TABLE
		    [
		    ]).

colourise_buffer(M) :->
	"Cross-reference the buffer and set up colours"::
	push_chr_operators,
	call_cleanup(send_super(M, colourise_buffer),
		     pop_chr_operators).

:- emacs_end_mode.


		 /*******************************
		 *	   SYNTAX RULES		*
		 *******************************/

:- multifile
	prolog_colour:term_colours/2,
	prolog_colour:goal_colours/2.

%%	term_colours(+Term, -Colours)
%
%	Colourisation of a toplevel term as read from the file.

term_colours((_Name @ Rule), delimiter - [ identifier, RuleColours ]) :- !,
	term_colours(Rule, RuleColours).
term_colours((Rule pragma _Pragma), delimiter - [RuleColours,pragma]) :- !,
	term_colours(Rule, RuleColours).
term_colours((Head <=> Body), delimiter - [ HeadColours, BodyColours ]) :- !,
	chr_head(Head, HeadColours),
	chr_body(Body, BodyColours).
term_colours((Head ==> Body), delimiter - [ HeadColours, BodyColours ]) :- !,
	chr_head(Head, HeadColours),
	chr_body(Body, BodyColours).

chr_head(_C#_Id, delimiter - [ head, identifier ]) :- !.
chr_head((A \ B), delimiter - [ AC, BC ]) :- !,
	chr_head(A, AC),
	chr_head(B, BC).
chr_head((A, B), functor - [ AC, BC ]) :- !,
	chr_head(A, AC),
	chr_head(B, BC).
chr_head(_, head).

chr_body((Guard|Goal), delimiter - [ GuardColour, GoalColour ]) :- !,
	chr_body(Guard, GuardColour),
	chr_body(Goal, GoalColour).
chr_body(_, body).


%%	goal_colours(+Goal, -Colours)
%
%	Colouring of special goals.

goal_colours(constraints(Decls), deprecated-[DeclColours]) :-
	chr_constraint_colours(Decls, DeclColours).
goal_colours(chr_constraint(Decls), built_in-[DeclColours]) :-
	chr_constraint_colours(Decls, DeclColours).
goal_colours(chr_type(TypeDecl), built_in-[DeclColours]) :-
	chr_type_decl_colours(TypeDecl, DeclColours).

chr_constraint_colours(Var, instantiation_error(Var)) :-
	var(Var), !.
chr_constraint_colours((H,T), classify-[HeadColours,BodyColours]) :- !,
	chr_constraint_colours(H, HeadColours),
	chr_constraint_colours(T, BodyColours).
chr_constraint_colours(PI, Colours) :-
	pi_to_term(PI, Goal), !,
	Colours = predicate_indicator-[ goal(constraint(0), Goal),
					arity
				      ].
chr_constraint_colours(Goal, Colours) :-
	atom(Goal), !,
	Colours = goal(constraint(0), Goal).
chr_constraint_colours(Goal, Colours) :-
	compound(Goal), !,
	compound_name_arguments(Goal, _Name, Args),
	maplist(chr_argspec, Args, ArgColours),
	Colours = goal(constraint(0), Goal)-ArgColours.

chr_argspec(Term, mode(Mode)-[chr_type(Type)]) :-
	compound(Term),
	compound_name_arguments(Term, Mode, [Type]),
	chr_mode(Mode).

chr_mode(+).
chr_mode(?).
chr_mode(-).

pi_to_term(Name/Arity, Term) :-
	atom(Name), integer(Arity), Arity >= 0, !,
	functor(Term, Name, Arity).

chr_type_decl_colours((Type ---> Def), built_in-[chr_type(Type), DefColours]) :-
	chr_type_colours(Def, DefColours).
chr_type_decl_colours((Type == Alias), built_in-[chr_type(Type), chr_type(Alias)]).

chr_type_colours(Var, classify) :-
	var(Var), !.
chr_type_colours((A;B), control-[CA,CB]) :- !,
	chr_type_colours(A, CA),
	chr_type_colours(B, CB).
chr_type_colours(T, chr_type(T)).

prolog_colour:term_colours(Term, Colours) :-
	term_colours(Term, Colours).
prolog_colour:goal_colours(Term, Colours) :-
	goal_colours(Term, Colours).

		 /*******************************
		 *	   SYNTAX HOOKS		*
		 *******************************/

:- multifile
	emacs_prolog_mode:alternate_syntax/3.


emacs_prolog_mode:alternate_syntax(chr,
				   emacs_chr_mode:push_chr_operators,
				   emacs_chr_mode:pop_chr_operators).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we could generalise this to deal with all included files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	chr_operators/1.

push_chr_operators :-
	(   chr_operators(Ops)
	->  true
	;   init_chr_operators(Ops),
	    assert(chr_operators(Ops))
	),
	'$set_source_module'(SM, SM),
	push_operators(SM:Ops).

pop_chr_operators :-
	pop_operators.

init_chr_operators(Ops) :-
	absolute_file_name(library('chr/chr_op'),
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   OpFile),
	open(OpFile, read, In),
	read(In, Term),
	read_ops(Term, In, Ops),
	close(In).

read_ops(end_of_file, _, []) :- !.
read_ops((:- op(Pre, Ass, Ops)), In, [ op(Pre, Ass, Ops) |T]) :- !,
	read(In, T2),
	read_ops(T2, In, T).
read_ops(_, In, Ops) :-
	read(In, T2),
	read_ops(T2, In, Ops).
