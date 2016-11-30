/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

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

:- module(pce_global,
	[ pce_global/2				  % Ref x Goal
	]).

:- meta_predicate
      pce_global(+, :).

:- use_module(pce_boot(pce_principal)).

:- require([strip_module/3, gensym/2, append/3]).

:- dynamic
	'pce global goal'/3,			  % Ref, Module, Goal
	'pce catcher'/2.			  % Module, Goal


		/********************************
		*            USER PART		*
		********************************/

%%	pce_global(+Object, :Goal) is det.
%
%	Register Goal to be a goal that creates @Reference

pce_global(@Ref, MGoal) :-
	strip_module(MGoal, Module, Goal),
	global(Ref, Module, Goal).

global(Ref, Module, Goal) :-
	var(Ref), !,
	retractall('pce catcher'(Module, Goal)),
	asserta('pce catcher'(Module, Goal)).
global(Ref, Module, Goal) :-			  % just reconsult
	'pce global goal'(Ref, Module, Goal), !,
	(   Goal = new(_)
	->  true
	;   reload_global(@Ref)
	).
global(Ref, Module, Goal) :-
	'pce global goal'(Ref, Module, _), !,	  % definition changed
	reload_global(@Ref),
	retractall('pce global goal'(Ref, Module, _)),
	asserta('pce global goal'(Ref, Module, Goal)).
global(Ref, _M1, new(Term)) :-			  % same definition
	'pce global goal'(Ref, _M2, new(Term)), !.
global(Ref, M1, G1) :-
	'pce global goal'(Ref, M2, G2), !,
	print_message(warning, object_already_defined(Ref, M2)),
	retractall('pce global goal'(Ref, M2, G2)),
	asserta('pce global goal'(Ref, M1, G1)).
global(Ref, Module, Goal) :-
	asserta('pce global goal'(Ref, Module, Goal)).

reload_global(Ref) :-
	object(Ref), !,
	(   get(Ref, references, 0)
	->  free(Ref)
	;   Ref = @Name,
	    gensym(Name, NewName),
	    send(Ref, name_reference, NewName),
	    print_message(informational, renamed_reference(Name, NewName))
	).
reload_global(_).


		/********************************
		*            SYSTEM		*
		********************************/

register_handler :-
   send(@pce?exception_handlers,
	append(attribute(undefined_assoc,
			 message(@prolog, call, trap_ref, @arg1)))).

:- initialization
	register_handler.

trap_ref(Ref) :-
	'pce global goal'(Ref, Module, Goal), !,
	(   Goal = new(Term)
	->  (   new(@Ref, Module:Term)
	    ->  true
	    ;   print_message(error, create_failed(Term)),
		trace,
	        fail
	    )
	;   Goal =.. List,
	    append(List, [@Ref], GoalList),
	    GoalTerm =.. GoalList,
	    (	Module:GoalTerm
	    ->  true
	    ;   print_message(error, make_global_failed(Module:GoalTerm)),
		trace,
		fail
	    )
	).
trap_ref(Ref) :-
	'pce catcher'(Module, Goal),
	call(Module:Goal, Ref).
