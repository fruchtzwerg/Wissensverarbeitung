/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (C): 1985-2011, University of Amsterdam
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

:- module(pce_compatibility_layer,
	  [ auto_call/1,
	    callable_predicate/1,
	    modified_since_last_loaded/1,
	    pce_error/1,
	    pce_warn/1,
	    pce_info/1
	  ]).

/** <module> XPCE Compatibility layer

This layer defines some predicates to   enhance portability with SICStus
and Quintus Prolog. These systems are  no   longer  supported, but it is
probably wise to keep this layer for `just-in-case'.
*/

:- meta_predicate
	auto_call(0),
	callable_predicate(:).

%%	auto_call(:Goal)
%
%	Autoload Goal and call it.  In   SWI-Prolog,  this  simply means
%	calling it.

auto_call(G) :-
	call(G).


		 /*******************************
		 *      DIALOG EDITOR SUPPORT	*
		 *******************************/

%%	callable_predicate(:Head) is semidet.
%
%	Succeeds if Head can be called without raising an exception for
%	an undefined predicate

callable_predicate(M:Head) :-
	callable(Head),
	functor(Head, Name, Arity),
	current_predicate(M:Name/Arity).

%%	modified_since_last_loaded(Path) is semidet.
%
%	True is file has been modified since the last time it was loaded.

modified_since_last_loaded(File) :-
	'$time_source_file'(File, LoadTime, user), !,
	time_file(File, Modified),
	Modified > LoadTime.
modified_since_last_loaded(InFile) :-
	'$time_source_file'(File, LoadTime, user),
	same_file(InFile, File), !,
	time_file(File, Modified),
	Modified > LoadTime.


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- consult(library('english/pce_messages')).

:- multifile
	prolog:message/3.

prolog:message(Spec) -->
	pce_message(Spec).
prolog:message(context_error(Goal, Context, What)) -->
	[ '~w: ~w '-[Goal, What] ],
	pce_message_context(Context).
prolog:message(type_error(Goal, ArgN, Type, _Value)) -->
	[ '~w: argument ~w must be a ~w'-[Goal, ArgN, Type], nl ].

%%	pce_error(+Term) is det.
%%	pce_warn(+Term) is det.
%%	pce_info(+Term) is det.
%
%	Portability layer wrappers around print_message/2.

pce_error(Term) :-
	(   current_prolog_flag(xref, true)
	->  true
	;   print_message(error, Term)
	).

pce_warn(Term) :-
	(   current_prolog_flag(xref, true)
	->  true
	;   print_message(warning, Term)
	).

pce_info(Term) :-
	print_message(informational, Term).


