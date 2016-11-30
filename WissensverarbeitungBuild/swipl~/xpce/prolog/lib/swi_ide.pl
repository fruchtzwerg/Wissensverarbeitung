/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
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

:- module(swi_ide,
	  [ prolog_ide/0,		%
	    prolog_ide/1		% +Action
	  ]).
:- use_module(library(pce)).

/** <module> SWI-Prolog IDE controller

This module defines  the  application   @prolog_ide  and  the  predicate
prolog_ide(+Action). The major motivation is be   able  to delay loading
the IDE components to the autoloading of one single predicate.
*/

		 /*******************************
		 *    AUTOLOAD OF COMPONENTS	*
		 *******************************/

:- pce_image_directory(library('trace/icons')).

:- pce_autoload(swi_console,		library('swi/swi_console')).
:- pce_autoload(prolog_debug_status,	library('trace/status')).
:- pce_autoload(prolog_navigator,	library('trace/browse')).
:- pce_autoload(prolog_query_frame,	library('trace/query')).
:- pce_autoload(prolog_trace_exception,	library('trace/exceptions')).
:- pce_autoload(prolog_thread_monitor,	library('swi/thread_monitor')).
:- pce_autoload(prolog_debug_monitor,	library('swi/pce_debug_monitor')).
:- pce_autoload(xref_frame,		library('pce_xref')).

		 /*******************************
		 *	      TOPLEVEL		*
		 *******************************/

%%	prolog_ide(+Action)
%
%	Invoke an action on the (SWI-)Prolog  IDE application. This is a
%	predicate to ensure  optimal  delaying   of  loading  and object
%	creation for accessing the  various   components  of  the Prolog
%	Integrated Development Environment.

prolog_ide :-
	prolog_ide(open_console).

prolog_ide(Action) :-
	in_pce_thread(send(@prolog_ide, Action)).


		 /*******************************
		 *	   THE IDE CLASS	*
		 *******************************/

:- pce_global(@prolog_ide, new(prolog_ide)).
:- pce_global(@prolog_exception_window, new(prolog_trace_exception)).

:- pce_begin_class(prolog_ide, application, "Prolog IDE application").

initialise(IDE) :->
	"Create as service application"::
	send_super(IDE, initialise, prolog_ide),
	send(IDE, kind, service).

open_console(IDE) :->
	"Open SWI-Prolog Cross-Referencer frontend"::
	(   get(IDE, member, swi_console, Console)
	->  send(Console, open)
	;   new(Console, swi_console),
	    send(Console, application, IDE),
	    send(Console, wait)
	).

open_debug_status(IDE) :->
	"Open/show the status of the debugger"::
	(   get(IDE, member, prolog_debug_status, W)
	->  send(W, expose)
	;   send(prolog_debug_status(IDE), open)
	).

open_exceptions(IDE, Gui:[bool]) :->
	"Open/show exceptions"::
	W = @prolog_exception_window,
	(   object(W)
	->  send(W, expose)
	;   (   Gui == @on
	    ->	catch(tdebug, _, guitracer)
	    ;	true
	    ),
	    send(W, application, IDE),
	    send(W, open)
	).

open_navigator(IDE, Where:[directory|source_location]) :->
	"Open Source Navigator"::
	(   send(Where, instance_of, directory)
	->  get(IDE, navigator, Where, Navigator),
	    send(Navigator, directory, Where)
	;   send(Where, instance_of, source_location)
	->  get(Where, file_name, File),
	    file_directory_name(File, Dir),
	    get(Where, line_no, Line),
	    (	integer(Line)
	    ->	LineNo = Line
	    ;	LineNo = 1
	    ),
	    get(IDE, navigator, Dir, Navigator),
	    send(Navigator, goto, File, LineNo)
	;   get(IDE, navigator, directory('.'), Navigator)
	),
	send(Navigator, expose).


navigator(IDE, Dir:[directory], Navigator:prolog_navigator) :<-
	"Create or return existing navigator"::
	(   get(IDE, member, prolog_navigator, Navigator)
	->  true
	;   new(Navigator, prolog_navigator(Dir)),
	    send(Navigator, application, IDE)
	).

open_query_window(IDE) :->
	"Open window to enter a query"::
	(   get(IDE, member, prolog_query_frame, QF)
	->  true
	;   new(QF, prolog_query_frame),
	    send(QF, application, IDE)
	),
	send(QF, expose).

open_interactor(_) :->
	"Create a new interactor window"::
	interactor.

thread_monitor(IDE) :->
	"Open a monitor for running threads"::
	(   current_prolog_flag(threads, true)
	->  (   get(IDE, member, prolog_thread_monitor, Monitor)
	    ->	true
	    ;	new(Monitor, prolog_thread_monitor),
		send(Monitor, application, IDE)
	    ),
	    send(Monitor, open)
	;   send(@display, report, error,
		 'This version of SWI-Prolog is not built \n\c
		  with thread-support')
	).

debug_monitor(IDE) :->
	"Open monitor for debug messages"::
	(   get(IDE, member, prolog_debug_monitor, Monitor)
	->  true
	;   new(Monitor, prolog_debug_monitor),
	    send(Monitor, application, IDE)
	),
	send(Monitor, open).

xref(IDE) :->
	"Open Cross-Referencer frontend"::
	(   get(IDE, member, xref_frame, XREF)
	->  send(XREF, open)
	;   new(XREF, xref_frame),
	    send(XREF, application, IDE),
	    send(XREF, wait),
	    send(XREF, update)
	).

:- pce_end_class(prolog_ide).
