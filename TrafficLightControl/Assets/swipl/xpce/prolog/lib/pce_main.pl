/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 1985-2013, University of Amsterdam
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

:- module(pce_main,
	  [ pce_loop/2,			% :Goal, +Argv
	    pce_loop/1,			% :Goal
	    pce_main_loop/1,		% :Goal
	    dispatch_for_frames/1	% +FrameList
	  ]).

:- meta_predicate
	pce_loop(:),
	pce_loop(:, +),
	pce_main_loop(:).

:- use_module(library(pce)).
:- use_module(library(pce_util)).
:- require([ append/3
	   , call/2
	   , ignore/1
	   , unix/1
	   , chain_list/2
	   ]).

%%	pce_main_loop(+Goal)
%
%	Simple XPCE runtime toplevel loop.  This goal extracts the command
%	line arguments, calls `call(Goal, CmdLineArgs)' and waits for all
%	frames created by this call to be invisible.  Then it will halt/0.

pce_main_loop(Goal) :-
	setup_runtime,
	current_prolog_flag(argv, Argv),
	pce_loop(Goal, Argv),
	halt.

%%	pce_loop(+Goal).
%%	pce_loop(+Goal, +Argv:list).
%
%	Runs `Goal', finds all toplevel frames created and then dispatches
%	events untill the last frame is destroyed.

pce_loop(Goal) :-
	pce_loop(Goal, []).
pce_loop(Goal, Argv) :-
	get(@display?frames, find_all, @arg1?kind == toplevel, FramesOld),
	call(Goal, Argv),
	get(@display?frames, find_all, @arg1?kind == toplevel, FramesNew),
	get(FramesNew, copy, FrameChain),
	send(FrameChain, subtract, FramesOld),
	chain_list(FrameChain, Frames),
	dispatch_for_frames(Frames).

dispatch_for_frames([]) :- !.
dispatch_for_frames(Frames) :-
	(   catch(send(@display, dispatch), E,
		  (   message_to_string(E, Msg),
		      send(@display, inform, Msg),
		      (	  E == '$aborted'
		      ->  throw(E)
		      ;	  true
		      )
		  ))
	->  true
	;   true
	),
	existing_frames(Frames, Existing),
	dispatch_for_frames(Existing).

existing_frames([], []).
existing_frames([H|T0], [H|T]) :-
	object(H),
	send(H, instance_of, frame),
	get(H, status, Status),
	Status \== unmapped, !,
	existing_frames(T0, T).
existing_frames([_|T0], T) :-
	existing_frames(T0, T).

setup_runtime :-
	(   get(@pce, is_runtime_system, @on)
	->  true
	;   send(@pce, trap_errors, @off)
	),
	catch(set_prolog_flag(debug_on_error, false), _, true).


