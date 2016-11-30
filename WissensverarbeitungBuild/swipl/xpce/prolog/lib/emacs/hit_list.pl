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

:- module(emacs_hit_list, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).

/** <module> PceEmacs class to show error search location
*/

:- pce_begin_class(emacs_hit_list, frame,
		   "Represent result of find, errors, etc.").

variable(expose_on_append, bool := @off, both,
	 "->expose on ->append_hit").
variable(clear_on_append,  bool := @off, both,
	 "Clear on ->append_hit after usage").
variable(used,		   bool := @off, both,
	 "->goto has been used").
variable(message,	   name := caret, both,
	 "Method to apply").

class_variable(confirm_done, bool, @off).

initialise(L, Label:[string]) :->
	"Create from label"::
	default(Label, 'Compilation errors', FrameLabel),
	send_super(L, initialise, FrameLabel),
	send(L, append, new(B, browser('', size(60, 6)))),
	send(B, select_message, message(L, goto, @arg1?object)),
	send(new(D, dialog), below, B),
	send(D, pen, 0),
	send(D, gap, size(10, 5)),
	send(D, append, button(quit, message(L, destroy))),
	send(D, append, label(reporter), right).


unlink(L) :->
	"Remove fragments from the buffers"::
	send(L, clear),
	send_super(L, unlink).

open(L) :->
	"Open, if possible as a transient window for PceEmacs"::
	debug(emacs, 'Open ~p~n', [L]),
	(   object(@emacs),
	    get(@emacs, current_frame, Frame)
	->  get(Frame, area, area(X,Y,W,_H)),
	    send(L, transient_for, Frame),
	    send(L, create),
	    get(L, size, size(BW,_)),
	    send_super(L, open, point(X+W-BW,Y+20))
	;   send_super(L, open)
	).

browser(L, Browser:list_browser) :<-
	get(L, member, browser, B),
	get(B, list_browser, Browser).


clear(L) :->
	"Clear browser and delete fragments"::
	get(L?browser, dict, Dict),
	send(Dict, for_all, message(@arg1?object, free)),
	send(Dict, clear),
	send(L, used, @off).


append_hit(L, Buffer:emacs_buffer, Start:int, Len:[int], Msg:[char_array]) :->
	"Append a hit to the hit-list"::
	(   get(L, expose_on_append, @on)
	->  send(L, expose)
	;   true
	),
	(   get(L, clear_on_append, @on),
	    get(L, used, @on)
	->  send(L, clear)
	;   true
	),
	(   Len == @default
	->  get(Buffer, scan, Start, line, 0, end, EOL),
	    FragLength is EOL - Start
	;   FragLength = Len
	),
	get(Buffer, line_number, Start, LineNo),
	(   Msg == @default
	->  get(Buffer, contents, Start, FragLength, String)
	;   String = Msg
	),
	get(Buffer, name, BufName),
	get(L, browser, ListBrowser),
	send(ListBrowser, append,
	     new(DI, dict_item('',
			       string('%s:%d: %s',
				      BufName, LineNo, String),
			       new(F, fragment(Buffer, Start, FragLength))))),
	new(_, emacs_mark_hyper(DI, F, dict_item, fragment)),
	send(ListBrowser, normalise, DI).

goto(L, Fragment:fragment) :->
	"Indicate the fragment"::
	send(L, used, @on),
	get(Fragment, text_buffer, TB),
	get(TB, open, tab, Frame),
	get(Frame, editor, Editor),
	get(L, message, Method),
	send(Editor, Method, Fragment?start).

:- pce_end_class.

:- pce_begin_class(emacs_mark_hyper, hyper).

unlink_to(H) :->
	get(H, from, From),
	free(From),
	free(H).
unlink_from(H) :->
	get(H, to, To),
	free(To),
	free(H).

:- pce_end_class.


