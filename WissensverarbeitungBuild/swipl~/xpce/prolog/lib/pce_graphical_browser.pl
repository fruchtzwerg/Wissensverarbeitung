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

:- module(pce_graphical_browser, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).

:- pce_begin_class(graphical_browser, window,
		   "List-browser for graphicals").

variable(select_message,	code*,	      both, "Executed on select").
variable(open_message,		code*,	      both, "double-click").
variable(multiple_selection,	bool := @off, both, "single/multiple").
variable(single_column,		bool := @off, get,  "Formated or column").
variable(render_function,	[function],   get,  "Create rendering").
variable(unrender_function,	[function],   get,  "Get rendered object").
variable(gap,			[size],	      get,  "Gap between objects").

initialise(B, W:[int], H:[int], RF:[function], UF:[function]) :->
	default(W, 200, TheW),
	default(H, 150, TheH),
	send(B, send_super, initialise, @default, size(TheW, TheH)),
	send(B, slot, render_function, RF),
	send(B, slot, unrender_function, UF),
	send(B, scrollbars, vertical),
	send(B, resize_message, message(B, resized, @arg2)),
	send(B?decoration, attribute, hor_stretch, 100),
	send(B, recogniser, @graphical_browser_recogniser).


resized(B, Size:size) :->
	"Recompute format after resize"::
	(   get(B, single_column, @off)
	->  new(Format, format(horizontal, Size?width, @off))
	;   new(Format, format(horizontal, 1, @on))
	),
	(   get_object(B, gap, size(W, H))
	->  send(Format, column_sep, W),
	    send(Format, row_sep, H)
	;   true
	),
	send(B, format, Format).


single_column(B, SC:bool) :->
	"Layout as paragraph or column"::
	(   get(B, single_column, SC)
	->  true
	;   send(B, slot, single_column, SC),
	    send(B, resized, B?size)
	).


gap(B, Gap:[size]) :->
	"Change spacing between objects"::
	send(B, slot, gap, Gap),
	send(B, resized, B?size).


:- pce_group(render).

render(B, Obj:any, Rendering:graphical) :<-
	"Create a graphical representation for an object"::
	get(B, render_function, Func),
	(   Func == @default
	->  Rendering = Obj
	;   get(Func, '_forward', Obj, Rendering)
	).

unrender(B, Rendering:graphical, Obj:any) :<-
	"Get the object from its graphical representation"::
	get(B, unrender_function, Func),
	(   Func == @default
	->  Obj = Rendering
	;   get(Func, '_forward', Rendering, Obj)
	).

represents(B, Gr:graphical, Obj:any) :->
	"Test if graphical represents object"::
	get(B, unrender, Gr, Obj).


:- pce_group(set).

append(B, Obj:any) :->
	"Append graphical to browser"::
	get(B, render, Obj, Gr),
	send(B, display, Gr).

delete(B, Obj:any) :->
	"Delete from brwoser"::
	get(B?graphicals, find, message(B, represents, @arg1, Obj), Gr),
	send(Gr, device, @nil).

members(B, Objs:chain) :->
	"->clear and fill with new members"::
	send(B, clear),
	send(Objs, for_all, message(B, append, @arg1)).
members(B, Objs:chain) :<-
	"Chain holding member graphicals"::
	get(B?graphicals, map, ?(B, unrender, @arg1), Objs).

member(B, Obj:any) :->
	"Test membership"::
	get(B?graphicals, find, message(B, represents, @arg1, Obj), _Gr).

:- pce_group(selection).

selection(B, Sel:'any|chain') :<-
	"Fetch selected object(s)"::
	get(B, get_super, selection, Sel0),
	(   get(B, multiple_selection, @off)
	->  (   get(Sel0, size, 1)
	    ->	get(Sel0, head, Sel1),
		get(B, unrender, Sel1, Sel)
	    ;	fail
	    )
	;   get(Sel0, map, ?(B, unrender, @arg1), Sel)
	).

:- pce_group(event).

:- pce_global(@graphical_browser_recogniser,
	      new(handler_group(click_gesture(left, '', single,
					      message(@receiver, clicked,
						      select_message, @event)),
				click_gesture(left, s, single,
					      message(@receiver, shift_clicked,
						      @event)),
				click_gesture(left, '', double,
					      message(@receiver, clicked,
						      open_message, @event))))).

clicked(B, MsgName:name, Ev:event) :->
	"Select item and forward message"::
	get(B, find, Ev, Gr),
	get(B, MsgName, Msg),
	(   MsgName == select_message
	->  send(B, selection, Gr),
	    (   Msg \== @nil
	    ->	send(Msg, forward, Gr)
	    ;	true
	    )
	;   (   Msg \== @nil
	    ->	send(Msg, forward, Gr)
	    ;	true
	    )
	).

shift_clicked(B, Ev:event) :->
	"Toggle selected status of item"::
	get(B, multiple_selection, @on),
	get(B, find, Ev, Gr),
	send(Gr, toggle_selected).

:- pce_end_class.

/*
test :-
	send(new(B, graphical_browser), open),
	send(B, members,
	     chain(box(100,100), bitmap('pce.bm'), circle(10))).
*/
