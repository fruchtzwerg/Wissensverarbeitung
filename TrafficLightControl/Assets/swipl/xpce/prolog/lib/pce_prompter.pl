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

:- module(pce_prompter,
	  [ prompter/2
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_report)).
:- require([ delete/3
	   , maplist/3
	   , term_to_atom/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a standard prompter-box for PCE applications.  It is
invoked with:

    prompter(+Tile, +ListOfAttributes)

where each attribute is a term of the form

   +Label:+Type = -Value[/+Default]

Examples:

?- prompter('Create class',
	    [ name:name = Name,
	      super:name = Super
	    ]).

Last updated:	Wed Sep 13 1995 by Jan Wielemaker
		- Added menu/browser for multiple values.
		- Added automatic stretching of dialog items on resize
		- Improved type and error handling

		Thu Aug  1 1996 by Jan Wielemaker
		- Fixed passing quoted types such as '0..100'
		- Added support for sliders for int- and real-ranges.

		Sat May 18 2002 by Jan Wielemaker
		- Use report-dialog for messages
		- Properly handle clicking away to window
		- Make it a transient window

NOTE:	Package is under development.  Needs support for more types;
	optional/obligatory fields and better error-messages.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@prompter, make_promper).

make_promper(P) :-
	new(P, dialog),
	send(P, resize_message, message(@prolog, stretch_items, P)),
	send(new(report_dialog), below, P),
	send(P, done_message, message(P, return, cancel)).

prompter(Title, Attributes) :-
	maplist(canonicalise_attribute, Attributes, CAtts),
	send(@prompter, clear),
	maplist(append_prompter(@prompter), CAtts),
	send(@prompter, append,
	     new(Ok, button(ok, message(@prompter, return, ok))), next_row),
	send(Ok, default_button, @on),
	send(@prompter, append,
	     button(cancel, message(@prompter, return, cancel))),
	get(@prompter, frame, Frame),
	send(Frame, label, Title),
	send(@prompter, fit),
	stretch_items(@prompter),
	(   send(@event, instance_of, event)
	->  get(@event, position, @display, Pos),
	    get(@event?receiver, frame, MainFrame),
	    send(Frame, transient_for, MainFrame),
	    send(Frame, modal, transient)
	;   Pos = @default
	),
	repeat,
	    (	get(@prompter, confirm_centered, Pos, OK)
	    ;	get(@prompter, confirm, OK) % second time
	    ),
	    (   OK == ok
	    ->  maplist(read_prompter(@prompter), CAtts), !,
	        send(@prompter, show, @off)
	    ;   !,
		send(@prompter, show, @off),
		fail
	    ).

canonicalise_attribute(Label:Type = Value, Label:PceType = Value) :-
	pce_type(Type, PceType).

pce_type(Type, Type) :-
	atom(Type), !.
pce_type(Term, Type) :-
	term_to_atom(Term, A0),
	atom_codes(A0, S0),
	delete(S0, 0' , S1),
	atom_codes(Type, S1).


		 /*******************************
		 *   STRETCH ITEMS TO THE RIGHT *
		 *******************************/

stretch_items(Dialog) :-
	send(Dialog?graphicals, for_all,
	     message(@prolog, stretch_item, @arg1)).

stretchable(text_item).
stretchable(list_browser).

stretch_item(Item) :-
	get(Item, class_name, ClassName),
	stretchable(ClassName),
	\+ (get(Item, right, RI), RI \== @nil), !,
	get(Item?device?visible, right_side, Right),
	get(Item?device?gap, width, W),
	R is Right - W,
	get(Item, left_side, L),
	Width is R - L,
	send(Item, do_set, width := Width).
stretch_item(_).


		/********************************
		*      CREATE DIALOG ITEMS	*
		********************************/

append_prompter(P, Label:Type = Value) :-
	make_dialog_item(Type, Label, DI),
	set_default(Value, DI),
	send(P, append, DI).

						  % TBD: specialised types
make_dialog_item(Type, Label, DI) :-
	get(@pce, convert, Type, type, PceType),
	get(PceType, kind, Kind),
	dialog_item_from_type_kind(Kind, PceType, Label, DI), !.
make_dialog_item(Type, Label, DI) :-
	get(@pce, convert, Type, type, PceType),
	get(PceType, value_set, Set), !,
	get(Set, size, Size),
	(   Size < 6
	->  new(DI, menu(Label, choice))
	;   new(DI, list_browser(@default, 10, 6)),
	    send(DI, label, Label),
	    send(DI, name, Label)
	),
	send(Set, for_all, message(DI, append, @arg1)).
make_dialog_item(Type, Label, DI) :-
	new(DI, text_item(Label, '')),
	send(DI, type, Type).


dialog_item_from_type_kind(int,   Type, Label, DI) :- !,
	new(DI, int_item(Label)),
	send(DI, type, Type).
dialog_item_from_type_kind(Range, Type, Label, DI) :-
	(   Range == int_range
	;   Range == real_range
	), !,
	get(Type?context, first, Low),
	get(Type?context, second, High),
	new(DI, slider(Label, Low, High, (Low+High)/2)).


		/********************************
		*          SET DEFAULTS		*
		********************************/

set_default(Value, DI) :-
	nonvar(Value),
	Value = _RVal/Default, !,
	send(DI, selection, Default).
set_default(_, _).

		/********************************
		*           READ VALUES		*
		********************************/

read_prompter(P, Label:Type = Value) :-
	get(P, member, Label, DI),
	(   get(DI, selection, V0)
	->  canonicalise(DI, V0, V1),
	    (   get(@pce, convert, V1, Type, Val)
	    ->  (   nonvar(Value),
		    Value = RVal/_
		->  RVal = Val
		;   Value = Val
		)
	    ;   send(DI, report, warning, 'Invalid value for %s', Label),
		fail
	    )
	;   send(DI, report, warning, 'No selection for %s', Label),
	    fail
	).


canonicalise(DI, A, B) :-
	send(DI, instance_of, text_item), !,
	get(A, strip, B).
canonicalise(DI, A, B) :-
	send(DI, instance_of, list_browser), !,
	get(A, key, B).
canonicalise(_, Val, Val).				  % TBD
