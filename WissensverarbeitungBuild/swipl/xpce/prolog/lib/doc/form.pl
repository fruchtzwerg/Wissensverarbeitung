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

:- module(html_form, []).
:- use_module(library(pce)).
:- use_module(library(url)).
:- use_module(doc(emit)).
:- use_module(doc(util)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define the classes required to realise  HTML   forms.  Note  that a form
itself is *not* a parbox object (and   device). This would seem logical,
but makes it extremely difficult  to   make  it completely `absent' with
regard to layout, for example of the form is inside a <center> it should
center its content.

For these reasons, a form is  just   an  abstract object, related to its
controls and enclosing pbox using hyper-links.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(html_form, object,
		   "Represent an HTML form").

variable(method,	{get,post},	both, "Submission method").
variable(action,	name,		both, "Script URL").
variable(hidden,	sheet*,		get,  "Hidden fields").
variable(enctype,	name := 'application/x-www-form-urlencoded',
					both, "Field-encoding used").

initialise(F, Attrs:prolog, PB:parbox) :->
	"Create from HTML attributes"::
	send_super(F, initialise),
	new(_, hyper(PB, F, form, parbox)),
	apply_options(Attrs, form_option, F).

form_option(_,_) :- fail.

hidden_input(F, Name:name, Value:name) :->
	"Attach a hidden input-field to the form"::
	(   get(F, hidden, Sheet),
	    Sheet \== @nil
	->  true
	;   send(F, slot, hidden, new(Sheet, sheet))
	),
	send(Sheet, value, Name, Value).

submit(F) :->
	"Submit the form"::
	get(F, form_data, Sheet),
	(   get(F, method, get)
	->  get(F, action, Action),
	    new(S, string),
	    send(Sheet, for_all,
		 message(@prolog, add_field, S, @arg1?name, @arg1?value)),
	    send(S, prepend, Action),
	    get(F, hypered, parbox, PB),
	    send(PB?window, goto_url, S) 		% @browser
	;   send(@display, inform, 'No support for POST forms yet')
	).

add_field(String, Name, Value) :-
	www_form_encode(Name, CodedName),
	www_form_encode(Value, CodeValue),
	(   get(String, size, 0)
	->  Fmt = '?%s=%s'
	;   Fmt = '&%s=%s'
	),
	send(String, append, string(Fmt, [CodedName, CodeValue])).


form_data(F, Data:sheet) :<-
	"Get data from the form as a sheet"::
	(   get(F, hidden, Hidden),
	    Hidden \== @nil
	->  get(Hidden, clone, Data)
	;   new(Data, sheet)
	),
	send(F, send_hyper, control, fill_form, Data).

append(F, Control:object) :->
	"Associate a control to this form"::
	new(_, hyper(F, Control, control, form)).

:- pce_end_class.

:- pce_begin_class(html_text_input, text_item,
		   "HTML Input field").

initialise(I, Attributes:prolog) :->
	send_super(I, initialise, input),
	send(I, show_label, @off),
	apply_options(Attributes, input_option, I).

input_option(type(_), _).
input_option(size(W), I) :-
	send(I, length, W).
input_option(value(V), I) :-
	send(I, selection, V).

fill_form(I, Data:sheet) :->
	"Add my contribution to the form"::
	send(Data, value, I?name, I?selection).

:- pce_end_class.

:- pce_begin_class(html_submit_input, button,
		   "Submit the form").

initialise(B, Attrs:prolog) :->
	"Create from HTML attributes"::
	send_super(B, initialise, submit, message(@receiver, submit)),
	apply_options(Attrs, submit_options, B).

submit_options(value(Name), B) :-
	send(B, selection, Name).
submit_options(type(_), _).

fill_form(_B, _Data:sheet) :->
	"Nothing to do for buttons"::
	true.

submit(B) :->
	"Find form and submit it"::
	(   get(B, hypered, form, Form)
	->  send(Form, submit)
	;   send(B, error, no_form)
	).

:- pce_end_class.

		 /*******************************
		 *	     <SELECT>		*
		 *******************************/

:- pce_begin_class(html_select_menu, menu, "<select> without size").

initialise(S, Name:name, Attr:prolog, Content:prolog) :->
	send_super(S, initialise, Name, cycle),
	send(S, show_label, @off),
	select_content(Content, S),
	apply_options(Attr, select_menu_option, S).

select_menu_option(_, _).

fill_form(I, Data:sheet) :->
	"Add my contribution to the form"::
	send(Data, value, I?name, I?selection).

select_content([], _).
select_content([element(option, Attr, Content)|T], Menu) :-
	content_to_atom(Content, Atom),
	get(Atom, strip, canonicalise, Clean),
	(   option(value(Value), Attr)
	->  true
	;   Value = Clean
	),
	send(Menu, append, new(MI, menu_item(Value, @default, Clean))),
	(   option(selected, Attr)
	->  send(Menu, selection, MI)
	;   true
	),
	select_content(T, Menu).

:- pce_end_class.
