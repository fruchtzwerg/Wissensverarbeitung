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

:- module(key_binding,
	  [ show_key_bindings/1
	  ]).
:- use_module(library(pce)).
:- require([ auto_call/1
	   , atomic_list_concat/2
	   , portray_object/2
	   , send_list/3
	   , term_to_atom/2
	   ]).


:- pce_global(@show_key_bindings_recogniser,
	      make_show_key_bindings_recogniser).

make_show_key_bindings_recogniser(R) :-
	new(R, popup_gesture(new(P, popup(options)))),
	send_list(P, append,
		  [ menu_item(documentation,
			      message(@prolog, show_documentation, @arg1),
			      end_group := @on),
		    menu_item(source,
			      message(@prolog, show_source, @arg1))
		  ]).

show_source(TextImage) :-
	method(TextImage, Method),
	auto_call(editpce(Method)).

show_documentation(TextImage) :-
	method(TextImage, Method),
	auto_call(manpce(Method)).

method(TextImage, Method) :-
	get(TextImage, window, View),
	get(View, hypered, target, Target),
	get(TextImage, index, View?focus_event, Here),
	get(View, scan, Here, line, 0, start, StartOfLine),
	get(View?text_buffer, find, StartOfLine, '\t', StartOfSelector),
	get(View, word, StartOfSelector, Selector),
	get(Target, send_method, Selector, tuple(_, Method)).


		 /*******************************
		 *	    ENTRY POINT		*
		 *******************************/

show_key_bindings(Object) :-
	new(V, view),
	send(V?image, cursor, arrow),
	send(new(D, dialog), below, V),
	send(D, append, button(quit, message(V, destroy))),
	send(D, append,
	     new(B, button(apply,
			   and(message(@prolog, show_key_bindings,
				       Object, V, D?apropos_member?selection),
			       message(@receiver, active, @off))))),
	send(D, append,
	     text_item(apropos, new(R, regex(''))), right),
	send(B, active, @off),
	send(D, default_button, apply),
	send(V, tab_stops, vector(100, 300)),
	send(V?image, wrap, none),
	send(V?image, recogniser, @show_key_bindings_recogniser),
	show_key_bindings(Object, V, R),
	send(V, open).


show_key_bindings(Table, View, Pattern) :-
	send(Table, instance_of, key_binding), !,
	get(Table, name, Label),
	send(View?frame, label,
	     string('Key Binding table "%s"', Label)),
	display_bindings(Table, @nil, Pattern, View),
	send(View, caret, 0).
show_key_bindings(TableName, View, Pattern) :-
	atom(TableName),
	get(@pce, convert, TableName, class, Class),
	get(Class, instance, Instance),
	send(Instance, lock_object, @on),
	get(@pce, convert, TableName, key_binding, Table),
	get(Table, name, Label),
	send(View?frame, label,
	     string('Key Binding table "%s"', Label)),
	display_bindings(Table, Instance, Pattern, View),
	send(View, caret, 0).
show_key_bindings(TableName, View, Pattern) :-
	atom(TableName), !,
	get(@pce, convert, TableName, key_binding, Table),
	show_key_bindings(Table, View, Pattern).
show_key_bindings(Editor, View, Pattern) :-
	send(Editor, has_get_method, bindings),
	get(Editor?window, label, Label),
	send(View?frame, label,
	     string('Key Bindings for editor in window "%s"', Label)),
	get(Editor, bindings, Bindings),
	display_bindings(Bindings, Editor, Pattern, View),
	send(View, caret, 0).


display_bindings(Table, Object, Pattern, Output) :-
	new(_, hyper(Output, Object, target, show_key_binding_view)),
	new(Done, chain),
	send(Output, clear),
	display_bindings(Table, Object, '', Done, Pattern, Output),
	send(Done, done).

display_bindings(Table, Object, Leader, Done, Pattern, Output) :-
	new_leader(Table, Leader, NewLeader),
	get(Table, bindings, Sheet),
	new(Lines, chain),
	send(Sheet, for_all,
	     if(not(message(Done, member, @arg1?name)),
		and(message(@prolog, display_binding,
			    Object, @arg1?name, @arg1?value, Pattern, Lines),
		    message(Done, append, @arg1?name)))),
	(   send(Lines, empty)
	->  true
	;   send(Output, appendf, '%s bindings:\n\n', NewLeader),
	    send(Lines, for_all, message(Output, append, @arg1)),
	    send(Output, appendf, '\n')
	),
	send(Lines, done),
	send(Table?defaults, for_all,
	     message(@prolog, display_bindings,
		     @arg1, Object, NewLeader, Done, Pattern, Output)).

new_leader(Table, '', Name) :-
	get(Table, name, Name),
	Name \== @nil, !.
new_leader(_, '', 'Local') :- !.
new_leader(Table, Leader, NewLeader) :-
	get(Table, name, Name),
	Name \== @nil,
	atomic_list_concat([Leader, /, Name], NewLeader).
new_leader(_, Leader, NewLeader) :-
	atomic_list_concat([Leader, /, anonymous], NewLeader).


display_binding(Editor, Key, Function, Pattern, View) :-
	binding(Editor, Key, Function, String),
	(   send(Pattern, search, String)
	->  send(View, append, String)
	;   send(String, done)
	).

binding(Editor, Key, Function, String) :-
	atom(Function),
	find_send_method(Editor, Function, Method), !,
	(   get(Method, summary, Summary), Summary \== @nil
	->  true
	;   Summary = 'Undocumented'
	),
	new(String, string('%s\t%s\t"%s"\n', Key, Function, Summary)).
binding(_, Key, Function, String) :-
	atom(Function),
	reserved_binding(Function, Summary), !,
	new(String, string('%s\t%s\t"%s"\n', Key, Function, Summary)).
binding(_, Key, Function, String) :-
	atom(Function),
	new(String, string('%s\t%s\t"%s"\n',
			   Key, Function, 'Not Implemented')).
binding(Editor, Key, Function, String) :-
	send(Function, instance_of, message),
	(   get(Function, receiver, Editor)
	;   get(Function, receiver, @receiver)
	), !,
	get(Function, name, Selector),
	binding(Editor, Key, Selector, String).
binding(_, Key, Function, String) :-
	send(Function, instance_of, code),
	portray_object(code, Term), !,
	term_to_atom(Term, Atom),
	new(String, string('%s\t%s\n', Key, Atom)).
binding(_, Key, Function, String) :-
	new(String, string('%s\t%s\n', Key, Function)).


%%	find_send_method(+Object, +Selector, -Method)
%	Try to locate the method invoking the specified behaviour.

find_send_method(Obj, Name, Method) :-
	get(Obj, send_method, Name, tuple(_, Method)).

%%	reserved_binding(+Name, -Summary)

reserved_binding(keyboard_quit,		'Abort current sequence').
reserved_binding(prefix,		'Prefix for multikey command').
reserved_binding(digit_argument,	'Construct numeric argument').
reserved_binding(universal_argument,	'Start numeric argument').

