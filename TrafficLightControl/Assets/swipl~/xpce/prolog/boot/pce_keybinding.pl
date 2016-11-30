/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
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


:- module(pce_keybinding, []).
:- use_module(pce_boot(pce_principal)).
:- use_module(pce_boot(pce_realise)).

:- multifile
	binding/3.

message_level(silent).
%message_level(informational).


		 /*******************************
		 *	STYLE PREFERENCES	*
		 *******************************/

%%	binding(+ModeName, +TableName, +Modifications)
%
%	Specify bindings for alternative key-binding-styles.
%
%	@param ModeName		Name of the key-binding-style
%	@param TableName	Syntax table to modify
%	@param Modifications	List of Key-Method

binding(cua, editor,
	[ '\\C-v' = paste,
	  '\\C-s' = save_buffer
	]).
binding(cua, 'emacs$fundamental',
	[ '\\C-f' = isearch_forward,
	  '\\C-o' = open,
	  '\\C-n' = new,
	  '\\C-p' = print
	]).
binding(apple, editor,
	[ '\\es'  = save_buffer,
	  '\\ez'  = undo
	]).
binding(apple, 'emacs$fundamental',
	[ '\\ec'  = copy_or_capitalize_word,
	  '\\ex'  = cut_or_execute_extended_command
	]).
binding(apple, emacs_page,
	[ '\\ev'  = paste_or_scroll_down
	]).


		 /*******************************
		 *	 CHANGE BINDINGS	*
		 *******************************/

%%	set_keybinding_style(+Id)
%
%	Runtime modification of the current key-binding style.

set_keybinding_style(Mode) :-
	current_style(Mode), !.
set_keybinding_style(emacs) :- !,
	send(@key_bindings, for_all,
	     message(@arg2, unmodify)),
	set_style(emacs).
set_keybinding_style(Style) :-
	set_keybinding_style(emacs),
	(   binding(Style, Table, Modifications),
	    get(@key_bindings, member, Table, KB),
	    modify(Modifications, KB),
	    fail
	;   true
	),
	set_style(Style).


modify([], _).
modify([Mod|T], KB) :-
	modify1(Mod, KB),
	modify(T, KB).

modify1(Key = Command, KB) :-
	get(KB?bindings, value, Key, Command), !.
modify1(Key = Command, KB) :-
	send(KB, save_default, Key),
	send(KB, function, Key, Command),
	get(KB, name, Table),
	message_level(Level),
	print_message(Level, format('~w (~p): ~w --> ~w',
				    [Table, KB, Key, Command])).
modify1(delete(Key), KB) :-
	\+ get(KB?bindings, value, Key, _), !.
modify1(delete(Key), KB) :-
	send(KB, save_default, Key),
	get(KB, bindings, Bindings),
	send(Bindings, delete, Key),
	get(KB, name, Table),
	message_level(Level),
	print_message(Level, format('~w: deleted ~w', [Table, Key])).


		 /*******************************
		 *	  DYNAMIC TABLES	*
		 *******************************/

:- pce_extend_class(key_binding).

class_variable(style, name,
	       [ 'X'(emacs),
		 windows(cua),
		 apple(apple)
	       ],
	       "Basic binding style (emacs,cua,apple)").

%%	current_style(-Style) is det.
%%	set_style(+Style) is det.
%
%	Manipulate the style.  The style is stored in the class-variable
%	key_binding.style, so it can be set in the users preferences
%	file.

current_style(Style) :-
	get(@pce, convert, key_binding, class, Class),
	get(Class, class_variable, style, Var),
	get(Var, value, Style).

set_style(Style) :-
	get(@pce, convert, key_binding, class, Class),
	get(Class, class_variable, style, Var),
	send(Var, value, Style).


apply_preferences(KB) :->
	"Apply CUA-mode preferences"::
	send(KB, apply_cua),
	send(KB, bind_resources).	% bind from ~/.xpce/Defaults

apply_cua(KB) :->
	"Apply our local overrides"::
	current_style(Mode),
	(   Mode == emacs
	->  true
	;   get(KB, name, Name),
	    binding(Mode, Name, Modifications)
	->  modify(Modifications, KB)
	;   true
	).

save_default(KB, Key:name) :->
	"Save default binding for Key"::
	(   get(KB, attribute, modified, Undo)
	->  true
	;   send(KB, attribute, modified, new(Undo, sheet))
	),
	(   get(Undo, value, Key, _)
	->  true			% Already saved this one
	;   get(KB, bindings, Bindings),
	    (   get(Bindings, value, Key, Command)
	    ->  send(Undo, value, Key, Command)
	    ;   send(Undo, value, Key, @nil)
	    )
	).

unmodify(KB) :->
	"Replay recorded modifications"::
	(   get(KB, attribute, modified, Undo)
	->  send(Undo, for_all,
		 message(KB, unbind, @arg1?name, @arg1?value)),
	    send(KB, delete_attribute, modified)
	;   true
	).

unbind(KB, Key:name, Command:[name|code]*) :->
	"Restore saved binding for Key"::
	get(KB, name, Table),
	message_level(Level),
	(   Command == @nil
	->  get(KB, bindings, Sheet),
	    send(Sheet, delete, Key),
	    print_message(Level,
			  format('~w: deleted ~w', [Table, Key]))
	;   send(KB, function, Key, Command),
	    print_message(Level,
			  format('~w (~p): ~w --> ~w',
				 [Table, KB, Key, Command]))
	).

:- pce_end_class(key_binding).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Runtime switching is connected to @pce as the operation influences an
unknown number of unknown key_binding objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_extend_class(pce).

:- pce_group(preferences).

key_binding_style(_Pce, Style:name) :->
	"Set the key-binding style"::
	set_keybinding_style(Style).

key_binding_style(_Pce, Style:name) :<-
	"Get the key-binding style"::
	current_style(Style).

key_binding_styles(_Pce, Styles:chain) :<-
	"List of supported styles"::
	findall(Style, binding(Style, _Class, _Mod), StyleList),
	sort([emacs|StyleList], Sorted),
	new(Styles, chain),
	add_styles(Sorted, Styles).

add_styles([], _).
add_styles([H|T], Chain) :-
	send(Chain, append, H),
	add_styles(T, Chain).

:- pce_end_class(pce).


%	Create the type key_binding_style, a dynamic `name-of' type
%	holding the defined key-binding styles.

make_key_binding_style_type :-
	get(@pce, convert, key_binding_style, type, Type),
	send(Type, name_reference, key_binding_style_type),
	send(Type, kind, name_of),
	get(@pce, key_binding_styles, Styles),
	send(Type, slot, context, Styles).

:- initialization make_key_binding_style_type.


		 /*******************************
		 *	       APPLE		*
		 *******************************/

:- pce_extend_class(editor).

copy_or_capitalize_word(E, Arg:[int]) :->
	"Command-c copies; ESC c capitalizes word"::
	(   Arg == @default,
	    send(@event, has_modifier, m)
	->  send(E, copy)
	;   send(E, capitalize_word, Arg)
	).

cut_or_execute_extended_command(E, Arg:[int]) :->
	"Command-X cut; ESC-x starts extended command"::
	(   Arg == @default,
	    send(@event, has_modifier, m)
	->  send(E, cut)
	;   send(E, noarg_call, execute_extended_command, Arg)
	).


paste_or_scroll_down(E, Arg:[int]) :->
	"Command-v pasts; ESC v scrolls down"::
	(   Arg == @default,
	    send(@event, has_modifier, m)
	->  send(E, paste)
	;   send(E, scroll_down, Arg)
	).

:- pce_end_class(editor).

:- pce_extend_class(list_browser).

paste_or_scroll_down(LB, Arg:[int]) :->
	"Forward to ->scroll_down (Apple keybinding)"::
	send(LB, scroll_down, Arg).

:- pce_end_class(list_browser).
