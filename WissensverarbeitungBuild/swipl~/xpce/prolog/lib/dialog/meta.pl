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

:- module(dia_meta, [class_of_type/2]).
:- use_module(library(pce)).
:- require([ chain_list/2
	   , member/2
	   ]).

:- pce_extend_class(object).

dia_argument_type(Obj, Selector:name, Type:type) :<-
	get(Obj, get_method, Selector, tuple(_, Implementation)),
	get(Implementation, return_type, Type).

:- pce_end_class.


class_of_type(Type, Class) :-
	get(Type, kind, class),
	get(Type, context, Class).
class_of_type(Type, Class) :-
	get(Type, kind, class_object),
	get(Type, context, Class).
class_of_type(Type, @object_class) :-
	get(Type, kind, any).
class_of_type(Type, Class) :-
	get(Type, kind, alias),
	get(Type, context, Type2),
	class_of_type(Type2, Class).
class_of_type(Type, Class) :-
	get(Type, kind, member),
	get(Type, context, Type2),
	class_of_type(Type2, Class).
class_of_type(Type, Class) :-
	get(Type, supers, Supers),
	Supers \== @nil,
	chain_list(Supers, List),
	member(T, List),
	class_of_type(T, Class).
