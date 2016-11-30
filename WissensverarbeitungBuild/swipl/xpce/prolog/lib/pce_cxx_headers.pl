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

:- module(pce_cpp_header,
	  [ generate_cpp_class_header_file/0
	  ]).
:- use_module(library(pce)).
:- require([ apply/2
	   , between/3
	   , atomic_list_concat/2
	   , forall/2
	   , ignore/1
	   , is_list/1
	   , memberchk/2
	   , nth1/3
	   , once/1
	   ]).

itf_max_arg(9).

cpp_header_dir(Dir) :-
	get(string('%s/include/pce', @pce?home), value, Dir).

generate_cpp_class_header_file :-
	new(X, chain),
	send(@classes, for_all, if(@arg2?creator == built_in,
				   message(X, append, @arg1))),
	send(X, delete, class),
	ignore(send(X, delete, call)),	% just make sure
	send(X, delete, object),
	send(X, delete, variable),
	send(X, delete, chain),
	send(X, delete, pce),
	send(X, delete, string),
	send(X, sort),
	send(X, for_all, message(@prolog, cpp_class, @arg1)).

cpp_class(ClassName) :-
	cpp_header_dir(Dir),
	cpp_class_name(ClassName, CppName),
	atomic_list_concat([Dir, /, CppName, '.h'], File),
	new(@output, file(File)),
	send(@output, open, write),
	xpce_file_header(CppName),
	generate_class_def(ClassName),
	generate_as(ClassName),
	xpce_file_footer(CppName),
	send(@output, close),
	free(@output).

xpce_file_header(ClassName) :-
	output('/*  $Id'),		% break to fool CVS
	output('$\n\n'),
	output('    Part of XPCE\n'),
	output('    Designed and implemented by Anjo Anjewierden and Jan Wielemaker\n'),
	output('    E-mail: jan@swi.psy.uva.nl\n\n'),
	output('    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.\n*/\n\n'),
	output('#ifndef _PCE_%s_H\n', ClassName?upcase),
	output('#define _PCE_%s_H\n\n', ClassName?upcase).

xpce_file_footer(ClassName) :-
	output('#endif /*!_PCE_%s_H*/\n', ClassName?upcase).


generate_as(ClassName) :-
	get(@pce, convert, ClassName, class, Class),
	get(Class, name, Name),
	once(cpp_class_name(Name, CppName)),
	output('inline Pce%s\nAs%s(PceArg a)\n', [CppName, CppName]),
	output('{ return *((Pce%s*) &a);\n}\n\n', CppName).


generate_class_def(ClassName) :-
	get(@pce, convert, ClassName, class, Class),
	get(Class, send_method, initialise, InitMethod),
	get(Class, name, Name),
	once(cpp_class_name(Name, CppName)),
	output('PceExternalClass(Class%s);\n', CppName),
	output('class Pce%s :public PceObject\n{\npublic:\n', CppName),
	get(InitMethod, types, Types),
	argument_range(Types, Low, High),
	forall(between(Low, High, Arity),
	       constructor(InitMethod, CppName, Arity)),
	output('};\n\n').

constructor(InitMethod, CppName, N) :-
	arg_names(InitMethod, 1, N, [], ArgNames),
	output('  Pce%s(', CppName),
	forall(between(1, N, Arg),
	       constructor_iarg(ArgNames, Arg)),
	output(') :\n    PceObject(Class%s', CppName),
	forall(between(1, N, Arg),
	       constructor_oarg(ArgNames, Arg)),
	output(')\n  {\n  }\n').

arg_names(_, I, N, _, []) :-
	I =:= N + 1, !.
arg_names(InitMethod, I, N, D, [TheName|R]) :-
	get(InitMethod, argument_type, I, Type),
	get(Type, argument_name, Name),
	cpp_arg_name(Name, ArgName),
	unique_name(ArgName, D, TheName),
	NI is I + 1,
	arg_names(InitMethod, NI, N, [TheName|D], R).

cpp_arg_name(@nil, a).			% unnamed xpce argument
cpp_arg_name(return, ret).		% avoid C-keywords
cpp_arg_name(class, cl).
cpp_arg_name(int, i).
cpp_arg_name(char, chr).
cpp_arg_name(then, thn).
cpp_arg_name(else, els).
cpp_arg_name(default, def).
cpp_arg_name(X, X).			% ok-to-use name

unique_name(Name, D, Name) :-
	\+ memberchk(Name, D), !.
unique_name(Name, D, Unique) :-
	between(2, 100, N),
	atom_concat(Name, N, Unique),
	\+ memberchk(Unique, D).

constructor_iarg(ArgNames, Arg) :-
	(   Arg \== 1
	->  output(', ')
	;   true
	),
	nth1(Arg, ArgNames, Name),
	output('PceArg %s', Name).

constructor_oarg(ArgNames, Arg) :-
	nth1(Arg, ArgNames, Name),
	output(', %s', Name).

argument_range(Types, Low, High) :-
	get(Types, size, Size),
	(   Size == 0
	->  Low = 0,
	    High = 0
	;   get(Types, element, Size, Last),
	    (   get(Last, vector, @on)
	    ->	VLow is Size - 1,
		itf_max_arg(High)
	    ;	VLow is Size,
		High = Size
	    ),
	    (	between(1, VLow, DLow),
		get(Types, element, DLow, Type),
		get(Type, check, @default, _)
	    ->  Low is DLow - 1
	    ;	Low is VLow
	    )
	).


		 /*******************************
		 *	      OUTPUT		*
		 *******************************/

output(Fmt) :-
	output(Fmt, []).
output(Fmt, Atom) :-
	\+ is_list(Atom), !,
	output(Fmt, [Atom]).
output(Fmt, Args) :-
	apply(send(@output, format, Fmt), Args).

cpp_class_name(*,	'Times').
cpp_class_name(-,	'Minus').
cpp_class_name(+, 	'Plus').
cpp_class_name(/,	'Divide').
cpp_class_name(:=,	'Binding').
cpp_class_name(<,	'Less').
cpp_class_name(=,	'Equation').
cpp_class_name(=<,	'LessEqual').
cpp_class_name(==,	'Equal').
cpp_class_name(>,	'Greater').
cpp_class_name(>=,	'GreaterEqual').
cpp_class_name(?,	'Obtain').
cpp_class_name(\==,	'NonEqual').
cpp_class_name(@=,	'NameObject').
cpp_class_name(Name, CppName) :-
	get(Name, capitalise, CppName).
