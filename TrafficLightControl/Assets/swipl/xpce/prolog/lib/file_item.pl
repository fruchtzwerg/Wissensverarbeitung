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

:- module(pce_file_item, []).
:- use_module(library(pce)).

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).


		 /*******************************
		 *	  CLASS SAVE-FILE	*
		 *******************************/

:- pce_begin_class(save_file, file,
		   "File as a destination (type-check only)").

convert(_, Name:name, SaveFile:save_file) :<-
	new(SaveFile, save_file(Name)).

:- pce_end_class(save_file).


		 /*******************************
		 *       FILE COMPLETION	*
		 *******************************/

:- pce_begin_class(file_item, text_item,
		   "text_item with file-name completion").

variable(exists,    'bool|{open,save}' := @off,	both, "File must exist").
variable(directory, directory*,			both, "Relative to").

initialise(FI, Name:[name], Def:[any|function], Msg:[code]*) :->
	clean_file_name(Def, Clean),
	send(FI, send_super, initialise, Name, Clean, Msg),
	send(FI, style, combo_box).


activate(FI, Val:bool) :->
	(   Val == @on
	->  true
	;   send(FI, send_super, activate, Val)
	).


selected_completion(FI, Component:char_array, _Apply:[bool]) :->
	send(FI, send_super, selected_completion, Component, @off),
	get(FI, selection, @off, Selection),
	(   send(directory(Selection), exists)
	->  send(FI, append, '/'),
	    send(FI, caret, @default)
	;   send(file(Selection), exists)
	->  send(FI, apply, @on)
	;   true
	).


local_path(FI, In:char_array, Out:name) :<-
	(   get(FI, directory, Dir),
	    Dir \== @nil
	->  get(Dir, file_name, In, Out)
	;   Out = In
	).


completions(FI, Tuple:tuple, Matches:chain) :<-
	"Chain with completions of FileName in DirName"::
	get(Tuple, first, DirName0),
	get(Tuple, second, FileName),
	new(Matches, chain),
	new(Re, regex((string('^%s', FileName)))),
	(   send(class(file), has_feature, case_sensitive, @off)
	->  send(Re, ignore_case, @on)
	;   true
	),
	(   is_absolute_file_name(DirName0)
	->  DirName = DirName0
	;   get(FI, local_path, DirName0, DirName)
	),
	send(directory(DirName), scan, Matches, Matches, Re),
	send(Matches, delete_all, '.'),
	send(Matches, delete_all, '..').


split_completion(_FI, Value, Tuple:tuple) :<-
	"Split the current entry"::
	new(S, string('%s', Value)),
						% delete ...// or .../~
	get(S, size, L),
	(   get(regex('//|~|\\w:[/\\\\]'), search, S, L, 0, Start)
	->  send(S, delete, 0, Start),
	    (   send(S, prefix, '//')
	    ->  send(S, delete, 0, 1)
	    ;   true
 	    )
	;   true
	),

	(   send(S, suffix, /)
	->  get(S, value, Path),
	    BaseName = ''
	;   new(F, file(S)),
	    get(F, directory_name, DirName),
	    get(F, base_name, BaseName),
	    make_path(DirName, Path)
	),
	new(Tuple, tuple(Path, BaseName)).

make_path('', '') :- !.
make_path('.', '') :- !.
make_path(Path, WithSlash) :-
	get(Path, ensure_suffix, /, WithSlash).


indicate_directory(_FI, Dir:string) :->
	(   send(directory(Dir), exists)
	->  send(Dir, ensure_suffix, /)
	;   true
	).


selection(FI, Warn:[bool], FileName:name) :<-
	"Get the current selection"::
	get(FI, modified, Modified),
	get(FI?value_text, string, RawName),
	get(RawName, size, L),
	(   get(regex('//|/~'), search, RawName, L, 0, Start)
	->  new(S, string('%s', RawName)),
	    send(S, delete, 0, Start),
	    (	send(S, prefix, '//')
	    ->  send(S, delete, 0, 1)
	    ;   true
 	    ),
	    get(S, value, FileName0)
	;   get(RawName, value, FileName0)
	),
	get(FI, local_path, FileName0, FileName),
	(   (   Warn == @off
	    ;	Modified == @off
	    )
	->  true
	;   get(FI, exists, Exists),
	    (   (   Exists == @on
		;   Exists == open
		)
	    ->  (   send(FI, check_existence, FileName)
		->  true
		;   send(FI, report, warning,
			 'File %s does not exist', FileName),
		    fail
		)
	    ;   Exists == save
	    ->  (   send(FI, check_existence, FileName)
		->  send(FI?display, confirm, 'Overwrite file %s?', FileName)
		;   true
		)
	    ;   true
	    )
	).


check_existence(_FI, Name:name) :->
	"Check existence of file"::
	send(file(Name), exists).

clean_file_name(Def, Clean) :-
	\+ send(Def, '_instance_of', function),
	get(@pce, convert, Def, string, Clean), !,
	send(regex(//), for_all, Clean,
	     message(@arg1, replace, @arg2, '/')).
clean_file_name(Def, Def).


browse(FI) :->
	"Run finder to fill with value"::
	get(FI?value, value, Sofar),
	(   exists_directory(Sofar)
	->  Dir = Sofar
	;   file_directory_name(Sofar, Dir)
	),
	get(FI, exists, Exists),
	get(@finder, file, Exists, directory := Dir, New),
	send(FI, value, New),
	send(FI, apply, @on).

:- pce_end_class(file_item).


		 /*******************************
		 *     CLASS DIRECTORY_ITEM	*
		 *******************************/

:- pce_begin_class(directory_item, file_item).

completions(_FI, Tuple:tuple, Matches:chain) :<-
	"Chain with completions of FileName in DirName"::
	get(Tuple, first, DirName),
	get(Tuple, second, FileName),
	get(directory(DirName), directories, string('^%s', FileName), Matches).


check_existence(_FI, Name:name) :->
	"Check existence of directory"::
	send(directory(Name), exists).

browse(DI) :->
	"Run finder to fill with value"::
	get(DI?value, value, Sofar0),
	(   Sofar0 == ''
	->  Sofar = '.'
	;   Sofar = Sofar0
	),
	(   send(@display, has_get_method, win_directory)
	->  get(@display, win_directory, @default, Sofar, DirName),
	    send(DI, value, DirName),
	    send(DI, apply, @on)
	;   send_super(DI, browse)
	).

:- pce_end_class.
