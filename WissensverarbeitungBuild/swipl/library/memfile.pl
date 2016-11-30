/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module(memory_file,
	  [ new_memory_file/1,		% -Handle
	    free_memory_file/1,		% +Handle
	    size_memory_file/2,		% +Handle, -Size
	    size_memory_file/3,		% +Handle, -Size, +Encoding
	    open_memory_file/3,		% +Handle, +Mode, -Stream
	    open_memory_file/4,		% +Handle, +Mode, -Stream, +Options
	    insert_memory_file/3,	% +Handle, +Position, +Data
	    delete_memory_file/3,	% +Handle, +Position, +Length
	    atom_to_memory_file/2,	% +Atom, -Handle
	    memory_file_to_atom/2,	% +Handle, -Atom
	    memory_file_to_codes/2,	% +Handle, -CodeList
	    memory_file_to_string/2,	% +Handle, -String
	    memory_file_to_atom/3,	% +Handle, -Atom, +Encoding
	    memory_file_to_codes/3,	% +Handle, -CodeList, +Encoding
	    memory_file_to_string/3,	% +Handle, -String, +Encoding
	    memory_file_substring/5,	% +Handle, +Before, +Length, +After, -String
	    memory_file_line_position/4,% +Handle, ?Line, ?ListPos, ?Offset
	    utf8_position_memory_file/3 % +Handle, -Here, -Size
	  ]).
:- use_foreign_library(foreign(memfile)).

:- predicate_options(open_memory_file/4, 4,
		     [ encoding(encoding),
		       free_on_close(boolean)
		     ]).
