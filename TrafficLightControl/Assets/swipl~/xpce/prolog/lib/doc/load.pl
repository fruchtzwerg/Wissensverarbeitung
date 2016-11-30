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

:- module(doc_load, []).
:- use_module(library(pce)).

		 /*******************************
		 *	       PATHS		*
		 *******************************/

:- multifile
	user:file_search_path/2.

user:file_search_path(doc, pce('prolog/lib/doc')).


		 /*******************************
		 *	OBLIGATORY PARTS	*
		 *******************************/

:- use_module(doc(util)).		% generic stuff
:- use_module(doc(objects)).		% global reusable objects
:- use_module(doc(emit)).		% basic conversion library


		 /*******************************
		 *	     CLASSES		*
		 *******************************/

:- pce_autoload(doc_table,	 doc(table)).

:- pce_autoload(doc_mode,	 doc(layout)).
:- pce_autoload(pbox,		 doc(layout)).
:- pce_autoload(bullet_list,	 doc(layout)).
:- pce_autoload(enum_list,	 doc(layout)).
:- pce_autoload(definition_list, doc(layout)).
:- pce_autoload(button_box,	 doc(layout)).
:- pce_autoload(anchor_box,	 doc(layout)).

:- pce_autoload(doc_window,	 doc(window)).
:- pce_autoload(doc_browser,	 doc(browser)).
