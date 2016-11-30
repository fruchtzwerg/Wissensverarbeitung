/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- multifile
	prolog:doc_object_summary/4,	% Object, ?Category, ?Section, ?Summary
	prolog:doc_object_page//2,	% +Object, +Options
	prolog:doc_nav_tree//2,		% +Object, +Options
	prolog:doc_object_link//2,	% +Object, +Options
	prolog:doc_category/3,		% Name, Order, Description
	prolog:doc_file_index_header//2,% +File, +Options
	prolog:doc_object_title/2,	% +Object, -Title
	prolog:doc_object_href/2,	% +Object, -HREF
	prolog:doc_canonical_object/2,	% +ObjectIn, -CanonicalObj
	prolog:doc_search_field//1,	% +Options
	prolog:doc_places_menu//1,	% +Dir
	prolog:doc_directory/1,		% ?Dir
	prolog:doc_object_page_footer//2, % +Object, +Options
	prolog:doc_page_header//2,	% +File, +Options
	prolog:doc_links//2,		% +Directory, +Options
	prolog:doc_file_title//3.	% +Title, +File, +Options
