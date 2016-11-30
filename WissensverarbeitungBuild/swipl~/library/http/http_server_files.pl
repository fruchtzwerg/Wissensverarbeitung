/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University, Amsterdam

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

:- module(http_server_files,
	  [ serve_files_in_directory/2	% +Alias, +HTTPRequest
	  ]).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).

/** <module> Serve files needed by modules from the server

This module provides an infrastructure   for serving resource-files such
as icons, JavaScript, CSS files, etc.   The default configuration serves
the    HTTP    locations    =icons=,     =css=      and     =js=    (see
http_absolute_location/3)

The location for these services  can  be   changed  by  adding rules for
http:location/3.  Directories  providing  additional    or   alternative
resources can be achieved by adding rules for user:file_search_path/2.
*/

:- multifile
	http:location/3.		% Alias, Expansion, Options
:- dynamic
	http:location/3.		% Alias, Expansion, Options

http:location(icons, root(icons), [ priority(-100) ]).
http:location(css,   root(css),   [ priority(-100) ]).
http:location(js,    root(js),    [ priority(-100) ]).

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

user:file_search_path(icons, library('http/web/icons')).
user:file_search_path(css,   library('http/web/css')).
user:file_search_path(js,    library('http/web/js')).

:- http_handler(icons(.), serve_files_in_directory(icons),
		[prefix, priority(-100)]).
:- http_handler(css(.),   serve_files_in_directory(css),
		[prefix, priority(-100)]).
:- http_handler(js(.),    serve_files_in_directory(js),
		[prefix, priority(-100)]).

%%	serve_files_in_directory(+Alias, +Request)
%
%	Serve files from the directory  Alias   from  the path-info from
%	Request.    This    predicate    is     used    together    with
%	file_search_path/2. Note that multiple  clauses   for  the  same
%	file_search_path alias can be used to merge files from different
%	physical locations onto the same HTTP   directory. Note that the
%	handler must be declared  as  =prefix=.   Below  is  an  example
%	serving images from  http://<host>/img/...   from  the directory
%	=http/web/icons=.
%
%	    ==
%	    http:location(img, root(img), []).
%	    user:file_search_path(icons, library('http/web/icons')).
%
%	    :- http_handler(img(.), serve_files_in_directory(icons), [prefix]).
%	    ==
%
%	This predicate calls http_404/2 if the   physical file cannot be
%	located. If the requested  path-name   is  unsafe  (i.e., points
%	outside  the  hierarchy  defines    by   the  file_search_path/2
%	declaration), this handlers returns a _403 Forbidden_ page.
%
%	@see http_reply_file/3

serve_files_in_directory(Alias, Request) :-
	memberchk(path_info(PathInfo), Request), !,
	Term =.. [Alias,PathInfo],
	(   catch(http_safe_file(Term, []),
		  error(permission_error(read, file, _), _),
		  fail)
	->  (   absolute_file_name(Term, Path,
				   [ access(read),
				     file_errors(fail)
				   ])
	    ->  http_reply_file(Path,
				[ unsafe(true),
				  static_gzip(true)
				], Request)
	    ;   http_404([], Request)
	    )
	;   memberchk(path(Path), Request),
	    throw(http_reply(forbidden(Path)))
	).
serve_files_in_directory(_Alias, Request) :-
	memberchk(path(Path), Request),
	throw(http_reply(forbidden(Path))).

