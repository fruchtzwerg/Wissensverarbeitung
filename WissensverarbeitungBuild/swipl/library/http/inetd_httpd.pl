/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
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

:- module(http_inetd,
	  [ http_server/2		% :Goal, +Options
	  ]).
:- use_module(http_wrapper).

:- meta_predicate
	http_server(:, +),
	server_loop(:, +).

/** <module> Run Prolog HTTP server from Unix inetd

This module implements handling a single request (or multiple as long as
=Keep-Alive= is respected), talking to stardard input and output.

@deprecated	This type of handling of HTTP requests should be
		considered outdated.  See library(http/thread_httpd).
*/

%%	http_server(:Goal, +Options)
%
%	Start the server from inetd. This is really easy as user_input
%	is connected to the HTTP input and user_output is the place to
%	write our reply to.

http_server(Goal, Options) :-
	prompt(_, ''),
	set_stream(user_output, buffer(full)),
	set_stream(user_output, encoding(octet)),
	set_stream(user_input, buffer(full)),
	set_stream(user_input, encoding(octet)),
	server_loop(Goal, Options).

server_loop(_, _) :-
	at_end_of_stream(user_input), !,
	halt.
server_loop(Goal, Options) :-
	http_wrapper(Goal, user_input, user_output, Connection, []),
	(   downcase_atom(Connection, 'keep-alive')
	->  server_loop(Goal, Options)
	;   halt
	).
server_loop(_, _) :-			% wrapper failed
	halt.
