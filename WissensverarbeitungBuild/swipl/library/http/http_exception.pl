/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2015, University of Amsterdam
			      VU University Amsterdam

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

:- module(http_exception,
	  [ map_exception_to_http_status/4
	  ]).

/** <module> Map Prolog exceptions to HTTP errors

This module maps exceptions from various parts  of the HTTP libraries as
well as exceptions from user  handler   predicates  into meaningful HTTP
error codes such as 4XX and 5XX  codes. For example, existence errors on
http locations are mapped to 404 while out-of-stack is mapped to 503.

This library provides one hook: http:bad_request_error/2 can be extended
to map exceptions into 400 bad request responses.

@see	http_header.pl, http_wrapper.pl
*/

:- multifile
	http:bad_request_error/2.	% Formal, Context

%%	map_exception_to_http_status(+Exception, -Reply, -HdrExtra, -Context)
%
%	Map certain defined  exceptions  to   special  reply  codes. The
%	http(not_modified)   provides   backward     compatibility    to
%	http_reply(not_modified).

map_exception_to_http_status(http(not_modified),
	      not_modified,
	      [connection('Keep-Alive')],
              []) :- !.
map_exception_to_http_status(http_reply(Reply),
	      Reply,
	      [connection(Close)],
              []) :- !,
	(   keep_alive(Reply)
	->  Close = 'Keep-Alive'
	;   Close = close
	).
map_exception_to_http_status(http_reply(Reply, HdrExtra0),
	      Reply,
	      HdrExtra,
              Context) :- !,
        map_exception_to_http_status(http_reply(Reply, HdrExtra0, []),
                                     Reply,
                                     HdrExtra,
                                     Context).

map_exception_to_http_status(http_reply(Reply, HdrExtra0, Context),
	      Reply,
	      HdrExtra,
              Context):- !,
	(   memberchk(close(_), HdrExtra0)
	->  HdrExtra = HdrExtra0
	;   HdrExtra = [close(Close)|HdrExtra0],
	    (   keep_alive(Reply)
	    ->  Close = 'Keep-Alive'
	    ;   Close = close
	    )
	).
map_exception_to_http_status(error(existence_error(http_location, Location), _),
	      not_found(Location),
	      [connection(close)],
              []) :- !.
map_exception_to_http_status(error(permission_error(_, http_location, Location), _),
	      forbidden(Location),
	      [connection(close)],
              []) :- !.
map_exception_to_http_status(error(threads_in_pool(_Pool), _),
	      busy,
	      [connection(close)],
              []) :- !.
map_exception_to_http_status(E,
	      resource_error(E),
	      [connection(close)],
              []) :-
	resource_error(E), !.
map_exception_to_http_status(E,
	      bad_request(E2),
	      [connection(close)],
              []) :-
	bad_request_exception(E), !,
	discard_stack_trace(E, E2).
map_exception_to_http_status(E,
	      server_error(E),
	      [connection(close)],
              []).

resource_error(error(resource_error(_), _)).

bad_request_exception(error(Error, Context)) :-
	nonvar(Error),
	bad_request_error(Error, ContextGeneral),
	(   var(ContextGeneral)
	->  true
	;   Context = context(_Stack, ContextInstance)
	->  subsumes_term(ContextGeneral, ContextInstance)
	), !.

bad_request_error(Error, Context) :-
	http:bad_request_error(Error, Context).
bad_request_error(Error, Context) :-
	default_bad_request_error(Error, Context).

default_bad_request_error(domain_error(http_request, _), _).
default_bad_request_error(existence_error(http_parameter, _), _).
default_bad_request_error(type_error(_, _), http_parameter(_)).
default_bad_request_error(syntax_error(http_request_line(_)), _).
default_bad_request_error(syntax_error(http_request(_)), _).
default_bad_request_error(syntax_error(_), in_http_request).

discard_stack_trace(error(Formal, context(_,Msg)),
		    error(Formal, context(_,Msg))).

%%	http:bad_request_error(+Formal, -ContextTemplate) is semidet.
%
%	If  an  exception  of  the   term  error(Formal,  context(Stack,
%	Context)) is caught and  subsumes_term(ContextTemplate, Context)
%	is true, translate the exception into  an HTTP 400 exception. If
%	the exception contains a stack-trace, this  is stripped from the
%	response.
%
%	The idea behind this hook  is   that  applications can raise 400
%	responses by
%
%	  - Throwing a specific (error) exception and adding a rule
%	    to this predicate to interpret this as 400.
%	  - Define rules for prolog:error_message//1 to formulate
%	    an appropriate message.


%%	keep_alive(+Reply) is semidet.
%
%	If true for Reply, the default is to keep the connection open.

keep_alive(not_modified).
keep_alive(file(_Type, _File)).
keep_alive(tmp_file(_Type, _File)).
keep_alive(stream(_In, _Len)).
keep_alive(cgi_stream(_In, _Len)).
keep_alive(switching_protocols(_Goal, _)).


		 /*******************************
		 *	    IDE SUPPORT		*
		 *******************************/

% See library('trace/exceptions')

:- multifile
	prolog:general_exception/2.

prolog:general_exception(http_reply(_), http_reply(_)).
prolog:general_exception(http_reply(_,_), http_reply(_,_)).
