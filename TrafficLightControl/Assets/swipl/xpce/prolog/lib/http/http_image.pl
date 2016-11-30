/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2015, University of Amsterdam
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

:- module(http_image,
	  [ reply_image/2		% +Image, +Options
	  ]).
:- use_module(library(readutil)).
:- use_module(library(pce)).

/** <module> Serve dynamically generated images through XPCE
*/

%%	reply_image(+Image, +Options)
%
%	Formulate a CGI reply from an XPCE graphical.  This call handles
%	anything that can be converted into a pixmap object, notably any
%	XPCE graphical object.
%
%	Currently the only option recognised   is content_type(+Type) to
%	specify the type. image/jpeg and image/gif are the only sensible
%	values. The default is to generate gif.
%
%	If this module is used as a server on X11-based systems the user
%	must ensure the presence of  an   X11  server.  The XPCE library
%	'Xserver' provides code to start a `head-less' (i.e. server that
%	doesn't  need  a  physical  display)    server  and  adjust  the
%	environment to make XPCE use this server.

% (*) Note that this code uses  a   text_buffer  as intermediate for the
% data. this is pretty dubious as binary data is not well supported this
% way. It still works, but only when using newline(posix) for Windows.

reply_image(Image, Options) :-
	(   memberchk(content_type(Type), Options)
	->  image_format(Type, ImgType)
	;   Type = image/gif,
	    ImgType = gif
	),
	get(@pce, convert, Image, pixmap, Pixmap),
	new(TB, text_buffer),
	send(TB, undo_buffer_size, 0),
	send(Pixmap, save, TB, ImgType),
	format('Content-type: ~w~n~n', [Type]),
	pce_open(TB, read, Data),
	set_stream(Data, newline(posix)),	% (*)
	copy_stream_data(Data, current_output),
	close(Data),
	free(TB),
	(   Pixmap \== Image
	->  free(Pixmap)
	;   true
	).

image_format(image/Type, Type) :- !.
image_format(Format, Type) :-
	atom_concat('image/', Type, Format).

