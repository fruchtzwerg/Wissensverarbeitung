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

:- module(url_image, []).
:- use_module(library(pce)).

resource(noimg, image, image('16x16/noimg.xpm')).

:- pce_autoload(http_client, library(http_client)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple class to provide URL-based  images.   Currently  deals  only with
fetching images using the file and http protocols.

Fetched images may be cached in a table using @on for the cache argument
of ->initialise. Creating a url_image  for   the  2nd  time with caching
enabled returns the previously loaded image.

->initialise also provides a no_image argument. This  image is used as a
default image if the system cannot find the required image.

TBD: What to  do/how  to  implement  reload?   Could  we  be  using  the
broadcasting service for this?

This class was designed to be used with the library scaledbitmap.pl.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(url_image, image,
		   "Image whose source comes from a URL").

variable(url,	 name, get, "Source of the image").
variable(cache,  bool, get, "Image is cached").
variable(exists, bool, get, "We succeeded loading the image from URL").

initialise(I, URL:url=name, Cache:cache=[bool], NoImage:no_image=[image]*) :->
	"Create image from URL data"::
	send_super(I, initialise),
	(   (   NoImage == @nil
	    ->	send(I, load, URL, Cache)
	    ;	pce_catch_error(_, send(I, load, URL, Cache))
	    )
	->  send(I, slot, exists, @on)
	;   send(I, slot, exists, @off),
	    (	send(NoImage, instance_of, image)
	    ->  send(I, copy, NoImage)
	    ;   NoImage == @default
	    ->  send_super(I, load, resource(noimg))
	    )
	).

:- pce_global(@url_image_table, new(hash_table)).

lookup(_, URL:name, Cache:[bool], I:url_image) :<-
	"Lookup from image table"::
	Cache \== @off,
	get(@url_image_table, member, URL, I).

unlink(I) :->
	get(I, url, URL),
	(   get(I, cache, @on)
	->  send(@url_image_table, delete, URL)
	;   true
	),
	send_super(I, unlink).

free(I) :->
	"Only free if not referenced"::
	(   get(I, references, 1)	% 1 from hash-table
	->  send_super(I, free)
	).

:- pce_group(file).

load(I, URL:name, Cache:[bool]) :->
	"load from URL data"::
	send(I, slot, url, URL),
	(   new(Re, regex('file:(.*)', @off)),
	    send(Re, match, URL)
	->  get(Re, register_value, URL, 1, name, FileName),
	    send_super(I, load, FileName)
	;   send(URL, prefix, 'http:', @on)
	->  new(HC, http_client(URL)),
	    new(TB, text_buffer),
	    send(HC, fetch_data, TB),
	    send_super(I, load, TB),
	    free(TB),
	    free(HC)
	),
	(   Cache == @off
	->  send(I, slot, cache, @off)
	;   send(@url_image_table, append, URL, I),
	    send(I, slot, cache, @on)
	).

:- pce_end_class.

