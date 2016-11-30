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

:- module(pce_fontviewer,
	  [ fontviewer/0
	  ]).
:- use_module(library(pce)).
:- use_module(library('unicode/blocks')).
:- require([ between/3
	   ]).

fontviewer :-
	new(FontViewer, frame('Font Viewer')),
	send(FontViewer, append, new(B, browser(size := size(35, 10)))),
	send(FontViewer, append, new(D, dialog)),
	send(FontViewer, append, new(P, picture(size := size(350,350)))),
	send(P, right, B),
	send(D, below, B),

	send(D, append, new(Open, button(open))),
	send(D, append,
	     button(quit, message(FontViewer, destroy))),
	send(D, append, new(ChartMenu, menu(unicode_chart, cycle)), right),
	fill_chart_menu(ChartMenu),

	new(OpenMsg, message(@prolog, show_font,
			     P,
			     B?selection?object,
			     ChartMenu?selection)),
	send_list([Open, ChartMenu], message, OpenMsg),

	send(D, append, label(reporter), right),
	send(D, default_button, open),

	send(B, tab_stops, vector(80, 180)),
	send(B, open_message, message(Open, execute)),
	send(FontViewer, open),

	new(FontList, chain),
	send(@fonts, for_all, message(FontList, append, @arg2)),
	send(FontList, sort,
	     ?(@prolog, compare_fonts, @arg1, @arg2)),

	send(FontList, for_all,
	     message(@prolog, append_font_browser, B, @arg1)).

compare_fonts(F1, F2, Result) :-
	get(F1?family, compare, F2?family, Result),
	Result \== equal, !.
compare_fonts(F1, F2, Result) :-
	get(F1?style, compare, F2?style, Result),
	Result \== equal, !.
compare_fonts(F1, F2, Result) :-
	get(F1?points, compare, F2?points, Result).

append_font_browser(B, Font) :-
	get(Font, family, Fam),
	get(Font, style, Style),
	get(Font, points, Points),
	get(Font, object_reference, Name),
	send(B, append, dict_item(Name,
				  string('%s\t%s\t%d', Fam, Style, Points),
				  Font)).

fill_chart_menu(Menu) :-
	forall(unicode_block(Name, _, _),
	       send(Menu, append, Name)).

show_font(P, Font, Chart) :-
	unicode_block(Chart, From, To),
	send(P, clear),
	new(F, format(horizontal, 2, @on)),
	send(F, row_sep, 0),
	send(P, format, F),
	new(A, string(x)),
	MaxRow is ((To-From)//15)-1,
	(   between(0, MaxRow, Y),
	    I is Y*16+From,
	    send(P, display,
		 text(string('%03o/0x%02x/%03d:', I, I, I), left, fixed)),
	    new(S, string),
	    (   between(0, 15, X),
		C is 16*Y + X + From,
		C \== 0, C \== 9, C \== 10, C \== 13,
		send(A, character, 0, C),
		send(S, append, A),
		fail
	    ;   send(P, display, font_text(S, left, Font))
	    ),
	    fail
	;   true
	).

:- pce_begin_class(font_text, text,
		   "Show current character").

event(FT, Ev:event) :->
	(   send(FT, send_super, event, Ev)
	->  true
	;   send(Ev, is_a, area_exit)
	->  send(FT, report, status, '')
	;   get(FT, pointed, Ev, Index),
	    get(FT?string, character, Index, C),
	    send(FT, report, status, '%c = %03o/0x%02x/%03d', C, C, C, C)
	).

:- pce_end_class.
