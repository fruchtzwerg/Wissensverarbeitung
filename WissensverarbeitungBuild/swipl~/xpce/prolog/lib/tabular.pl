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

:- module(tabular, []).
:- use_module(library(pce)).

:- pce_begin_class(tabular, device,
		   "Device with associated table <-layout_manager").

delegate_to(layout_manager).

initialise(TD, Table:[table]) :->
	send_super(TD, initialise),
	(   Table == @default
	->  send(TD, layout_manager, new(_, tabular_table))
	;   send(TD, layout_manager, Table)
	).

:- pce_group(appearance).


:- pce_group(event).

:- pce_global(@tabular_device_recogniser,
	      new(resize_table_slice_gesture(column, left))).

event(RT, Ev:event) :->
	get(RT, table, Table),
	(   get(Table, cell_from_position, Ev, Cell),
	    send(Cell, instance_of, table_cell),
	    get(Cell, image, Gr),
	    (   send(Gr, has_send_method, on_mark_clicked),
	        send(Ev, is_a, button),
		get(Cell, note_mark, Mark),
		Mark \== @nil,
		get(Cell, area, area(X, Y, W, _)),
		get(Ev, position, RT, point(EX, EY)),
		get(Mark, size, size(MW, MH)),
		EX > X+W-MW,
		EY < Y+MH
	    ->	(   send(Ev, is_up)
		->  send(Gr, on_mark_clicked)
		;   true
		)
	    ;   send(Ev, post, Gr)
	    )
	;   send(@tabular_device_recogniser, event, Ev)
	).

:- pce_group(geometry).

table_width(TD, W:int) :->
	"Set width of the table"::
	send(TD?table, width, W).

:- pce_group(parts).

table(TD, Table:table) :<-
	"Get the table layout_manager"::
	get(TD, layout_manager, Table).

:- pce_group(fill).

append(TD,
       Label:label='name|graphical|table_cell',
       Font:font=[font],
       HAlign:halign=[{left,center,right}],
       VAlign:valign=[{top,center,bottom}],
       Span:colspan='[1..]',
       RSpan:rowspan='[1..]',
       BG:background=[colour],
       FG:colour=[colour]) :->
	"Append a cell to the table"::
	get(TD, table, Table),
	(   atom(Label)
	->  new(TC, table_cell(text(Label, @default, Font)))
	;   send(Label, instance_of, graphical)
	->  new(TC, table_cell(Label)),
	    (   Font \== @default
	    ->	send(Label, font, Font)
	    ;	true
	    )
	;   TC = Label,
	    (   Font \== @default
	    ->	send(Label, font, Font)
	    ;	true
	    )
	),
	(   HAlign \== @default
	->  send(TC, halign, HAlign)
	;   true
	),
	send(TC, background, BG),
	(   FG \== @default
	->  send(TC?image, colour, FG)
	;   true
	),
	(   Span \== @default
	->  send(TC, col_span, Span)
	;   true
	),
	(   RSpan \== @default
	->  send(TC, row_span, RSpan)
	;   true
	),
	(   VAlign \== @default
	->  send(TC, valign, VAlign)
	;   true
	),
	send(Table, append, TC).

clear(TD) :->
	"Delete all rows"::
	get(TD, table, Table),
	send(Table, delete_rows).

:- pce_group(label).

append_label_button(TD, Field:name) :->
	"Append a button to sort the field"::
	get(TD, layout_manager, Table),
	get(Table, current, point(X, Y)),
	send(Table, append,
	     new(TC, table_cell(new(B, button(Field,
					      message(TD, sort_rows,
						      X, Y+1)))))),
	send(B, radius, 0),
	get(class(dialog), class_variable, background, BGVar),
	send(TC, background, BGVar?value),
	send(TC, cell_padding, size(0,0)),
	send(TC, halign, stretch).

:- pce_group(sort).

sort_rows(TD, Col:int, FromRow:int) :->
	"Sort rows starting at FromRow on the indicated column"::
	format('~p: Sorting rows below ~w on column ~w~n', [TD, FromRow, Col]).

:- pce_end_class(tabular).


		 /*******************************
		 *	    THE TABLE		*
		 *******************************/

:- pce_begin_class(tabular_table, table,
		   "The layout manager class tabular").

stretched_column(Table, Col:table_column, W:int) :->
	"Adjust the size of cells holding a wrapped text"::
	get(Col, index, Index),
	send(Col, for_all, message(Table, stretched_cell, @arg1, W, Index)),
	send_super(Table, stretched_column, Col, W).

stretched_cell(T, Cell:table_cell, W:int, ColN:int) :->
	(   get(Cell, image, Graphical),
	    send(Graphical, instance_of, graphical)
	->  (   send(Graphical, has_send_method, margin),
	        get(Graphical, send_method, margin,
		    tuple(Graphical, Method)),
		get(Method, argument_type, 2, T2),
		send(T2, validate, wrap)
	    ->  spanned_cell_width(Cell, ColN, W, T, TextW),
		send(Graphical, margin, TextW, wrap)
	    ;   send(Graphical, instance_of, device),
		get(Graphical, format, Format),
		Format \== @nil,
		get(Format, columns, @off)
	    ->  spanned_cell_width(Cell, ColN, W, T, TextW),
		send(Graphical, format, width, TextW)
	    ;   send(Graphical, has_get_method, auto_align),
		get(Graphical, auto_align, @on)
	    ->  spanned_cell_width(Cell, ColN, W, T, TextW),
		send(Graphical, do_set, width := TextW)
	    ;	true
	    )
	;   true
	).

spanned_cell_width(Cell, ColN, W, T, TextW) :-
	get(Cell, col_span, Span),
	get(Cell, column, Col0),
	EndCol is Col0+Span,
	cell_width(Col0, EndCol, ColN, W, T, 0, TotalW),
	(   get(Cell, cell_padding, size(PW, _))
	->  TextW is TotalW - PW*2
	;   get(Cell?table, cell_padding, size(PW, _))
	->  TextW is TotalW - PW*2
	;   TextW is TotalW
	).

%	Determine the width of a spanned cell.

cell_width(End, End, _, _, _, W, W) :- !.
cell_width(C, End, N, W, T, W0, Width) :-
	(   C == N
	->  W1 is W0 + W
	;   get(T, column, C, Col),
	    get(Col, width, WC),
	    W1 is W0 + WC
	),
	C2 is C + 1,
	cell_width(C2, End, N, W, T, W1, Width).

:- pce_end_class(tabular_table).
