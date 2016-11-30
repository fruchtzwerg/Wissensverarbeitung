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

:- module(emacs, []).

:- use_module(library(pce)).
:- use_module(library(emacs_extend)).
:- require([ send_list/3
	   ]).

:- multifile
	no_backup/1.
:- multifile
	user:file_search_path/2.


		 /*******************************
		 *   PUT USER EXTENSIONS HERE	*
		 *******************************/

user:file_search_path(emacs_user_library,
		      user_profile('/lib/xpce/emacs')).


		 /*******************************
		 *           PROLOG		*
		 *******************************/

:- new(@loading_emacs, object).
					% SWI-Prolog extensions
pce_ifhostproperty(prolog(swi),
		   (:- ensure_loaded(user:library('emacs/swi_prolog')))).


		 /*******************************
		 *	    LIBRARIES		*
		 *******************************/

:- pce_autoload(file_item, library(file_item)).
:- pce_autoload(save_file, library(file_item)).


		 /*******************************
		 *          KERNEL FILES	*
		 *******************************/

:- consult(window).
:- consult(buffer).
:- consult(application).
:- consult(buffer_menu).
:- consult(server).
:- consult(history).
:- consult(fundamental_mode).
:- consult(language_mode).
:- consult(outline_mode).
:- consult(bookmarks).


		 /*******************************
		 *       AUTOLOAD CLASSES	*
		 *******************************/

:- pce_autoload(emacs_hit_list,		library('emacs/hit_list')).
:- pce_autoload(emacs_process_buffer,	library('emacs/shell')).
:- pce_autoload(emacs_gdb_buffer,	library('emacs/gdb')).
:- pce_autoload(emacs_annotate_buffer,  library('emacs/annotate_mode')).


		 /*******************************
		 *	      MODES		*
		 *******************************/




%:- declare_emacs_mode(outline, library('emacs/outline_mode')).
%:- declare_emacs_mode(language,library('emacs/language_mode')).
:- declare_emacs_mode(prolog,	library('emacs/prolog_mode')).
:- declare_emacs_mode(chr,	library('emacs/chr_mode')).
:- declare_emacs_mode(latex,	library('emacs/latex_mode')).
:- declare_emacs_mode(logtalk,	library('emacs/logtalk_mode')).
%:- declare_emacs_mode(html,	library('emacs/html_mode')).
:- declare_emacs_mode(java,	library('emacs/java_mode')).
:- declare_emacs_mode(javascript, library('emacs/javascript_mode')).
:- declare_emacs_mode(c,	library('emacs/c_mode')).
:- declare_emacs_mode(cpp,	library('emacs/cpp_mode')).
:- declare_emacs_mode(script,	library('emacs/script_mode')).
:- declare_emacs_mode(man,	library('emacs/man_mode')).
:- declare_emacs_mode(text,	library('emacs/text_mode')).
:- declare_emacs_mode(annotate,	library('emacs/annotate_mode')).
:- declare_emacs_mode(gdb,	library('emacs/gdb')).
:- declare_emacs_mode(sgml,	library('emacs/sgml_mode')).
:- declare_emacs_mode(xml,	library('emacs/sgml_mode')).
:- declare_emacs_mode(html,	library('emacs/sgml_mode')).
:- declare_emacs_mode(rdf,	library('emacs/rdf_mode')).
:- declare_emacs_mode(rdfs,	library('emacs/rdf_mode')).
:- declare_emacs_mode(owl,	library('emacs/rdf_mode')).
:- declare_emacs_mode(turtle,	library('emacs/turtle_mode')).


		 /*******************************
		 *     EMACS GLOBAL OBJECTS	*
		 *******************************/

:- pce_global(@emacs_base_names,
	      new(chain_table)).		  % file-base --> buffers
:- pce_global(@emacs_buffers,
	      new(dict)).			  % name --> buffer object
:- pce_global(@emacs_modes,
	      new(hash_table)).			  % name --> mode object
:- pce_global(@emacs,
	      new(emacs(@emacs_buffers))).
:- pce_global(@emacs_default_mode, new(var(value := script))).
:- pce_global(@emacs_mode_list, make_emacs_mode_list).
:- pce_global(@emacs_interpreter_mode_list, make_emacs_interpreter_mode_list).
:- pce_global(@emacs_content_mode_list, make_emacs_content_mode_list).
:- pce_global(@emacs_no_backup_list, make_no_backup_list).

make_emacs_mode_list(Sheet) :-
	new(Sheet, sheet),
	(   send(class(file), has_feature, case_sensitive, @off)
	->  CaseSensitive = @on
	;   CaseSensitive = @off
	),
	(   default_emacs_mode(Regex, Mode),
	       send(Sheet, value, regex(Regex, CaseSensitive), Mode),
	    fail
	;   true
	).

%%	default_emacs_mode(+Regex, -Mode) is nondet.
%
%	True if Mode is the PceEmacs mode   associated  with a file that
%	matches Regex.

default_emacs_mode('.*\\.pl~?$',		   prolog).
default_emacs_mode('.*\\.plu~?$',		   prolog).
default_emacs_mode('\\.(pl|xpce|pceemacs)rc~?',	   prolog).
default_emacs_mode('\\.yap~?',			   prolog).
default_emacs_mode('.*\\.chr~?$',		   chr).
default_emacs_mode('.*\\.(tex|sty)~?$',		   latex).
default_emacs_mode('.*\\.doc~?$',		   latex).
default_emacs_mode('.*\\.lgt~?$',		   logtalk).
default_emacs_mode('.*\\.html~?$',		   html).
default_emacs_mode('.*\\.php[0-9]?~?$',		   html).
default_emacs_mode('.*\\.sgml~?$',		   sgml).
default_emacs_mode('.*\\.xml~?$',		   xml).
default_emacs_mode('.*\\.ttl~?$',		   turtle).
default_emacs_mode('.*\\.rdf~?$',		   rdf).
default_emacs_mode('.*\\.rdfs~?$',		   rdfs).
default_emacs_mode('.*\\.owl~?$',		   owl).
default_emacs_mode('.*\\.ann~?$',		   annotate).
default_emacs_mode('.*\\.[ch]~?$',		   c).
default_emacs_mode('.*\\.java~?$',		   java).
default_emacs_mode('.*\\.js~?$',		   javascript).
default_emacs_mode('.*\\.C$',			   cpp).
default_emacs_mode('.*\\.cc$',			   cpp).
default_emacs_mode('.*\\.cpp$',			   cpp).
default_emacs_mode('.*\\.idl$',			   cpp).
default_emacs_mode('.*\\.txt~?$',		   text).
default_emacs_mode('[Cc]ompose|README|\\.article', text).
default_emacs_mode(Pattern, prolog) :-
	user:prolog_file_type(Ext, prolog),
	Ext \== pl,
	atomic_list_concat(['.*\\.', Ext, '~?$'], Pattern).

make_emacs_interpreter_mode_list(Sheet) :-
	new(Sheet, sheet),
	(   emacs_interpreter_mode(Regex, Mode),
	       send(Sheet, value, regex(Regex), Mode),
	    fail
	;   true
	).

%%	emacs_interpreter_mode(+Regex, -Mode) is nondet.
%
%	True if Mode must be used for   a  file starting with #!Path and
%	Path matches Regex.

emacs_interpreter_mode('.*/pl',				prolog).
emacs_interpreter_mode('.*/xpce',			prolog).
emacs_interpreter_mode('.*/perl',			c).
emacs_interpreter_mode('.*/awk',			c).

%%	emacs_content_mode(+Regex, +SearchLimit, +Mode)
%
%	Select Mode if Regex matches in the first SearchLimit characters
%	of the file.

make_emacs_content_mode_list(Sheet) :-
	new(Sheet, sheet),
	(   emacs_content_mode(Regex, SearchLimit, Mode),
	       send(Sheet, value, tuple(regex(Regex), SearchLimit), Mode),
	    fail
	;   true
	).

%%	emacs_content_mode(?Regex, ?Limit, ?Mode) is nondet.
%
%	True if Mode must be used for a   file in which Regex matches in
%	the first Limit characters of the file.

emacs_content_mode('library(chr)',	5000,	chr).


%	Do not make backup of a file matching this pattern

make_no_backup_list(Ch) :-
	new(Ch, chain),
	(   send(class(file), has_feature, case_sensitive, @off)
	->  CaseSensitive = @on
	;   CaseSensitive = @off
	),
	forall(no_backup(Pattern),
	       send(Ch, append, regex(Pattern, CaseSensitive))).

%%	no_backup(?Regex)
%
%	True if PceEmacs does not backup files that match Regex.

no_backup('/tmp/.*').
no_backup('.*/COMMIT_EDITMSG$').

:- free(@loading_emacs).


