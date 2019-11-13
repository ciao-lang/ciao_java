:- module(_, [], [doccfg]).

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

:- include(core_docsrc(common/'LPDOCCOMMON')).

output_name := 'ciao_java'.

filepath := ~ciaofilepath_common.
filepath := at_bundle(ciao_java, 'lib').

doc_structure := 
    'javall/javall_doc'-[
      'javall/javart',
      'javall/jtopl',
      %
      'javall/javasock'
    ].

%doc_mainopts := no_patches.
doc_mainopts := _ :- fail. % Allow patches in main changelog (those are the release notes)
% TODO: Added no_propuses because texindex breaks with very large
%       indices (due to internal, maybe arbitrary, limitations) --JF.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog|no_propuses.

bibfile := ~ciao_bibfile.

% TODO: port this manual
allow_markdown := no.
syntax_highlight := no.
