:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao Java").

% ===========================================================================
:- doc(section, "Configuration rules").

:- use_module(library(system), [find_executable/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(lists), [append/3]).
:- use_module(engine(system_info), [get_platform/1]).

:- bundle_flag(enabled, [
    comment("Enable Java interface"),
    valid_values(['yes', 'no']),
    %
    default_comment("javac and javadoc detected"),
    default_value_comment(no,
        "Java has not been detected. If you would like to use the\n"||
        "utilities for the Java interface it is highly recommended that\n"||
        "you stop the Ciao configuration now and install Java first."),
% 	    "If Java is already installed and you use Debian/Ubuntu perhaps\n"||
% 	    "you forgot to run: sudo update-java-alternatives --set java-6-sun."
    rule_default(HasJava, has_javac(HasJava)),
    %
    interactive([advanced])
]).

has_javac(Value) :-
	( javac_installed, javadoc_installed -> Value = yes ; Value = no ).

javac_installed :-
	find_executable('javac', _),
	is_sun_javac.

% TODO: Any better way to detect Sun Java? (EMM)
is_sun_javac :-
	process_call(path(javac), ['-version'],
	             [stderr(stdout), stdout(string(String)), status(_)]),
	append(_, "javac 1."||_, String),
	process_call(path(java), ['-version'],
	             [stderr(stdout), stdout(string(SJava)), status(_)]),
	% Kludge: In linux 64, you have to use the 64-bit Server VM --EMM
	( ( get_platform('LINUXi686') ; get_platform('LINUXx86_64') ) ->
	    append(_, "64-Bit"||_, SJava)
	; true
	),
	!.

javadoc_installed :- find_executable('javadoc', _).

:- doc(subsection, "Ant (Java build tool)").

% Detect Apache Ant
% TODO: should it be configurable?
:- bundle_flag(with_ant, [
    comment("Enable Ant support"), % http://ant.apache.org/
    details(
      % .....................................................................
      "Set to \"yes\" if you wish to enable Ant support.\n"),
    valid_values(['yes', 'no']),
    %
    default_comment("Ant detected"),
    rule_default(HasAnt, (
      flag(enabled(EnabledJava)),
      has_ant(EnabledJava, HasAnt))),
    %
    interactive([advanced])
]).

has_ant(no,  no).
has_ant(yes, HasAnt) :-
	has_ant_(HasAnt).

has_ant_(Value) :-
	( ant_installed -> Value = yes ; Value = no ).

ant_installed :- find_executable('ant', _).

:- bundle_flag(ant_cmd, [
    comment("Path of Apache Ant command"),
    rule_set_value(Value, get_ant_cmd(Value))
]).

get_ant_cmd(CmdPath) :-
	( find_executable('ant', Path0) ->
	    CmdPath = Path0
	; % error_message("Cannot find any version of 'ant' command in the path."),
	  CmdPath = '' % TODO: Make it optional
	  % fail
	).

% ===========================================================================
:- doc(section, "Build rules").

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(config_common), [with_docs/1]).

% TODO: add an item for the Prolog part
% TODO: add more items for the plserver etc
% TODO: remove most of Makefile completely

'$builder_hook'(item_nested(java)). % item for .java code

enabled := ~get_bundle_flag(ciao_java:enabled).

'$builder_hook'(java:build_bin) :- !,
	( enabled(yes) ->
	    invoke_gmake_javall(build)
	; true
	).
'$builder_hook'(java:build_docs) :- !,
	% TODO: missing installation of docs
	( enabled(yes) ->
	    ( with_docs(yes) ->
	        invoke_gmake_javall(docs)
	    ; true
	    )
	; true
	).

'$builder_hook'(java:clean_bin) :-
	( enabled(yes) ->
	    invoke_gmake_javall(distclean) % TODO: 'clean' or 'distclean'?
	; true
	).

:- use_module(ciaobld(builder_aux), [invoke_gmake/2]).

invoke_gmake_javall(Cmd) :-
	invoke_gmake(~bundle_path(ciao_java, 'lib/javall'), [Cmd]).

% ----------------------------------------------------------------
:- doc(section, "Tests and Benchmarks").
% TODO: Add bundle defs for unit tests, integration tests, regression
%   tests, etc.

:- use_module(library(system), [working_directory/2]).
:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).
:- use_module(ciaobld(ciaoc_aux), [invoke_ciaosh_batch/1]).

'$builder_hook'(test) :- !,
	% runtests_dir(ciao_java, 'tests'),
	runtests_ciaotests_hook.

% (integration tests)
runtests_ciaotests_hook :-
	working_directory(ThisDir, ~bundle_path(ciao_java, 'tests')),
	invoke_ciaosh_batch([
          use_module(library(unittest), [show_test_summaries/1, run_tests_in_module/3]),
	  run_tests_in_module(test_java, [dump_output, dump_error, rtc_entry], TS),
	  show_test_summaries(TS)
	]),
	working_directory(_, ThisDir).

