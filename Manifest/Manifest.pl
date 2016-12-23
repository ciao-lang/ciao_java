:- bundle(ciao_java).
version('1.0').
depends([
    core-[version>='1.15']
]).
alias_paths([
    library = 'lib'
]).
lib('lib').
manual('ciao_java', [main='doc/SETTINGS.pl']).
