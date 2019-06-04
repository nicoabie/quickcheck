:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

% Add the package source files relative to the current file location
:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/../prolog', PackageDir),
   asserta(user:file_search_path(package, PackageDir)).

:- use_module(package(quickcheck)).
:- use_module(library(settings)).

:- set_setting(quickcheck:test_count, 100). 

% This gen will be used across all tests with a forall processing option
test_count_generator(X) :-
  setting(quickcheck:test_count, TestCount),
  between(1, TestCount, X).

:- load_files([
  arbitrary/float,
  arbitrary/integer,
  arbitrary/list,
  arbitrary/negative_integer,
  composite/single_argument,
  shrink/string,
  shrink/composite_many,
  shrink/composite
], [ if(not_loaded) ]).