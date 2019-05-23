:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

% Add the package source files relative to the current file location
:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/../prolog', PackageDir),
   asserta(user:file_search_path(package, PackageDir)).

:- use_module(package(quickcheck)).
:- use_module(library(settings)).

:- set_setting(quickcheck:test_count, 100). 

:- load_files([
  arbitrary/float,
  arbitrary/list,
  arbitrary/negative_integer,
  arbitrary/arbitrary_type,
  shrink/string
], [ if(not_loaded) ]).