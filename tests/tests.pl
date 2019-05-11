:- use_module(prolog/quickcheck).
:- use_module(library(settings)).

:- set_setting(quickcheck:test_count, 100). 

:- load_files([
  tests/arbitrary/float,
  tests/arbitrary/list,
  tests/arbitrary/negative_integer,
  tests/shrink/string
], [ if(not_loaded) ]).