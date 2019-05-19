# quickcheck

Randomized testing for Prolog à la QuickCheck  
[![CircleCI](https://circleci.com/gh/nicoabie/quickcheck.svg?style=svg)](https://circleci.com/gh/nicoabie/quickcheck)
[![SemVer](https://img.shields.io/:semver-0.2.3-brightgreen.svg)](https://semver.org/)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/nicoabie/quickcheck/labels/good%20first%20issue)

## Getting Started

Using SWI-Prolog 7.1 or later:

    ?- pack_install(quickcheck).

## Synopsis

    :- use_module(library(quickcheck)).

    % reversing a list twice gives back the same list
    prop_reverse_twice(L:list) :-
        reverse(L, R),
        reverse(R, L).

    ?- quickcheck(prop_reverse_twice/1).
    100 tests OK
    true.


## Description

Randomized testing of program properties in the spirit of [QuickCheck](http://hackage.haskell.org/package/QuickCheck).  Describe properties of your predicates and let `library(quickheck)` generate test cases for you.

A [detailed tutorial](http://blog.ndrix.com/2013/12/quickcheck-for-prolog.html) is available.