# quickcheck

Randomized testing of program properties in the spirit of [QuickCheck](http://hackage.haskell.org/package/QuickCheck).  
Describe properties of your predicates and let `library(quickheck)` generate test cases for you.  
[![CircleCI](https://circleci.com/gh/nicoabie/quickcheck.svg?style=shield)](https://circleci.com/gh/nicoabie/quickcheck)
[![SemVer](https://img.shields.io/:SemVer-0.2.3-brightgreen.svg)](https://semver.org/)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/nicoabie/quickcheck/labels/good%20first%20issue)
[![License](https://img.shields.io/badge/license-UNLICENSE-brightgreen.svg)](https://unlicense.org)

## Example

    :- use_module(library(quickcheck)).

    % reversing a list twice gives back the same list
    prop_reverse_twice(L:list) :-
        reverse(L, R),
        reverse(R, L).

    prop_silly_list(L:list(integer)) :-
        length(L, Len),
        Len =:= 3.  % nonsense!

    ?- quickcheck(prop_reverse_twice/1).
    100 tests OK
    true.

    ?- quickcheck(prop_silly_list/1).
    Shrinking to depth 1
    Failed test prop_silly_list([]:list(integer))
    false.


## Installation

To install as a package:

    ?- pack_install(quickcheck).

Tested with Swi-Prolog 8.0.x but should work with earlier versions too.

## Running tests

In the package root, insert into swipl:

    [tests/tests].
    run_tests.

Or if you cloned the repo:

    make test

## Built With

* [library(apply)](http://www.swi-prolog.org/pldoc/man?section=apply) - Apply predicates on a list
* [library(error)](http://www.swi-prolog.org/pldoc/man?section=error) - Error generating support
* [library(random)](http://www.swi-prolog.org/pldoc/man?section=random) - Random numbers
* [library(settings)](http://www.swi-prolog.org/pldoc/man?section=settings) - Setting management

## Bug reports/feature requests

Please send bug reports/feature request through the GitHub
project [page](https://github.com/nicoabie/quickcheck).

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/nicoabie/quickcheck/tags). 

## Authors

* **Michael Hendricks** - *Initial work* - [quickcheck for prolog](http://blog.ndrix.com/2013/12/quickcheck-for-prolog.html)

See also the list of [contributors](https://github.com/nicoabie/quickcheck/contributors) who participated in this project.

## License

The UNLICENSE license. See the [LICENSE](LICENSE) file for details.