![LSMR](media/logo.png)
============

LSMR: Sparse Equations and Least Squares.

This is a slightly modernized version of the original FSMR (f90 version) by David Fong & Michael Saunders, Systems Optimization Laboratory (SOL), Stanford University, Stanford, CA 94305-4026, USA.

### Status

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![GitHub release](https://img.shields.io/github/release/jacobwilliams/LSMR.svg)](https://github.com/jacobwilliams/LSMR/releases/latest)
[![CI Status](https://github.com/jacobwilliams/LSMR/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/LSMR/actions)
[![codecov](https://codecov.io/gh/jacobwilliams/LSMR/branch/master/graph/badge.svg)](https://codecov.io/gh/jacobwilliams/LSMR)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/LSMR)](https://github.com/jacobwilliams/LSMR/commits/master)

### Compiling

A [Fortran Package Manager](https://github.com/fortran-lang/fpm) manifest file is included, so that the library and test cases can be compiled with FPM. For example:

```
fpm build --profile release
fpm test --profile release
```

To use `lsmr` within your fpm project, add the following to your `fpm.toml` file:
```toml
[dependencies]
LSMR = { git="https://github.com/jacobwilliams/LSMR.git" }
```

### License

The original version of LSMR (f90 version) was provided by SOL, Stanford University under the terms of the [OSI Common Public License (CPL)](http://www.opensource.org/licenses/cpl1.0.php) or the [BSD License](http://www.opensource.org/licenses/bsd-license.php).

### Documentation

The latest API documentation can be found [here](https://jacobwilliams.github.io/LSMR/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

### See also

 * [LSMR: Sparse Equations and Least Squares](https://web.stanford.edu/group/SOL/software/lsmr/) -- LSMR original code.
 * [Sparse Parallel Robust Algorithms Library](https://github.com/ralna/spral) -- another version
 * [LSQR](https://github.com/jacobwilliams/LSQR)
 * [LUSOL](https://github.com/jacobwilliams/lusol)
 * [nlesolver-fortran](https://github.com/jacobwilliams/nlesolver-fortran)
