LSMR: Sparse Equations and Least Squares

### Status
[![GitHub release](https://img.shields.io/github/release/jacobwilliams/LSMR.svg)](https://github.com/jacobwilliams/LSMR/releases/latest)
[![CI Status](https://github.com/jacobwilliams/LSMR/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/LSMR/actions)
[![codecov](https://codecov.io/gh/jacobwilliams/LSMR/branch/master/graph/badge.svg)](https://codecov.io/gh/jacobwilliams/LSMR)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/LSMR)](https://github.com/jacobwilliams/LSMR/commits/master)

### Readme

README for lsmrf90

The software for LSMR (f90 version) is provided by SOL, Stanford University
under the terms of the OSI Common Public License (CPL)
   http://www.opensource.org/licenses/cpl1.0.php
or the BSD License
   http://www.opensource.org/licenses/bsd-license.php

17 Jul 2010: f90 version of LSMR derived from f90 verion of LSQR
             and Matlab version lsmr.m.
             Separate modules are used for LSMR, example test problems,
             and Check routines for A and x.

             LSMR lives in lsmrModule.f90.
             Aprod1, Aprod2 (matrix-vector product routines) are in
             lsmrTestModule.f90.  This module illustrates how
             problem-specific data can be created for Aprod1, Aprod2
             even though LSMR calls them with a simple parameter list.

17 Jul 2010: 3 of the 18 test problems request local reorthogonalization
             (localSize = 10).  These 3 tests currently fail.
07 Sep 2010: Local reorthogonalization now works (localSize > 0).
26 Oct 2012: lsmrTestProgram outputs helpful info to the screen.

Maintained by
 David Fong       <david3.14159@gmail.com>
 Michael Saunders <saunders@stanford.edu>
 Systems Optimization Laboratory (SOL)
 Stanford University
 Stanford, CA 94305-4026, USA
-----------------------------------------------------------------------------

LSMR (f90 version) involves the following files:

   lsmrblas.f90         (not needed if Level 1 BLAS are available)
   lsmrblasInterface.f90
   lsmrCheckModule.f90
   lsmrDataModule.f90
   lsmrModule.f90
   lsmrTestModule.f90
   lsmrTestProgram.f90
   LSMR.txt             (example output file from an Intel Xeon system
                         compiled with gfortran -O on Linux Debian Elive)
   Makefile
   README

To compile the code and run the test program on Linux or Unix,
proceed as follows:

   make                 (creates executable TestProgram)
   ./TestProgram
   grep appears LSMR.txt

"LSMR  appears to be successful" should occur 20 times.
"LSMR  appears to have failed" might occur for the most
ill-conditioned problem, but this is not cause for alarm
if ||A'r|| is very small (~= 1e-12).


### See also
 * [LSMR: Sparse Equations and Least Squares](https://web.stanford.edu/group/SOL/software/lsmr/) -- LSMR original code.
 * [Sparse Parallel Robust Algorithms Library](https://github.com/ralna/spral) -- another version
