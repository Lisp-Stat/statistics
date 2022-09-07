
<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MS-PL License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/statistics">
    <img src="https://lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Lisp-Stat Statistics</h3>

  <p align="center">
	A consolidation of Common Lisp statistics libraries
	<br />
    <a href="https://lisp-stat.dev/docs/"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/statistics/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/statistics/issues">Request Feature</a>
    ·
    <a href="https://lisp-stat.github.io/statistics/">Reference Manual</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About the Project</a>
    </li>
	<li><a href="#installation">Installation</a></li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#functions">Functions</a></li>
	<li><a href="#roadmap">Roadmap</a></li>
	<li><a href="#resources">Resources</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About the Project

There are three statistics libraries that can be considered relatively complete and well written:

- The statistics library from numerical-utilities
- Larry Hunter's cl-statistics
- Gary Warren King's cl-mathstats

There are a few challenges in using these as independent systems on projects though:

- There is a good amount of overlap.  Everyone implements, for example `mean` (as does alexandria, cephes, and others)
- In the case of `mean`, `variance`, etc., the functions deal only with samples, not distributions

This library brings these three systems under a single 'umbrella', and adds a few missing ones.  To do this we use Tim Bradshaw's [conduit-packages](https://github.com/tfeb/conduit-packages).  For the few functions that require dispatch on type (sample data vs. a distribution), we use `typecase` because of its simplicity and not needing another system.  There's a slight performance hit here in the case of run-time determination of types, but until it's a problem prefer it.  Some alternatives considered for dispatch was https://github.com/pcostanza/filtered-functions.

### nu-statistics
These functions cover sample moments in detail, and are accurate.  They include up to forth moments, and are well suited to the work of an econometrist (and were written by one).

### cl-statistics
These were written by Larry Hunter, based on the methods described in Bernard Rosner's book, *Fundamentals of Biostatistics* 5th Edition, along with some from the [CLASP](https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/math/clasp/0.html) system.  They cover a wide range of statistical applications.

### gwk-statistics
These are from Gary Warren King, and also partially based on CLASP.  It is well written, and the functions have excellent documentation.  The major reason we don't include it by default is because it uses an older ecosystem of libraries that duplicate what we have not (for example, numerical utilities, alexandria).  If you want to use these, you'll need to uncomment the appropiate code in the ASDF and `pkgdcl.lisp` files.

### Accuracy
LH and GWK statistics compute quantiles, CDF, PDF, etc. using routines from CLASP, that in turn are based on algorithms from Numerical Recipes.  These are known to be accurate to only about four decimal places.   This is probably accurate enough for many stistical problem, however should you need greater accuracy look at the [distributions](https://github.com/Lisp-Stat/distributions) system.  The computations there are based on [special-functions](https://github.com/Lisp-Stat/special-functions), which has accuracy around 15 digits.  Unfortunately documentation of distributions and the 'wrapping' here are incomplete, so you'll need to know the pattern, e.g. pdf-gamma, cdf-gamma, etc., which is described in the link above.


<!-- GETTING STARTED -->
## Installation
To get a local copy up and running follow these steps:

```lisp
(ql:quickload :statistics)
```
_or_

```lisp
(asdf:load-system :statistics)
```

If you already have the system downloaded to your local machine.

If you are using SBCL you will see a large number of notes printed
about the inability to optimise.  This was the subject of issue #1 and
the short answer is that the functions all take arbitrary inputs, do
input tests specific to the calculation, and then coerce and provide
declarations so that the actual calculations can be optimized.  So,
you should be able to ignore the notes.


<!-- USAGE EXAMPLES -->
## Usage

Create a data frame of weather data:

```lisp
(load #P"LS:DATA;sg-weather")
```
and take the mean maximum temperature:
```lisp
LS-USER> (statistics-1:mean sg-weather:max-temps)
```

For more examples, please refer to the [Documentation](https://lisp-stat.dev/docs/).

You can use a [package local
nickname](https://lispcookbook.github.io/cl-cookbook/packages.html#package-local-nicknames-pln)
to give the package a shorter name, e.g. "stats" if you like.

Often times all you'll need is lh-stats for general statistical
analysis.  You can load that with:
```
(asdf:load-system :statistics/lh)
```

**NB** You can expect to see many warnings when loading lh-stats.  These are expected and nothing to worry about.


## LH-Stat Functions

These abreviations are used in function and variable names:
| abbreviation | meaning |
| --- | --- |
| ci  | confidence interval          |
| cdf | cumulative density function  |
| ge  | greater than or equal to     |
| le  | less than or equal to        |
| pdf | probability density function |
| sd  | standard deviation           |
| rxc | rows by columns              |
| sse | sample size estimate         |


### Descriptive statistics

- mean
- median
- mode
- geometric mean
- range
- percentile
- variance
- standard-deviation (sd)
- coefficient-of-variation
- standard-error-of-the-mean

### Distribution functions

- Poisson & Binomial
- binomial-probability
- binomial-cumulative-probability
- binomial-ge-probability
- poisson-probability
- poisson-cumulative-probability
- poisson-ge-probability
- normal
- normal-pdf
- convert-to-standard-normal
- phi
- z
- t-distribution
- chi-square
- chi-square-cdf

### Confidence Intervals

- binomial-probability-ci
- poisson-mu-ci
- normal-mean-ci
- normal-mean-ci-on-sequences
- normal-variance-ci
- normal-variance-ci-on-sequence
- normal-sd-ci

### Hypothesis tests (parametric)

- z-test
- z-test-on-sequence
- t-test-one-sample
- t-test-one-sample-on-sequence
- t-test-paired
- t-test-paired-on-sequences
- t-test-two-sample
- t-test-two-sample-on-sequences
- chi-square-test-one-sample
- f-test
- binomial-test-one-sample
- binomial-test-two-sample
- fisher-exact-test
- mcnemars-test
- poisson-test-one-sample

### Hypothesis tests (non-parametric)

- sign-test
- sign-test-on-sequence
- wilcoxon-signed-rank-test
- chi-square-test-rxc
- chi-square-test-for-trend

### Sample size estimates

- t-test-one-sample-sse
- t-test-two-sample-sse
- t-test-paired-sse
- binomial-test-one-sample-sse
- binomial-test-two-sample-sse
- binomial-test-paired-sse
- correlation-sse

### Correlation and Regression

- linear-regression
- correlation-coefficient
- correlation-test-two-sample
- spearman-rank-correlation

### Significance test functions

- t-significance
- f-significance (chi square significance is calculated from chi-square-cdf in various ways depending on the problem)

### Utilities

- random-sample
- random-pick
- bin-and-count
- fishers-z-transform
- mean-sd-n
- square
- choose
- permutations
- round-float


<!-- ROADMAP -->
## Roadmap
gwk-stats has many useful functions. We'd like to port them to use the Lisp-Stat ecosystem of utilities.


## Resources

This system is part of the [Lisp-Stat](https://lisp-stat.dev/)
project; that should be your first stop for information. Also see the
[resources](https://lisp-stat.dev/resources) and
[community](https://lisp-stat.dev/community) pages for more
information.

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**. Please see [CONTRIBUTING](CONTRIBUTING.md) for details on the code of conduct and the process for submitting pull requests.

<!-- LICENSE -->
## Licenses

Lisp-Stat: Microsoft Public License. See [LICENSE](LICENSE)
LH Stats: MIT License. See [LH-LICENSE](LH-LICENSE)
GWK-Stats: BSD-3-Clause. See [GWK-LICENSE](https://github.com/gwkkwg/cl-mathstats/blob/master/COPYING)

## CLASP Copyright
Copyright (c) 1990 - 1994 University of Massachusetts
Department of Computer Science
Experimental Knowledge Systems Laboratory
Professor Paul Cohen, Director.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of EKSL, this paragraph and the one following appear
in all copies and in supporting documentation.

EKSL makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall EKSL be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if EKSL is advised of the possiblity of
such damages.


<!-- CONTACT -->
## Contact

Project Link: [https://github.com/lisp-stat/statistics](https://github.com/lisp-stat/statistics)



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/statistics.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/statistics/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/statistics.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/statistics/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/statistics.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/statistics/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/statistics.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/statistics/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/statistics.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/statistics/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/
