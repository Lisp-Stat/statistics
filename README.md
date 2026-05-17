
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

This repository contains two main system definitions for statistics, `streaming` and `batch`.  `batch` is the kind of implementation you probably think of first when working with statistics in R, Python or Julia.  All data is loaded into memory and then analysed.

`streaming` is based for the most part on the work of Bennett, J. et al. (2009). Numerically stable, single-pass, parallel statistics algorithms. IEEE Cluster Computing and Workshops, pp. 1-8. and West, D.H.D. (1979). Updating mean and variance estimates: An improved method. Communications of the ACM, 22(9), pp. 532-535.  These are high quality, stable numerical statistical algorithms that are somewhat unusual in statistical libraries.  These are more complete than batch and often we'll turn a matrix into a stream and then compute the descriptive statistic.

This means that if you want to contribute to batch, it's more or less a green field at the moment.

Both of these depend on an externalized [stats-generic](https://github.com/Lisp-Stat/stat-generics) system that holds the generic definiton of things like `mean`.  The reason for this is that these terms are overloaded.  We might want the `mean` of a vector, matrix, data-frame or stream so the generics live in a separate system.


<!-- GETTING STARTED -->
## Installation
To get a local copy up and running follow these steps:

```lisp
(ql:quickload :statistics)
```
_or_

```lisp
(asdf:load-system :statistics/batch)
(asdf:load-system :statistics/streaming)
```

If you already have the system downloaded to your local machine.

<!-- USAGE EXAMPLES -->
## Usage

Create a data frame of weather data:

```lisp
(load #P"LS:DATA;sg-weather")
```
and take the mean maximum temperature:
```lisp
LS-USER> (mean sg-weather:max-temps)
```

For more examples, please refer to the [Documentation](https://lisp-stat.dev/docs/).

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

- Lisp-Stat: Microsoft Public License. See [LICENSE](LICENSE)

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
