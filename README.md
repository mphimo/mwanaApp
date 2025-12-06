

# mwanaApp: A seamless graphical interface to the mwana R package for data wrangling, plausibility checks, and prevalence estimation

<!-- badges: start -->

[![R-CMD-check](https://github.com/mphimo/mwanaApp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mphimo/mwanaApp/actions/workflows/R-CMD-check.yaml)
![check-app](https://github.com/mphimo/mwanaApp/actions/workflows/check-app.yml/badge.svg)
[![Codecov test
coverage](https://codecov.io/gh/mphimo/mwanaApp/graph/badge.svg)](https://app.codecov.io/gh/mphimo/mwanaApp)
<!-- badges: end -->

`mwanaApp` is a lightweight, field-ready application thoughtful designed
to seamlessly streamline plausibility checks and wasting prevalence
estimation of child anthropometric data, by automating key steps of the
R package mwana for non-R users.

The app is divided in five easy-to-navigate tabs:

1.  Home
2.  Data Upload
3.  Data Wrangling
4.  Plausibility Check
5.  Prevalence Analysis
6.  IPC Check

## Installation

The App can be installed from GitHub:

``` r
# First install remotes package with: install.package("remotes")
# The install mwana package from GitHub with: 
remotes::install_github(repo = "mphimo/mwanaApp", dependencies = TRUE)
```

Once installed, the app can be launched by running the below-given
command in R console:

``` r
run_mwana_app()
```

The app can be cited in this way:

``` r
citation("mwanaApp")
```

    To cite mwanaApp in publications use:

      Tomás Zaba (2025). _mwanaApp: A seamless graphical interface to the
      mwana R package for data wrangling, plausibility checks, and
      prevalence estimation_. R package version 0.0.1,
      <https://github.com/mphimo/mwanaApp.git>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {mwanaApp: A seamless graphical interface to the mwana R package for data wrangling, plausibility checks, and prevalence estimation},
        author = {{Tomás Zaba}},
        year = {2025},
        note = {R package version 0.0.1},
        url = {https://github.com/mphimo/mwanaApp.git},
      }
