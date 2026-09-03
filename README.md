

# mwanaApp: A seamless graphical interface to the mwana R package for data wrangling, plausibility checks, and prevalence estimation

<!-- badges: start -->

[![R-CMD-check](https://github.com/mphimo/mwanaApp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mphimo/mwanaApp/actions/workflows/R-CMD-check.yaml)
[![Test
app](https://github.com/mphimo/mwanaApp/actions/workflows/test-app-description.yaml/badge.svg)](https://github.com/mphimo/mwanaApp/actions/workflows/test-app-description.yaml)
[![Test
coverage](https://github.com/mphimo/mwanaApp/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/mphimo/mwanaApp/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/mphimo/mwanaApp/graph/badge.svg)](https://app.codecov.io/gh/mphimo/mwanaApp)
<!-- badges: end -->

`mwanaApp` is a lightweight, field-ready application thoughtful designed
to seamlessly streamline plausibility checks and wasting prevalence
estimation of child anthropometric data, by automating key steps of the
R package mwana for non-R users.

The app is divided in six easy-to-navigate tabs:

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

      Tomás Zaba (2026). _mwanaApp: A seamless graphical interface to the
      mwana R package for data wrangling, plausibility checks, and
      prevalence estimation of wasting_. R package version 0.2.2,
      <https://github.com/mphimo/mwanaApp>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {mwanaApp: A seamless graphical interface to the mwana R package for data wrangling, plausibility checks, and prevalence estimation of wasting},
        author = {{Tomás Zaba}},
        year = {2026},
        note = {R package version 0.2.2},
        url = {https://github.com/mphimo/mwanaApp},
      }
