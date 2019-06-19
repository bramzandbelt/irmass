<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Last-changedate](https://img.shields.io/badge/last%20change-2019--06--19-brightgreen.svg)](https://github.com/bramzandbelt/irmass/commits/master) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.6.0-brightgreen.svg)](https://cran.r-project.org/) [![Task DOI](https://zenodo.org/badge/49258308.svg)](https://zenodo.org/badge/latestdoi/49258308) [![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0002--6491--1247-green.svg)](https://orcid.org/0000-0002-6491-1247)

irmass - Research compendium for the report on independent race model analysis of selective stopping by Zandbelt & Van den Bosch
================================================================================================================================

Compendium DOI
--------------

<!-- TODO: Add Zenodo DOI -->
The files at the URL above will generate the results as found in the preprint. The files hosted at <https://github.com/bramzandbelt/irmass> are the development versions and may have changed since the preprint was published.

Author of this repository
-------------------------

Bram Zandbelt (<bramzandbelt@gmail.com>)

Published in:
-------------

TBA

<!-- TODO: Add PsyArXiv DOI and Bibliography
```
 
```
-->
Overview of contents
--------------------

The packagae `irmass` is one of two research compendia of the research project *Cognitive and Neurobiological Mechanisms of Selective Stopping* by Bram Zandbelt and Ruben van den Bosch (the other research compendium, `cnmss`, can be found [here](github.com/bramzandbelt/cnmss)<!-- TODO: Check Github URL -->). This project was conducted at the Donders Institute, Radboud University, Nijmegen, the Netherlands, and registered at the Donders Centre for Cognitive Neuroimaging under project number 3017031.05 (DCCN PI: Roshan Cools).

This research compendium contains all data, code, and text associated with the above-mentioned publication and is organized as follows (showing directories in a tree-like format with a maximum depth of two levels):

    .
    ├── MATLAB
    ├── R
    ├── analysis
    │   ├── bash
    │   └── notebooks_and_scripts
    ├── data
    │   ├── derivatives
    │   ├── raw
    │   └── simulations
    ├── documents
    │   ├── content
    │   ├── context
    │   └── manuscript
    ├── figures
    │   ├── 01_preprocess_log_files
    │   ├── 02_assess_task_performance_criteria
    │   ├── 03_individual_analysis_effect_ssd_on_prob_responding_given_stopsignal
    │   ├── 04_individual_analysis_rt_difference_nosignal_stoprespond
    │   ├── 05_individual_analysis_effect_ssd_on_stoprespond_rt
    │   ├── 07_group_analysis_rt_difference_nosignal_stoprespond
    │   ├── 08_group_analysis_effect_ssd_on_stoprespond_rt
    │   ├── 09_exploration_support_for_hypotheses_under_different_bayes_factor_criteria
    │   ├── 10_exploration_support_for_hypotheses_under_different_priors
    │   └── 11_analysis_of_simulated_race_model_data
    ├── man
    ├── opt
    ├── packrat
    │   ├── lib
    │   ├── lib-R
    │   ├── lib-ext
    │   └── src
    └── reports

The `MATLAB/` directory contains:

-   MATLAB code specific to the present project; specifically, for simulating the impact of race model violations on SSRT.

The `R/` directory contains:

-   R code specific to the present project; functions are organized into files (e.g. functions for plotting are in `plot_functions.R`).

The `analysis/` directory contains:

-   R Markdown notebooks implementing the analyses (`notebooks_and_scripts/` directory), numbered in the order in which they should be run;
-   shell scripts running the R Markdown notebooks with appropriate parameters, if any (`bash/` directory).

The `data/` directory contains:

-   the raw performance data (`raw/` directory), organized by subject index;
    -   the performance data from the experiment session (`experiment/` directory):
        -   `triallog_*.csv` contains the raw trial-by-trial data (see codebook in `documents/content/codebook_triallog.csv`)
        -   `blocklog_*.csv` contains the block-by-block summary statistics of performance data (see codebook in `documents/content/codebook_blocklog.csv`)
        -   `runtimeinfo_*.csv` contains runtime from PsychoPy (see <https://www.psychopy.org/api/info.html>)
    -   the performance data from the practice session (`practice/` directory):
        -   contains files in same format as under `experiment/`
-   the data derived from the raw data (`derivatives/` directory), organized by notebook name.
    -   for meaning of output variables, see notebook templates (`analysis/*.Rmd`) and static reports (`reports/`)
-   the simulated performance data (`simulations/` directory)

The `documents/` directory contains:

-   documents describing the content of the experimental data (`content/` directory), such as codebooks;
-   documents describing the context of the data (`context/` directory), such as ethics documents and preregistration;
-   documents related to the report of this research project (`manuscript/` directory).

The `figures/` directory contains:

-   visualizations of descriptive and inferential statistics, organized by notebook name.

The `man/` directory contains:

-   documentation of objects inside the package, generated by `roxygen2`.

The `opt` directory contains:

-   MATLAB packages the research compendium depends on, including packages for:
    -   making multipanel figures (`panel/`);

The `packrat/` directory contains:

-   R packages the research compendium depends on; for more info see <https://rstudio.github.io/packrat/>.

The `reports/` directory contains:

-   static HTML versions of the knitted R Markdown notebooks, organized by notebook name.

Finally, this research compendium is associated with a number of online objects, including:

<table>
<colgroup>
<col width="9%" />
<col width="45%" />
<col width="45%" />
</colgroup>
<thead>
<tr class="header">
<th>object</th>
<th>archived version</th>
<th>development version</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>preregistration</td>
<td><a href="https://osf.io/mq64z/" class="uri">https://osf.io/mq64z/</a></td>
<td>NA</td>
</tr>
<tr class="even">
<td>stimulus presentation code</td>
<td><a href="https://doi.org/10.5281/zenodo.3243799" class="uri">https://doi.org/10.5281/zenodo.3243799</a></td>
<td><a href="github.com/bramzandbelt/StPy" class="uri">github.com/bramzandbelt/StPy</a></td>
</tr>
</tbody>
</table>

How to use
----------

This repository is organized as an R package. The R package structure was used to help manage dependencies, to take advantage of continuous integration for automated code testing and documentation, and to be able to follow a standard format for file organization. The package `irmass` depends on other R packages and non-R programs, which are listed below under [Dependencies](#Dependencies).

To download the package source as you see it on GitHub, for offline browsing, use this line at the shell prompt (assuming you have Git installed on your computer):

Install `irmass` package from Github:

``` r
devtools::install_github("bramzandbelt/irmass")
```

Once the download is complete, open the file `irmass.Rproj` in RStudio to begin working with the package and compendium files. To reproduce all analyses, run the shell script `analysis/bash/run_all_analyses.sh`. This will run all RMarkdown notebooks in correct order. It may take several hours to complete.

Licenses
--------

Manuscript: CC-BY-4.0 <http://creativecommons.org/licenses/by/4.0/>

Code: MIT <http://opensource.org/licenses/MIT>, year: 2019, copyright holder: Bram B. Zandbelt

Data: Data Use Agreement of Donders Institute <!-- TODO: Add URL -->

Dependencies
------------

Below is the output of `sessionInfo()`, showing version information about R, the OS, and attached or loaded packages:

``` r
devtools::session_info()
#> ─ Session info ──────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 3.6.0 (2019-04-26)
#>  os       macOS Mojave 10.14.5        
#>  system   x86_64, darwin15.6.0        
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       Europe/Amsterdam            
#>  date     2019-06-19                  
#> 
#> ─ Packages ──────────────────────────────────────────────────────────────
#>  package     * version date       lib source        
#>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.0)
#>  backports     1.1.4   2019-04-10 [1] CRAN (R 3.6.0)
#>  callr         3.2.0   2019-03-15 [1] CRAN (R 3.6.0)
#>  cli           1.1.0   2019-03-19 [1] CRAN (R 3.6.0)
#>  commonmark    1.7     2018-12-01 [1] CRAN (R 3.6.0)
#>  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.0)
#>  desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.0)
#>  devtools    * 2.0.2   2019-04-08 [1] CRAN (R 3.6.0)
#>  digest        0.6.19  2019-05-20 [1] CRAN (R 3.6.0)
#>  evaluate      0.14    2019-05-28 [1] CRAN (R 3.6.0)
#>  fs            1.3.1   2019-05-06 [1] CRAN (R 3.6.0)
#>  glue          1.3.1   2019-03-12 [1] CRAN (R 3.6.0)
#>  htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.6.0)
#>  knitr         1.23    2019-05-18 [1] CRAN (R 3.6.0)
#>  magrittr      1.5     2014-11-22 [1] CRAN (R 3.6.0)
#>  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.6.0)
#>  packrat       0.5.0   2018-11-14 [1] CRAN (R 3.6.0)
#>  pkgbuild      1.0.3   2019-03-20 [1] CRAN (R 3.6.0)
#>  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.6.0)
#>  prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.6.0)
#>  processx      3.3.1   2019-05-08 [1] CRAN (R 3.6.0)
#>  ps            1.3.0   2018-12-21 [1] CRAN (R 3.6.0)
#>  R6            2.4.0   2019-02-14 [1] CRAN (R 3.6.0)
#>  Rcpp          1.0.1   2019-03-17 [1] CRAN (R 3.6.0)
#>  remotes       2.0.4   2019-04-10 [1] CRAN (R 3.6.0)
#>  rlang         0.3.4   2019-04-07 [1] CRAN (R 3.6.0)
#>  rmarkdown     1.13    2019-05-22 [1] CRAN (R 3.6.0)
#>  roxygen2    * 6.1.1   2018-11-07 [1] CRAN (R 3.6.0)
#>  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.0)
#>  rstudioapi    0.10    2019-03-19 [1] CRAN (R 3.6.0)
#>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.0)
#>  stringi       1.4.3   2019-03-12 [1] CRAN (R 3.6.0)
#>  stringr       1.4.0   2019-02-10 [1] CRAN (R 3.6.0)
#>  testthat      2.1.1   2019-04-23 [1] CRAN (R 3.6.0)
#>  usethis     * 1.5.0   2019-04-07 [1] CRAN (R 3.6.0)
#>  withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.0)
#>  xfun          0.7     2019-05-14 [1] CRAN (R 3.6.0)
#>  xml2          1.2.0   2018-01-24 [1] CRAN (R 3.6.0)
#>  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.6.0)
#> 
#> [1] /Users/bramzandbelt/surfdrive/projects/irmass/packrat/lib/x86_64-apple-darwin15.6.0/3.6.0
#> [2] /Users/bramzandbelt/surfdrive/projects/irmass/packrat/lib-ext/x86_64-apple-darwin15.6.0/3.6.0
#> [3] /Users/bramzandbelt/surfdrive/projects/irmass/packrat/lib-R/x86_64-apple-darwin15.6.0/3.6.0
```

For a complete list of the packrat Packrat takes care of dependencies.

Acknowledgment
--------------

Thanks to Ben Marwick for inspiration on [how to create, organize, and describe research compendia](https://github.com/benmarwick/researchcompendium).

Contact
-------

[Bram B. Zandbelt](mailto:bramzandbelt@gmail.com)

``` r
# Ignore rest of document
knitr::knit_exit()
```
