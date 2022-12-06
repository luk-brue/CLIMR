# Construal Level International Multi-lab Replication (CLIMR) Project

This project entails multi-lab collaborative replications of studies on
Construal Level Theory (CLT):

Liberman, N., & Trope, Y. (1998). The role of feasibility and desirability
considerations in near and distant future decisions: A test of temporal
construal theory. *Journal of Personality and Social Psychology, 75*(1), 5-18.
Study 1.

Fujita, K., Henderson, M., Eng, J., Trope, Y., & Liberman, N. (2006). Spatial
distance and mental construal of social events. *Psychological Science, 14*,
278-282. Study 1.

In addition to close replications of these two studies, this project also
entails two paradigmatic replications relevant to social and likelihood
distance.

## Compendium Structure

The CLIMR project compendium comprises two main parts, one hosted on the Open
Science Framework (OSF; <https://osf.io/ra3dp/>) and one hosted on GitHub
(<https://github.com/RabbitSnore/CLIMR>). The OSF repository for CLIMR contains
the materials, methods, and data (when data are collected). The GitHub
repository contains the code necessary to execute and reproduce the statistical
analyses for the project.

This repository is structured as follows:  

- The source file for compiling the project is located in the root directory.  
- The source files to render analysis reports (i.e., `.Rmd` files) in the root
directory.  
- The source files for data wrangling, visualization, simulation,
effect calculation, and analysis are located in the `/R/` directory.  
- Rendered figures are located in the `/figures/` directory.  
- Some data are located in the `/data/` directory, and data required for
analyses will be downloaded automatically from the Open Science Framework as
required.
- Rendered reports are located in the `/reports/` directory.

## (Re-)Building the Project

If you want to reproduce the analyses and output for the CLIMR project, the most
straightforward way to do this is to clone this repository into an R Project in
R Studio. [This
webpage](https://resources.github.com/whitepapers/github-and-rstudio/) has
information about how to clone repositories in R Studio. Once you have cloned
the repository, open the project in R Studio, and run the following code:

> `source("CLIMR_build_project.R")`

Running this script will install all necessary packages and build the project,
including all effect size calculations, meta-analytic models, data
visualizations, and reports.

Additionally, `CLIMR_build_project.R` includes parameters that control its
operations (e.g., whether to run a simulation or load data files). These
parameters must be changed manually, and they should only be changed if you know
what you are doing.

Additional supplemental materials are available on the Open Science Framework:
https://osf.io/ra3dp/

## Readable Reports

Reports of the preliminary studies (as well as placeholder reports using
simulated data for the main studies) are available in the `/reports/` directory.
These reports are rendered in a format that is readable on GitHub.

## IMPORTANT NOTE

Currently, the project is configured so that it will simulate data by default,
rather than load real data. No actual data for the main replication studies have
been collected. When data collection for the project is complete, the code will
be updated to import the publicly available data set by default.

# Technical Information

The CLIMR project analysis code was built using R 4.1.3. The packages required
for the project with information about the version with which the code has been
tested is available in the file `/data/meta/climr_version-info.csv`. A brief
overview of the most central required packages is provided below.

If you are attempting to reproduce the analyses and encountering errors, some
issues may be resolved by updating R and/or the required packages. For
convenience, you can set the `force_update` parameter in `CLIMR_build_project.R`
to `TRUE` if you want to update and (re)install all the required packages.

```
  Package  Version
   cowplot   1.1.1
     dplyr  1.0.10
ggbeeswarm   0.6.0
   ggplot2   3.4.0
  ggstance   0.3.6
   leaflet   2.1.1
      lme4  1.1-31
   mapview  2.11.0
   metafor   3.8-1
      osfr   0.2.9
     purrr   0.3.4
     readr   2.1.3
 rmarkdown    2.18
      simr   1.0.6
   stringr   1.4.1
     tidyr   1.2.1
   viridis   0.6.2
```
