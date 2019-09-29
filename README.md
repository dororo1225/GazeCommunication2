GazeCommunication2
====

## Overview
This folder includes rstan codes, graphing codes, and data for the following paper:

Yamamoto, H., Sato, A., & Itakura, S. (submit). Transition from crawling to walking changes gaze communication space in everyday infant-parent interaction.

## Description
It consists maily of three folders:
- Analysis 1
  - This folder includes rstan codes, graphing codes. Main purpose is to compare statistical models in Analysis 1.
- Analysis 2
  - This folder includes rstan codes, graphing codes. Main purpose is to compare statistical models in Analysis 2.
- Analysis 3
  - This folder includes rstan codes, graphing codes. Main purpose is to compare statistical models in Analysis 3.

## Data
- Data_locomotion.csv
  - data about proportion of infant's walking time
  - used in Analysis 1 & Analysis 3
- Data_processed.csv
  - data about interpersonal distance and the number of objects between the dyad at the time of eye contact
  - used in Analysis 1 & Analysis 3
- Data_Analysis2.csv
  - data about usual interpersonal distance, which is not limited to gaze communication context.
  - used in Analysis 2

## Usage
1. Clone/download the repository.
2. Double-clicking on the project file `GazeCommunication2.Rproj`.
3. Run the `Analysis1/Analysis1.R` for Analysis1.
4. Run the `Analysis2/Analysis2.R` for Analysis2.
5. Run the `Analysis3/Analysis3.R` for Analysis3.

## Software & Package Versions
- RStudio:1.2.1335
- R:3.6.0
- tidyverse:1.2.1
- rstan:2.19.2
- ggmcmc:1.3
- knitr:1.23

## Author

[Hiroki Yamamoto](https://github.com/dororo1225)
