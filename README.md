# size-at-age

This repo houses data and code for the following paper in _Global Ecology and Biogeography_

Bigman, J. S., Barnett, L. A. K., Thorson, J. T., Anderson, S. C., Oke, K. B., Kearney, K. A., Pilcher, D. J., Cheng, W., Goldstein, E. D., Matta, M. E., Holsman, K. K., & Rogers, L. A. (2026). Changes in body size with age do not follow the temperature-size rule. Global Ecology and Biogeography, 35(4), e70238. https://doi.org/10.1111/geb.70238

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17038970.svg)](https://doi.org/10.5281/zenodo.17038970)


The data/ folder contains all data to fit the models in the paper: (1) size-at-age data for the four groundfish species and (2) environmental data from the Bering10K ROMS model (see [here](https://beringnpz.github.io/roms-bering-sea/B10K-dataset-docs/) for more detail about the model). 

The output/ folder contains a subfolder called plots for paper/ that provides all figures in the paper and supplementary material (as well as extras!). 

The scripts/ folder contains (1) all code to re-create the analyses within the paper (see below) and a subfolder called manuscript figure code/ that contains all code to re-create all figures in the paper and supplementary materials. Note you can start with 01-load each time, which loads the processed data (processed in steps 00a through 00g). 

Code in scripts/ folder:

Data-processing code:
00a - load GAP data and fix missing stations.R: assigns stations to haul locations where station is missing, if haul locations match other station information<br>
00b - wrangle and map data from GAP May 2024.R: map sample distribution<br>
00c - remove outliers.R: follow NOAA AFSC protocol to remove outliers of length and weight (see paper for more detail)<br>
00d - download and concatenate Bering10K output.R: collates all hindcasted temperature and oxygen output from the Bering10K model from the THREDDS server<br> 
00e - data wrangling and nearest neighbor matching.R: matches each climate model grid cell to haul location<br>
00f - download and concatenate Bering10k ROMS projections.R: collates all forecasted temperature and oxygen output from the Bering10K model from the THREDDS server<br>
00g - exploratory plots.R: make preliminary, exploratory plots<br>

Model code:
01 - load each time.R: loads all packages, data needed to fit models<br> 
02 - year models.R: fits models to explore change in size-at-age by year for all age classes of all species<br>
03a - fully MV model arrowtooth.R: fits age-specific spatiotemporal GLMMs (see paper for more detail) to explore how temperature and oxygen predict changes in size-at-age for arrowtooth flounder<br>
03b - fully MV model pcod.R: same as above but for Pacific cod<br>
03c - fully MV model pollock.R: same as above but for walleye pollock<br> 
03d - fully MV model yellowfin.R: same as above but for yellowfin sole<br>
04 - all shared mods.R: fits spatiotemporal GLMMs (see paper for more detail) to explore how temperature and oxygen predict changes in size-at-age with shared spatial and spatiotemporal fields across ages<br> 
05 - shared spatial field only.R: fits spatial GLMMs (see paper for more detail) to explore how temperature and oxygen predict changes in size-at-age with shared spatial fields across ages<br> 
06a - REML model comparisons.R: compares models fitted with REML<br>
06b - top model ML comparison.R: compares most supported models for each species fitted with ML<br> 
06c - all models ML comparison.R: compares all models fitted with ML<br>
07 - year model prediction.R: predictions for year-only models (i.e., those fitted in 02 - year models)<br> 
08 - predictions of temp for top models.R: predictions for top models (i.e., those fitted in 06b - top model ML comparison)<br>  

