# DivChangeResponse
Analysis of the updated data from Vellend et al 2013 PNAS.

*version 30 april 2016*
NEEDS UPDATE

This repo contains two folders, one with the data and one with R code

## Data
- *Vellend_data_original.csv*: these are the data that were used in the Vellend et al. 2013 PNAS paper. You can also download them from the Supplementary Material of that paper.
- *Vellend_data_updated.csv*: the original data were updated to contain all relevant papers published until the end of 2014. The same literature search was done as described in the original paper.

## R
- *model_duration.stan*: Stan model with duration as predictor dataset-level predictor and intercepts varying by study. This corresponds to ```y ~ duration + (1 | study)```
- *model_duration_randslope.stan*: Stan model with duration as dataset-level predictor and intercepts and slopes varying by study. This corresponds to ```y ~ duration + (1 + duration | study)```

- *analysis_gonz.R*: mixed-effects model analysis that was used in the Gonzalez et al. (in press) Ecology paper
- *analysis_vel.R*: analysis of the reponse paper, based no the model_duration_randslope.stan model
- *dataprep.R*: read, filter, mutate... prepare for the analyses
- *helperfuncions.R*: some helperfunctions
