# DivChangeResponse
Analyses for *Vellend et al. Estimates of long-term local biodiversity change stand up to scrutiny, Ecology, in press* (available in [preprint version](http://onlinelibrary.wiley.com/doi/10.1002/ecy.1660/full)). More information on the data can be found in the main text; the models are explained in the Supplementary Material (Appendix S1). In brief, we analysed the effect of duration on diversity changes, using the original and updated data from  Vellend et al. 2013 PNAS and the data from Dornelas et al. 2014 Science. All code and data are also available in the Supplementary Material of the paper (Data S1, Metadata S1).

*version 30 November 2016*

## Data
- *Vellend_data_original.csv*: this is the complete dataset that was used in the Vellend et al. 2013 PNAS paper. You can also download it from the Supplementary Material of that paper (sd01.csv)
- *Vellend_data_updated.csv*: the original dataset was updated to contain all relevant papers published through the end of 2014 (the original paper had studies published up to July 2012). The same literature search was done as described in the original paper
- *Dornelas_by_study.csv*: dataset that was used in the Dornelas et al. 2014 Science paper. You can also download it from the Supplementary Material of that paper (Dataset S1). The subset of modified data is an exact copy of the original data, but with just one changed value for the log ratio and duration (study 147 without the outlier); see main text of the paper for more information

## R

### Stan models
- *model_duration_rintslope.stan*: Stan model with duration as dataset-level predictor and intercepts and slopes varying by study. This corresponds to ```y ~ duration + (1 + duration | study)``` in lmer syntax.
- *model_duration_rslope.stan*: Stan model with duration as dataset-level predictor, no intercept, and slopes varying by study. This corresponds to ```y ~ -1 + duration + (-1 + duration | study)``` in lmer syntax.
- *model_duration_intslope.stan*: Stan model with duration as predictor, with intercept and slope. This corresponds to ```y ~ duration``` in lm syntax.
- *model_duration_slope.stan*: Stan model with duration as predictor, without an intercept and wih slope. This corresponds to ```y ~ -1 + duration``` in lm syntax.
- *model_rint.stan*: Stan model with intercepts varying by study, as in the original Vellend et al. 2013 analyses. This corresponds to ```y ~ 1 + (1 | study)``` in lmer syntax.

### Analyses
- *analysis_vel.R*: reanalysis of the Vellend et al. original and updated dataset. This script reproduces **Fig. 2** from Vellend et al. Estimates of long-term local biodiversity change stand up to scrutiny
- *analysis_dor.R*: reanalysis of the Dornelas et al. dataset (with log ratio's and slopes as effect sizes). This script reproduces **Fig. 3** from Vellend et al. Estimates of long-term local biodiversity change stand up to scrutiny
- *analysis_vel_impact.R*: Gonzalez et al. eliminated studies classified as “post-disturbance”, “post-fire”, and “cessation of grazing” to analyse a subset of 'impact only' data. This script runs our original (Vellend et al. 2013) analysis on this subset, both for the original and updated dataset.

### Extra scripts
- *helperfunctions.R*: some helperfunctions used in the different scripts (e.g. restructuring the data for Stan models)
- *dataprep.R*: prepare the Vellend and Dornelas data for analyses
