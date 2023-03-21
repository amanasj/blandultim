# blandultim

blandultim package: 

computes B-A statistics and produces B-A plots based on several possible conditions



1) blandultim(data, x, y, bootstrap=F) - 
   Normally distributed data without repeated measures (i.e. each point in the B-A plot is 
   independent - only one pair of measurements per subject such as mean sensitivity for test 1 
   vs mean sensitivity for test2)


2) blandultim(data, x, y, bootstrap=F, repeated=T, fixed, random) - 
   Normally distributed data with repeated measures (i.e. multipke dependent points per patient on the 
   B-A plot such as in pointwise test 1 vs pointwise test2. Here must specify fixed and random effects 
   for the mixed linear modelling (e.g. random=patient_ID, fixed=stimulus_ID)


3) blandultim(data, x, y, bootstrap=T, random, fixed, nboot) - 
   Non-normally distributed data benefits from a bootstrapping approach to estimate the 
   possible distribution of population means and hence 95% CI's in order to calculated CI's 
   for the bias and LoA. Must specify random, fixed and nboot (resampling rate for 
   bootstrapping ~ 1000 is adequate)
   
   CoR is calculated from within patient stdev so doesn't use the bootstrapping aspect but 
   rather uses either the simply calculated within patient stdev in the non-repeated measures 
   option or calculateds the within stdev using the random intercept per patient_ID in the repeated 
   measures option. 

      NOTE: repeated measures is on by default for bootstrapping method as I haven't gotten around to 
         writting a bootstrapping code for non-repeated measures (simple to do though) 
         
         
         
 4) Finally produce B-A plot using function:   BA_plot(data, x, y, title)


To install R package:  
library(devtools)  
install_github('amanasj/blandultim')  
library(blandultim)  
blandultim(data, x, y, ...)
