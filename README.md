# blandultim package

blandultim package: 

<br>

computes Bland-Altman statistics and produces B-A plots based on several possible conditions

<br>

1) blandultim(data, x, y, bootstrap=F) - 
   Normally distributed data without repeated measures (i.e. each point in the B-A plot is 
   independent - only one pair of measurements per subject such as mean sensitivity for test 1 
   vs mean sensitivity for test2)

<br>

2) blandultim(data, x, y, bootstrap=F, repeated=T, fixed, random) - 
   Normally distributed data with repeated measures (i.e. multiple dependent points per patient on the 
   B-A plot such as in pointwise test 1 vs pointwise test2. Here must specify fixed and random effects 
   for the mixed linear modelling (e.g. random=patient_ID, fixed=stimulus_ID)

<br>

3) blandultim(data, x, y, bootstrap=T, random, fixed, nboot) - 
   Non-normally distributed data benefits from a bootstrapping approach to estimate the 
   possible distribution of population means and hence 95% CI's in order to calculated CI's 
   for the bias and LoA. Must specify random, fixed and nboot (resampling rate for 
   bootstrapping ~ 1000 is usually adequate)
   
   <br>
   
   CoR is calculated from within patient stdev so doesn't use the bootstrapping aspect but 
   rather uses either the simply calculated within patient stdev in the non-repeated measures 
   option or calculateds the within stdev using the random intercept per patient_ID in the repeated 
   measures option. 

<br>

      NOTE: repeated measures is on by default for bootstrapping method as I haven't yet gotten around to 
         writing a bootstrapping code for non-repeated measures 
         
<br>
         
 4) Finally produce B-A plot using function:   BA_plot(data, x, y, title)


<br><br>


To install R package:  
library(devtools)  
install_github('amanasj/blandultim', force=TRUE)  
library(blandultim)  
blandultim(data, x, y, ...)
