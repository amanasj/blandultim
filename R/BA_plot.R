library(tidyverse)
library(here)
library(patchwork)




BA_plot <- function(stats,x,y,title=NULL,pointcolour="black"){


  
  
theme <- theme_bw()+theme(
  axis.text=element_text(size=20), 
  axis.title=element_text(size=20,face="bold"),
  plot.title = element_text(size=20, hjust=0.5,face="bold",
                            margin = margin(t = 0, b = 0)),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())





###################### extract stats for plots ###########################
data <- stats$df
bias <- signif(stats$summary$bias, digits = 3)
biasLowerCI <- signif(stats$summary$bias_lowerCI, digits = 3)
biasUpperCI <- signif(stats$summary$bias_upperCI, digits = 3)
lloa <- signif(stats$summary$lower_LoA, digits = 3)
uloa <- signif(stats$summary$upper_LoA, digits = 3)
upperLOA_upperCI <- signif(stats$summary$upperLOA_upperCI, digits = 3)
upperLOA_lowerCI <- signif(stats$summary$upperLOA_lowerCI, digits = 3)
lowerLOA_upperCI <- signif(stats$summary$lowerLOA_upperCI, digits = 3)
lowerLOA_lowerCI <- signif(stats$summary$lowerLOA_lowerCI, digits = 3)
CoR <- signif(stats$summary$Coefficient_of_Repeatability, digits = 3)
xmax <- max(x)






if("stimulus_ID" %in% colnames(data)){
  baplot <- ggplot(data, aes(mean, diff)) +
    geom_point(aes(colour=patient_ID), size=3)+
    labs(x = "Average of test 2 & test 3", 
         y = "Difference (test 2 - test 3)", 
         title = title,
         color = "Patient ID")+
    theme
  
} else {
  baplot <- ggplot(data, aes(mean, diff)) +
    geom_point(colour=pointcolour, size=3)+
    labs(x = "Average of test 2 & test 3", 
         y = "Difference (test 2 - test 3)", 
         title = title)+
    theme
}
  
  
  

####change axis limits
baplot <- baplot + 
  ggplot2::coord_cartesian(xlim=c(-0.1,xmax+5), ylim=c(lowerLOA_lowerCI-15,upperLOA_upperCI+15), expand = F)

baplot <- baplot+annotate("text", x=xmax, y=biasUpperCI+1, 
                          label= paste0("bias = ", bias, " (95% CI: ", biasLowerCI, "," , biasUpperCI,")"), 
                          size=4, fontface = 2)
baplot <- baplot+annotate("text", x=xmax, y=upperLOA_upperCI+3, 
                          label= paste0("ULoA = ", uloa, " (95% CI: ", upperLOA_lowerCI, "," , upperLOA_upperCI,")"), 
                          size=6, fontface = 2)
baplot <- baplot+annotate("text", x=xmax, y=lowerLOA_lowerCI-3, 
                          label= paste0("LLoA = ", lloa, " (95% CI: ", lowerLOA_lowerCI, "," , lowerLOA_upperCI,")"), 
                          size=6, fontface = 2) 
baplot <- baplot+annotate("text", x=xmax, y=upperLOA_upperCI+8, 
                          label= paste0("CoR = ", CoR), 
                          size=6, fontface = 2) 
baplot <- baplot +
  geom_hline(yintercept = bias , linetype = 2) + # Bias
  geom_hline(yintercept = biasUpperCI, linetype = 2) + # Bias - upper confidence interval
  geom_hline(yintercept = biasLowerCI, linetype = 2) + # Bias - lower confidence interval
  geom_hline(yintercept = uloa, linetype = 2 ) + # ULOA 
  geom_hline(yintercept = lloa, linetype = 2 ) + # LLOA 
  geom_hline(yintercept = upperLOA_upperCI, linetype = 3 ) + # Upper limit of agreement - upper confidence interval
  geom_hline(yintercept = upperLOA_lowerCI, linetype = 3 ) + # Upper limit of agreement - lower confidence interval
  geom_hline(yintercept = lowerLOA_upperCI, linetype = 3 ) + # Lower limit of agreement - upper confidence interval
  geom_hline(yintercept = lowerLOA_lowerCI, linetype = 3 ) + # Lower limit of agreement - lower confidence interval
  annotate( "rect", xmin = -Inf , xmax = Inf , ymin = biasLowerCI , ymax = biasUpperCI , fill="blue" , alpha=0.3 ) + # Bias confidence interval shading
  annotate( "rect", xmin = -Inf , xmax = Inf , ymin = upperLOA_lowerCI , ymax = upperLOA_upperCI , fill="green" , alpha=0.3 ) + # Upper limits of agreement confidence interval shading
  annotate( "rect", xmin = -Inf , xmax = Inf , ymin = lowerLOA_lowerCI , ymax = lowerLOA_upperCI , fill="red" , alpha=0.3 ) # Lower limits of agreement confidence interval shading

baplot



}








