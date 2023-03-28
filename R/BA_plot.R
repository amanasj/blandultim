
BA_plot <- function(stats,
                    x,
                    y,
                    x_label = "Average of test 2 & test 3",
                    y_label = "Difference (test 2 - test 3)",
                    title=NULL,
                    pointcolour="black", 
                    axis_xshift=5, 
                    axis_yshift=15, 
                    biaslabel_yshift=1, 
                    biaslabel_xshift=-2,
                    CIlabel_yshift=3, 
                    CIlabel_xshift=-2,
                    CoRlabel_yshift=8, 
                    CoRlabel_xshift=-2,
                    digits=3,
                    pointsize=3.5,
                    axis_text_size=16,
                    axis_title_size=24,
                    plot_title_size=26,
                    alpha=0.3,
                    bias_type_size = 6,
                    LoA_type_size = 6,
                    CoR_type_size = 6,
                    legend_size=0.9)
{

library(tidyverse)
library(RColorBrewer)  
  
  
theme <- theme_bw()+theme(
  axis.text=element_text(size=axis_text_size), 
  axis.title=element_text(size=axis_title_size,face="bold"),
  plot.title = element_text(size=plot_title_size, hjust=0.5,face="bold",
                            margin = margin(t = 0, b = 0)),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.key.size = unit(legend_size, 'cm')
) 






###################### extract stats for plots ###########################
data <- stats$df
bias <- signif(stats$summary$bias, digits = digits)
biasLowerCI <- signif(stats$summary$bias_lowerCI, digits = digits)
biasUpperCI <- signif(stats$summary$bias_upperCI, digits = digits)
lloa <- signif(stats$summary$lower_LoA, digits = digits)
uloa <- signif(stats$summary$upper_LoA, digits = digits)
upperLOA_upperCI <- signif(stats$summary$upperLOA_upperCI, digits = digits)
upperLOA_lowerCI <- signif(stats$summary$upperLOA_lowerCI, digits = digits)
lowerLOA_upperCI <- signif(stats$summary$lowerLOA_upperCI, digits = digits)
lowerLOA_lowerCI <- signif(stats$summary$lowerLOA_lowerCI, digits = digits)
CoR <- signif(stats$summary$Coefficient_of_Repeatability, digits = digits)
xmax <- max(x)






if("stimulus_ID" %in% colnames(data)){
  baplot <- ggplot(data, aes(mean, diff)) +
    geom_point(aes(colour=patient_ID), size=pointsize) +
    scale_color_brewer(palette = "Paired") +
    labs(x = x_label, 
         y = y_label, 
         title = title,
         color = "Patient ID") +
    theme
  
} else {
  baplot <- ggplot(data, aes(mean, diff)) +
    geom_point(colour=pointcolour, size=pointsize) +
    labs(x = x_label, 
         y = y_label, 
         title = title) +
    theme
}
  
  
  

####change axis limits
baplot <- baplot + 
  ggplot2::coord_cartesian(xlim=c(-0.1, xmax + axis_xshift), ylim=c(lowerLOA_lowerCI - axis_yshift, upperLOA_upperCI + axis_yshift), expand = F)

baplot <- baplot+annotate("text", x=xmax+(biaslabel_xshift), y=biasUpperCI+(biaslabel_yshift), 
                          label= paste0("bias = ", bias, " (95% CI: ", biasLowerCI, "," , biasUpperCI,")"), 
                          size=bias_type_size, fontface = 2)
baplot <- baplot+annotate("text", x=xmax+(CIlabel_xshift), y=upperLOA_upperCI+(CIlabel_yshift), 
                          label= paste0("ULoA = ", uloa, " (95% CI: ", upperLOA_lowerCI, "," , upperLOA_upperCI,")"), 
                          size=LoA_type_size, fontface = 2)
baplot <- baplot+annotate("text", x=xmax+(CIlabel_xshift), y=lowerLOA_lowerCI-(CIlabel_yshift), 
                          label= paste0("LLoA = ", lloa, " (95% CI: ", lowerLOA_lowerCI, "," , lowerLOA_upperCI,")"), 
                          size=LoA_type_size, fontface = 2) 
baplot <- baplot+annotate("text", x=xmax+(CoRlabel_xshift), y=upperLOA_upperCI+(CoRlabel_yshift), 
                          label= paste0("CoR = ", CoR), 
                          size=CoR_type_size, fontface = 2) 
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
  annotate( "rect", xmin = -Inf , xmax = Inf , ymin = biasLowerCI , ymax = biasUpperCI , fill="grey" , alpha=alpha ) + # Bias confidence interval shading
  annotate( "rect", xmin = -Inf , xmax = Inf , ymin = upperLOA_lowerCI , ymax = upperLOA_upperCI , fill="lightgrey" , alpha=alpha ) + # Upper limits of agreement confidence interval shading
  annotate( "rect", xmin = -Inf , xmax = Inf , ymin = lowerLOA_lowerCI , ymax = lowerLOA_upperCI , fill="lightgrey" , alpha=alpha ) # Lower limits of agreement confidence interval shading

baplot



}








