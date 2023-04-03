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
                    CIlabel_yshift=2, 
                    CIlabel_xshift=-2,
                    CoRlabel_yshift=8, 
                    CoRlabel_xshift=-2,
                    digits=1,
                    pointsize=3.5,
                    axis_text_size=16,
                    axis_title_size=24,
                    plot_title_size=26,
                    alpha=0.3,
                    bias_type_size = 5,
                    LoA_type_size = 6,
                    CoR_type_size = 6,
                    legend_size=0.8,
                    legend_title_size=14,
                    legend_text_size=10,
                    show_patient_ID_cols=TRUE)

{
  
  library(tidyverse)
  library(viridis)
  
  
  theme <- theme_bw()+theme(
    axis.text=element_text(size=axis_text_size), 
    axis.title=element_text(size=axis_title_size,face="bold"),
    plot.title = element_text(size=plot_title_size, hjust=0.5,face="bold",
                              margin = margin(t = 0, b = 0)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(legend_size, 'cm'),
    legend.title = element_text(size=legend_title_size),
    legend.text = element_text(size=legend_text_size)
    #panel.border = element_blank()
    #axis.line = element_line()
  ) 
  
  
  
  
  
  
  ###################### extract stats for plots ###########################
  data <- stats$df
  bias <- round(stats$summary$bias, digits = digits)
  biasLowerCI <- round(stats$summary$bias_lowerCI, digits = digits)
  biasUpperCI <- round(stats$summary$bias_upperCI, digits = digits)
  lloa <- round(stats$summary$lower_LoA, digits = digits)
  uloa <- round(stats$summary$upper_LoA, digits = digits)
  upperLOA_upperCI <- round(stats$summary$upperLOA_upperCI, digits = digits)
  upperLOA_lowerCI <- round(stats$summary$upperLOA_lowerCI, digits = digits)
  lowerLOA_upperCI <- round(stats$summary$lowerLOA_upperCI, digits = digits)
  lowerLOA_lowerCI <- round(stats$summary$lowerLOA_lowerCI, digits = digits)
  CoR <- round(stats$summary$Coefficient_of_Repeatability, digits = digits)
  xmax <- max(x)
  
  
  
  
  
  
  if("stimulus_ID" %in% colnames(data)){

         if(show_patient_ID_cols == TRUE){
            baplot <- ggplot(data, aes(mean, diff)) +
            geom_point(aes(colour=patient_ID), size=pointsize) 
            scale_color_viridis(discrete = T, option = "turbo") +
            labs(x = x_label, y = y_label, title = title, color = "Patient ID") +
            theme
    
    }else{
            baplot <- ggplot(data, aes(mean, diff)) +
            geom_point(colour=pointcolour, size=pointsize) +
            scale_color_viridis(discrete = T, option = "turbo") +
            labs(x = x_label, y = y_label, title = title, color = "Patient ID") +
            theme
    }
    
    
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
                            label= paste0("CoR = ", CoR,"dB"), 
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
    annotate( "rect", xmin = -Inf , xmax = Inf , ymin = biasLowerCI , ymax = biasUpperCI , fill="darkgrey" , alpha=alpha ) + # Bias confidence interval shading
    annotate( "rect", xmin = -Inf , xmax = Inf , ymin = upperLOA_lowerCI , ymax = upperLOA_upperCI , fill="darkgrey" , alpha=alpha ) + # Upper limits of agreement confidence interval shading
    annotate( "rect", xmin = -Inf , xmax = Inf , ymin = lowerLOA_lowerCI , ymax = lowerLOA_upperCI , fill="darkgrey" , alpha=alpha ) # Lower limits of agreement confidence interval shading
  
  baplot
  
  
  
}







