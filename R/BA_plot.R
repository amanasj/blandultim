BA_plot <- function(stats,
                    x,
                    y,
                    x_label = "Average of test 1 & test 2",
                    y_label = "Difference (test 1 - test 2)",
                    title=NULL,
                    plot_title_size=10,
                    pointcolour="black",
                    axis_xshift=0.1,
                    axis_yshift=2,
                    biaslabel_yshift=0.08,
                    biaslabel_xshift=0.8,
                    bias_type_size=6,
                    LoA_type_size=6,
                    CIlabel_yshift=0.15,
                    CIlabel_xshift=0.8,
                    CoRlabel_yshift=1.3,
                    CoRlabel_xshift=0.9,
                    CoR_units="",
                    CoR_type_size=6,
                    digits=1,
                    pointsize=3.5,
                    axis_text_size=10,
                    axis_title_size=16,
                    legend_title_size=10,
                    legend_text_size=10,
                    legend_size=1,
                    show_patient_ID_cols=TRUE,
                    xaxis_min=-0.1,
                    xaxis_max=NULL,
                    yaxis_min=NULL,
                    yaxis_max=NULL,
                    alpha=0.3)


{

  library(tidyverse)
  library(viridis)


  theme <- theme_bw()+theme(
    axis.text=element_text(size=axis_text_size),
    axis.title=element_text(size=axis_title_size, face="bold"),
    plot.title = element_text(size=plot_title_size, hjust=0.5,face="bold",
                              margin = margin(t = 0, b = 0)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(legend_size, 'cm'),
    legend.title = element_text(size=legend_title_size),
    legend.text = element_text(size=legend_text_size),
    panel.border = element_blank(),
    axis.line = element_line()
  )






  ###################### extract stats for plots ###########################
  data <- stats$df
  bias <- stats$summary$bias
  biasLowerCI <- stats$summary$bias_lowerCI
  biasUpperCI <- stats$summary$bias_upperCI
  lloa <- stats$summary$lower_LoA
  uloa <- stats$summary$upper_LoA
  upperLOA_upperCI <- stats$summary$upperLOA_upperCI
  upperLOA_lowerCI <- stats$summary$upperLOA_lowerCI
  lowerLOA_upperCI <- stats$summary$lowerLOA_upperCI
  lowerLOA_lowerCI <- stats$summary$lowerLOA_lowerCI
  CoR <- stats$summary$Coefficient_of_Repeatability
  COR_lowerCI <- stats$summary$CoR_CI_lower
  COR_upperCI <- stats$summary$CoR_CI_upper


  mean_max <- abs(stats$df$mean)
  mean_max <- max(mean_max)
  diff_max <- max(c(upperLOA_upperCI, lowerLOA_lowerCI))

  if(is.null(xaxis_max)==T){
    xaxis_max <- mean_max + (axis_xshift*mean_max)
  }else{xaxis_max=xaxis_max}
  if(is.null(yaxis_min)==T){
    yaxis_min <- -diff_max - (axis_yshift*diff_max)
  }else{yaxis_min=yaxis_min}
  if(is.null(yaxis_max)==T){
    yaxis_max <- diff_max + (axis_yshift*diff_max)
  }else{yaxis_max=yaxis_max}



  if("stimulus_ID" %in% colnames(data)){

    if(show_patient_ID_cols == TRUE){
      baplot <- ggplot(data, aes(mean, diff)) +
        geom_point(aes(colour=patient_ID), size=pointsize) +
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
    ggplot2::coord_cartesian(xlim=c(xaxis_min, xaxis_max), ylim=c(yaxis_min, yaxis_max), expand = F)

  baplot <- baplot+annotate("text", x=xaxis_min+(biaslabel_xshift*mean_max), y=bias+(biaslabel_yshift*diff_max),
                            label= paste0("bias = ", round(bias, digits = digits), " (95% CI: ", round(biasLowerCI, digits = digits), "," , round(biasUpperCI, digits = digits),")"),
                            size=bias_type_size, fontface = 2)
  baplot <- baplot+annotate("text", x=xaxis_min+(CIlabel_xshift*mean_max), y=upperLOA_upperCI+(CIlabel_yshift*diff_max),
                            label= paste0("ULoA = ", round(uloa, digits = digits), " (95% CI: ", round(upperLOA_lowerCI, digits = digits), "," , round(upperLOA_upperCI, digits = digits),")"),
                            size=LoA_type_size, fontface = 2)
  baplot <- baplot+annotate("text", x=xaxis_min+(CIlabel_xshift*mean_max), y=lowerLOA_lowerCI-(CIlabel_yshift*diff_max),
                            label= paste0("LLoA = ", round(lloa, digits = digits), " (95% CI: ", round(lowerLOA_lowerCI, digits = digits), "," , round(lowerLOA_upperCI, digits = digits),")"),
                            size=LoA_type_size, fontface = 2)
  baplot <- baplot+annotate("text", x=xaxis_min+(CoRlabel_xshift*mean_max), y=diff_max+(CoRlabel_yshift*diff_max),
                            label= paste0("CoR = ", round(CoR, digits = digits), CoR_units, " (95% CI: ", round(COR_lowerCI, digits = digits), "," , round(COR_upperCI, digits = digits),")"),
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
    annotate( "rect", xmin = -Inf , xmax = Inf , ymin = biasLowerCI , ymax = biasUpperCI, fill="darkgrey", alpha=alpha) + # Bias confidence interval shading
    annotate( "rect", xmin = -Inf , xmax = Inf , ymin = upperLOA_lowerCI, ymax = upperLOA_upperCI, fill="darkgrey" , alpha=alpha) + # Upper limits of agreement confidence interval shading
    annotate( "rect", xmin = -Inf , xmax = Inf , ymin = lowerLOA_lowerCI, ymax = lowerLOA_upperCI, fill="darkgrey" , alpha=alpha) # Lower limits of agreement confidence interval shading

  baplot



}
