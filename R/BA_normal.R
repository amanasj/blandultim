
#########################################################################################################################
#############   Function to calculate all Bland-Altman statistics including coefficient of repeatability   ##############          
#               ----------------------------------------------------------------------------------------                #
#  Code taken and modified from:                                                                                        #
#  https://rowannicholls.github.io/R/statistics/agreement/bland_altman_giavarina.html#bland-altman-analysis-1           #
#  And                                                                                                                  #
#  https://github.com/deepankardatta/blandr/blob/v.0.5.3-development/R/blandr.statistics.r                              #
#########################################################################################################################





############## test with randomly generated normal dist ####################
#set.seed(123)
#x <- rnorm(10, 10, 2)
#y <- rnorm(10, 10, 2)
#df <- cbind.data.frame(x,y)
#x <- df$x
#y <- df$y
############################################################################






############################### BA Stats ###############################

################# Stats function ################
#################################################

bland_altman_stats <- function(df, x, y, repeated, random, fixed, nboot=NULL) {
  
  
  if(repeated==FALSE){ 
    
    
  # Individual sample calculations
  df$mean <- (x + y) / 2
  df$diff <- y - x
  
  # Whole sample calculations
  summary <- data.frame()
  means <- c(paste("mean of x"), paste("mean of y"))
  summary[1, paste("mean of x")] <- mean(x)
  summary[1, paste("mean of y")] <- mean(y)

  # Sample size
  N <- nrow(df)
  summary[1, "N"] <- N
  # Degrees of freedom
  dof <- nrow(df) -1 
  summary[1, "DoF"] <- dof
  # Bias (mean difference)
  bias <- mean(df[["diff"]])
  summary[1, "bias"] <- bias
  # Confidence intervals (based on 2nd paper) Based on significance level supplied
  # (defaults to 95% CI) CI for mean
  biasSEM <- sd(df[["diff"]])/sqrt(N)
  # Convert confidence interval to a two-tailed z-value Don't really need this but kept as
  # a remnant of old version to possibly use in the future
  alpha <- 0.05
  sig.level.two.tailed <- 1 - (alpha/2)
  sig.level.convert.to.z <- qnorm(sig.level.two.tailed)
  biasCI <- qt(sig.level.two.tailed, df = dof) * biasSEM
  biasLowerCI <- bias - biasCI
  biasUpperCI <- bias + biasCI
  summary[1, "bias_lowerCI"] <- biasLowerCI
  summary[1, "bias_upperCI"] <- biasUpperCI
  # Population standard deviation of the differences
  #  st_dev_diff <- sd(df[["Diff"]]) * sqrt((nrow(df) - 1) / nrow(df))   ### not sure why population sd was used here, not normally done
  # Sample standard deviation of the differences
  st_dev_diff <- sd(df[["diff"]])
  summary[1, "SD_diffs"] <- st_dev_diff
  lowerLOA <- bias - 1.96 * st_dev_diff
  upperLOA <- bias + 1.96 * st_dev_diff
  summary[1, "lower_LoA"] <- lowerLOA
  summary[1, "upper_LoA"] <- upperLOA
  # CI for limits of agreement LOAVariance from Carkeet
  LOAVariance <- ((1/N) + ((sig.level.convert.to.z^2)/(2 * dof))) * st_dev_diff^2
  LOA_SEM <- sqrt(LOAVariance)
  LOA_CI <- qt(sig.level.two.tailed, df = dof) * LOA_SEM
  lowerLOA_lowerCI <- lowerLOA - LOA_CI
  lowerLOA_upperCI <- lowerLOA + LOA_CI
  upperLOA_lowerCI <- upperLOA - LOA_CI
  upperLOA_upperCI <- upperLOA + LOA_CI
  summary[1, "lowerLOA_lowerCI"] <- lowerLOA_lowerCI
  summary[1, "lowerLOA_upperCI"] <- lowerLOA_upperCI
  summary[1, "upperLOA_lowerCI"] <- upperLOA_lowerCI
  summary[1, "upperLOA_upperCI"] <- upperLOA_upperCI
  # Within-subject standard deviation
  xy <- cbind(x,y)
  df[['sample_SD']] <- apply(xy, 1, sd)
  df[['sample_variance']] <- df[['sample_SD']]**2
  s_w <- sqrt(mean(df[['sample_variance']]))
  summary[1, "within-subject_SD_(Sw)"] <- s_w
  # Coefficient of repeatability
  summary[1, "Coefficient_of_Repeatability"] <- sqrt(2) * 1.96 * s_w
  
  # Return
  return(list(df = df, summary = summary))
  
  
results <- bland_altman_stats(df,x,y)
df <- results[[1]]
summary <- results[[2]]
print(summary)
print(df)




  } else {
    
    
    tryCatch({
   
    # Individual sample calculations
    df$mean <- (x + y) / 2
    df$diff <- y - x
    
        library(nlme)
    resa<-lme(diff~as.factor(fixed),random=~1|random,
              correlation=corCompSymm(form=~1|random),data=df, na.action=na.omit)  
      
    # Whole sample calculations
    summary <- data.frame()
    means <- c(paste("mean of x"), paste("mean of y"))
    summary[1, paste("mean of x")] <- mean(x)
    summary[1, paste("mean of y")] <- mean(y)
    
    # Sample size
    N <- nrow(df)
    summary[1, "N"] <- N
    # Degrees of freedom
    dof <- nrow(df) -1 
    summary[1, "DoF"] <- dof
    # Bias (mean difference)
    bias <- mean(df[["diff"]])
    summary[1, "bias"] <- bias
    # Confidence intervals (based on 2nd paper) Based on significance level supplied
    # (defaults to 95% CI) CI for mean
    biasSEM <- sd(df[["diff"]])/sqrt(N)
    # Convert confidence interval to a two-tailed z-value Don't really need this but kept as
    # a remnant of old version to possibly use in the future
    alpha <- 0.05
    sig.level.two.tailed <- 1 - (alpha/2)
    sig.level.convert.to.z <- qnorm(sig.level.two.tailed)
    biasCI <- qt(sig.level.two.tailed, df = dof) * biasSEM
    biasLowerCI <- bias - biasCI
    biasUpperCI <- bias + biasCI
    summary[1, "bias_lowerCI"] <- biasLowerCI
    summary[1, "bias_upperCI"] <- biasUpperCI
    # Population standard deviation of the differences
    #  st_dev_diff <- sd(df[["Diff"]]) * sqrt((nrow(df) - 1) / nrow(df))   ### not sure why population sd was used here, not normally done
    # Sample standard deviation of the differences
    st_dev_diff <- sd(df[["diff"]])
    summary[1, "SD_diffs"] <- st_dev_diff
    lowerLOA <- bias - 1.96 * st_dev_diff
    upperLOA <- bias + 1.96 * st_dev_diff
    summary[1, "lower_LoA"] <- lowerLOA
    summary[1, "upper_LoA"] <- upperLOA
    # CI for limits of agreement LOAVariance from Carkeet
    LOAVariance <- ((1/N) + ((sig.level.convert.to.z^2)/(2 * dof))) * st_dev_diff^2
    LOA_SEM <- sqrt(LOAVariance)
    LOA_CI <- qt(sig.level.two.tailed, df = dof) * LOA_SEM
    lowerLOA_lowerCI <- lowerLOA - LOA_CI
    lowerLOA_upperCI <- lowerLOA + LOA_CI
    upperLOA_lowerCI <- upperLOA - LOA_CI
    upperLOA_upperCI <- upperLOA + LOA_CI
    summary[1, "lowerLOA_lowerCI"] <- lowerLOA_lowerCI
    summary[1, "lowerLOA_upperCI"] <- lowerLOA_upperCI
    summary[1, "upperLOA_lowerCI"] <- upperLOA_lowerCI
    summary[1, "upperLOA_upperCI"] <- upperLOA_upperCI
    # Within-subject standard deviation
    s_w <- as.numeric(VarCorr(resa)[2,2])
    # Coefficient of repeatability
    summary[1, "Coefficient_of_Repeatability"] <- sqrt(2) * 1.96 * s_w
    
    # Return
    return(list(df = df, summary = summary))
    
    
    results <- bland_altman_stats(df,x,y)
    df <- results[[1]]
    summary <- results[[2]]
    print(summary)
    print(df)
    
    }, error=function(e){cat("\n \n ERROR : Must SPECIFY: RANDOM' AND 'FIXED'")})
    
  }

}


