
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

bland_altman_stats <- function(df, x, y, 
                               repeated, 
                               random, 
                               fixed, 
                               nboot,
                               alpha=0.05){
  
  
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
  CoR <- sqrt(2) * 1.96 * s_w
  summary[1, "Coefficient_of_Repeatability"] <- CoR
  
  
  ## CI for CoR
  # calculate lower and upper critical values c_l and c_u
  c_l <- sqrt((N - 1)/qchisq(alpha/2, N-1, lower.tail = FALSE))
  c_u <- sqrt((N - 1)/qchisq(alpha/2, N-1, lower.tail = TRUE))
  
  # calculate lower and upper confidence interval for sd
  s_w_CIlower <- s_w * c_l
  s_w_CIupper <- s_w * c_u 
  CoR_CI_lower <- sqrt(2) * 1.96 * s_w_CIlower
  CoR_CI_upper <- sqrt(2) * 1.96 * s_w_CIupper
  
  summary[1, "CoR_CI_lower"] <- CoR_CI_lower
  summary[1, "CoR_CI_upper"] <- CoR_CI_upper
  
  
  
  
  ### check that diff bw T1 and T2 is not significant 
  ### (i.e. LOA cross zero or p-value>0.05 for 2-tailed test)
  ### if not sig then (and only then) can report a CoR value
  ### otherwise means test is not repeatable so can't report CoR
  SE_CI <- (upperLOA - lowerLOA)/(2*1.96)
  z_score <- bias/SE_CI
  p_value_for_diff <- 2*pnorm(-abs(z_score))
  summary[1, "p_value for difference"] <- p_value_for_diff
  
  if (p_value_for_diff < 0.05){cat(
    "
  WARNING: -----------------------------------------------------------
  Difference between test1 and test2 is significant (p_value for diff < 0.05).
  This means test is not repeatable and hence should not report CoR.
  ---------------------------------------------------------------------"
    
  )}
  
  

  
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
    N <- length(unique(random))
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
    s_w <- as.numeric(nlme::VarCorr(resa)[2,2])
    # Coefficient of repeatability
    CoR <- sqrt(2) * 1.96 * s_w
    summary[1, "Coefficient_of_Repeatability"] <- CoR
    
    
    
    
    ## CI for CoR
    # calculate lower and upper critical values c_l and c_u
    c_l <- sqrt((N - 1)/qchisq(alpha/2, N-1, lower.tail = FALSE))
    c_u <- sqrt((N - 1)/qchisq(alpha/2, N-1, lower.tail = TRUE))
    
    # calculate lower and upper confidence interval for sd
    s_w_CIlower <- s_w * c_l
    s_w_CIupper <- s_w * c_u 
    CoR_CI_lower <- sqrt(2) * 1.96 * s_w_CIlower
    CoR_CI_upper <- sqrt(2) * 1.96 * s_w_CIupper
    
    summary[1, "CoR_CI_lower"] <- CoR_CI_lower
    summary[1, "CoR_CI_upper"] <- CoR_CI_upper
    
    
    
    
    
    ### check that diff bw T1 and T2 is not significant 
    ### (i.e. LOA cross zero or p-value>0.05 for 2-tailed test)
    ### if not sig then (and only then) can report a CoR value
    ### otherwise means test is not repeatable so can't report CoR
    SE_CI <- (upperLOA - lowerLOA)/(2*1.96)
    z_score <- bias/SE_CI
    p_value_for_diff <- 2*pnorm(-abs(z_score))
    summary[1, "p_value for difference"] <- p_value_for_diff
    
    if (p_value_for_diff < 0.05){cat(
      "
  WARNING: -----------------------------------------------------------
  Difference between test1 and test2 is significant (p_value for diff < 0.05).
  This means test is not repeatable and hence should not report CoR.
  ---------------------------------------------------------------------"
      
    )}
    
    
    
    
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




