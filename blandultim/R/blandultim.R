#####################################################################################
###    function to perform a normal BA or a repeated measures BA if a fixed       ###
###                 and random variables are supplied                             ###
#####################################################################################


  

blandultim <- function(data, x, y, nboot=NULL, fixed=NULL, random=NULL, bootstrap=NULL, repeated=NULL){

      
  
  if(bootstrap==TRUE){ 
    
      
      cat("\n \n WARNING:- I have only written the bootstrapping code to deal with repeated measures so 'repeated=TRUE' 
          is always ON by default and random and fixed variables MUST ALWAYS be specified when bootstrapping \n \n")
    
  
    
    tryCatch({
      
      
  #### Run bootstrap script for non-parametric or repeated measures BA
  source("C:/Users/ajosan/OneDrive - Nexus365/Desktop/R_scripts/Bland-Altman_Ultimate/Rscripts/MP/BA_bootstrap.R")
  stats <- parabootstraptLOA(nboot=nboot, data=data, x=x, y=y, seed=859, fixed=fixed, random=random)
  
  stats
  
    }, error=function(e){cat("\n \n ERROR : Must SPECIFY: 'NBOOT', 'RANDOM' AND 'FIXED'")})
    
    

  
  
} else {
  
  
  ### OR  run normal parametric non-repeated or repeated measures BA script
  source("C:/Users/ajosan/OneDrive - Nexus365/Desktop/R_scripts/Bland-Altman_Ultimate/Rscripts/MP/BA_normal.R")
  stats <- bland_altman_stats(df=data, x=x, y=y, random=random, fixed=fixed, repeated=repeated)
  
  stats
  
  }

}



