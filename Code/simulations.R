### Simulations ################################################################

### Libraries ##################################################################

  library(tidyverse)
  library(hrbrthemes)
  library(viridis)
  library(patchwork)


### Coefficients + setup #######################################################

  # Coefficients of linear probability model
  intercept <- 0.07
  mage <- 0.0005
  fage <- 0.00075
  
  # Sample size and replications for each sample size
  sizes <- seq(250,100000,by=250)
  replications <- 500
  
  # Objects for results (average p-values)
  result1 <- numeric(length(sizes))
  result2 <- numeric(length(sizes))
  
  # Object for results (proportion of times higher model is significant while other is not)
  result3 <- numeric(length(sizes))
  
  
### Run simulations ############################################################  
  
  # Loop over sample size
  for(size in sizes) {
    
    # A little output 
    cat(".")

    # For intermediate results   
    tmp1 <- numeric(replications)
    tmp2 <- numeric(replications)
    
      # Loop over replications for each sample size
      for(rep in 1:replications) {
  
        # Generate maternal and paternal age
        agem <- sample(20:45,size=size,rep=T)
        agef <- sample(20:45,size=size,rep=T)
        dat <- data.frame(agem=agem,
                          agef=agef)
        
        # Generate outcome based on underlying probability
        dat$predicted <- intercept + dat$agem*mage + dat$agef*fage + abs(rnorm(sd=0.01,n=size))
        dat$prob <- runif(n=size)
        dat$outcome <- as.numeric(dat$predicted>=dat$prob)
        
        # Calculate age difference
        dat$diff <- dat$agem - dat$agef
        
        # Calculate two models (M+P and M+D)
        fit1 <- glm(outcome~agem+agef,data=dat,family = binomial(link = "logit"))
        fit2 <- glm(outcome~agem+diff,data=dat,family = binomial(link = "logit"))
    
        # Save temporary result (p-value)
        tmp1[rep] <- (summary(fit1))$coefficients["agem",4]
        tmp2[rep] <- (summary(fit2))$coefficients["agem",4]
        
    }
    
    # Average over results
    result1[which(sizes==size)] <- mean(tmp1)
    result2[which(sizes==size)] <- mean(tmp2)
    result3[which(sizes==size)] <- sum(tmp2<0.05 & tmp1>=0.05)/replications

  }

  
### Save results ###############################################################  

  # Combine results for plotting (p-values)
  result <- data.frame(samplesize=c(sizes,sizes),
                       pvalue=c(result1,result2),
                       Model=c(rep("M-P",length(result1)),rep("M-D",length(result1))))

  # Save
  save(list=c("result","result3","sizes"),
       file="Results/simulations.rda")
  
    
### Plotting ###################################################################  

  # Plot
  fig1 <- result |> ggplot(aes(x=samplesize,y=pvalue,col=Model)) +
                    geom_line(data=data.frame(x=seq(250,100000,by=250),
                                              y=rep(0.05,length(seq(250,100000,by=250)))),
                              aes(x,y,col=NULL),col="grey50",linetype=2)+
                    geom_line() +
                    labs(x="Sample size",y="Average p-value")+
                    theme_ipsum() +
                    theme(panel.grid.minor = element_blank())
    
    
  # Combine results for plotting (proportion)
  plotresult <- data.frame(samplesize=sizes,
                       proportion=result3)
  
  # Plot
  fig2 <- plotresult |> ggplot(aes(x=samplesize,y=proportion)) +
    geom_line() +labs(x="Sample size",y="Proportion p1<0.05 & p2>=0.05")+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())

  # Combine and save
  fig_combined <- fig1 / fig2 
  ggsave(plot=fig_combined,file="Results/fig_sim.png",
         width=12,
         height=7)
