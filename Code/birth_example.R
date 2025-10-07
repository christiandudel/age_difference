### Birth register data ########################################################

### Loading packages ###########################################################

  library(tidyverse)
  library(data.table)
  library(httr)
  library(hrbrthemes)
  library(viridis)
  library(patchwork)


### Function for scale #########################################################

  # Source: https://stackoverflow.com/questions/48424682/how-do-i-limit-the-range-of-the-viridis-colour-scale
  rescaler <- function(x, to = c(0, 1), from = NULL) {
      ifelse(x<0.15, 
             scales::rescale(x,
                             to = to,
                             from = c(min(x, na.rm = TRUE), 0.15)),
             1)}

  
### Loading data ###############################################################

  # Data available from:
  # https://www.nber.org/data/vital-statistics-natality-data.html
  
  # Data for 2023
  url <- "https://data.nber.org/nvss/natality/csv/2023/natality2023us.csv"
  # Documentation:
  # https://data.nber.org/nvss/natality/inputs/pdf/2023/UserGuide2023.pdf
  
  # Download, the file is around 2GB, might take some time
  file1 <- "Data/nat2023us.csv"
  if(!file.exists(file1)) {
    GET(url, write_disk(file1, overwrite = TRUE), progress() )
  }
  
  # Load & save downloaded file
  file2 <- "Data/usregister.Rdata"
  if(!file.exists(file2)) {
    dat <- fread(file1,select=c("restatus",
                               "mager","fagecomb",
                               "dbwt"))
    save(dat,file=file2)
  }
  
  # Load
  load(file2)


### Editing data ###############################################################
  
  # Drop if not resident
  dat <- dat |> filter(restatus!=4)
  
  # Drop missing age of father
  dat <- dat |> filter(fagecomb!=99)
  
  # Drop missing birth weight
  dat <- dat |> filter(dbwt!=9999)

  # Select & rename variables
  dat <- dat |> 
         select(mager,fagecomb,dbwt) |> 
         rename(mage=mager, # mother's age
                fage=fagecomb, # father's age
                weight=dbwt) # birth weight in grams
  

### Aggregate ##################################################################
  
  # Counts
  dat$count <- 1
  count <- aggregate(count~fage+mage,data=dat,FUN=sum)
  
  # Risk of low birth weight
  dat <- dat |> mutate(low=ifelse(weight<2500,1,0))
  risk <- aggregate(low~fage+mage,data=dat,FUN=mean)
  
  # Drop if too few observations
  risk$low[count$count<30] <- NA
  
  # Generate age difference
  risk$diff <- risk$mage-risk$fage
  
  
### Visualize ##################################################################  

  # Plot 1
  fig1 <- risk |> filter(mage%in%15:49 & fage%in%15:59) |>  
    ggplot( ) +
    geom_tile(aes(mage, fage, fill= low)) +
    scale_y_continuous(breaks=seq(15,55,10),limits=c(-45,60))+
    scale_x_continuous(breaks=seq(20,50,10),limits=c(15,60))+
    geom_line(data=data.frame(x=15:49,y=rep(37,length(15:49))),aes(x,y))+
    labs(x="Maternal age",y="Paternal age")+
    scale_fill_viridis(discrete=F,guide="none",rescaler = rescaler)+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  # Plot 2
  fig2 <- risk |> filter(mage%in%15:49 & fage%in%15:59) |>  
    ggplot( ) +
    geom_tile(aes(mage, diff, fill= low)) +
    geom_line(data=data.frame(x=15:49,y=15:49-37),aes(x,y))+
    scale_x_continuous(breaks=seq(20,50,10),limits=c(15,60))+
    scale_y_continuous(breaks=seq(-45,25,10),limits=c(-45,60))+
    labs(x="Maternal age",y="Age difference")+
    scale_fill_viridis(discrete=F,guide="none",rescaler = rescaler)+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  # Plot 3
  fig3 <- risk |> filter(mage%in%15:49 & fage%in%15:59) |>  
    ggplot( ) +
    geom_tile(aes(fage, diff, fill= low)) +
    geom_line(data=data.frame(x=rep(37,length(15:49-37)),y=15:49-37),aes(x,y))+
    scale_y_continuous(breaks=seq(-45,25,10),limits=c(-45,60))+
    scale_x_continuous(breaks=seq(20,60,10),limits=c(15,60))+
    labs(x="Paternal age",y="Age difference")+
    scale_fill_viridis(discrete=F,rescaler = rescaler)+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  # Save, figure 2
  fig_combined <- fig1 + fig2 + fig3
  ggsave(plot=fig_combined,file="Results/fig2.png",
         width=12,
         height=7)
  
  
### Models #####################################################################  

  # Select cases
  select <- dat |> filter(mage%in%15:49 & fage%in%15:59)
  
  # Generate further variables: difference and interactions squared
  select <- select |> mutate(diff=mage-fage,
                       i_age=mage * fage,
                       i_difm=mage * diff,
                       i_diff=fage * diff)
  
  # Squared terms
  select <- select |> mutate(mage2=mage^2,
                       fage2=fage^2,
                       diff2=diff^2,
                       i_age2=i_age^2,
                       i_difm2=i_difm^2,
                       i_diff2=i_diff^2)
  
  # Dummies
  select <- select |> mutate(dmage=cut_width(mage,width=5,boundary=15),
                       dfage=cut_width(fage,width=5,boundary=15),
                       ddiff=cut_width(diff,width=5,boundary=-45),
                       piecemf=interaction(dmage,dfage),
                       piecemd=interaction(dmage,ddiff),
                       piecefd=interaction(dfage,ddiff))

  # Estimate models maternal + paternal age
  fit1 <- glm(low~mage+fage,data=select,family = binomial(link = "logit"))
  fit2 <- glm(low~mage+mage2+fage2+fage+i_age+i_age2,data=select,family = binomial(link = "logit"))
  fit3 <- glm(low~piecemf,data=select,family = binomial(link = "logit"))
  
  # Estimate models maternal + age difference
  fit4 <- glm(low~mage+diff,data=select,family = binomial(link = "logit"))
  fit5 <- glm(low~mage+mage2+diff+diff2+i_difm+i_difm2,data=select,family = binomial(link = "logit"))
  fit6 <- glm(low~piecemd,data=select,family = binomial(link = "logit"))
  
  # Estimate models maternal + age difference
  fit7 <- glm(low~fage+diff,data=select,family = binomial(link = "logit"))
  fit8 <- glm(low~fage+fage2+diff+diff2+i_diff+i_diff2,data=select,family = binomial(link = "logit"))
  fit9 <- glm(low~piecefd,data=select,family = binomial(link = "logit"))
  
  # Non-linear models without interaction
  fit10 <- glm(low~mage+mage2+fage2+fage,data=select,family = binomial(link = "logit"))
  fit11 <- glm(low~mage+mage2+diff+diff2,data=select,family = binomial(link = "logit"))
  fit12 <- glm(low~fage+fage2+diff+diff2,data=select,family = binomial(link = "logit"))
  
  # Clean up a little
  rm(select);gc()
  
  # AIC of the simple linear models is equal
  AIC(fit1);AIC(fit4);AIC(fit7)
  # For other models: not the same but extremely close
  AIC(fit2);AIC(fit5);AIC(fit8)
  AIC(fit3);AIC(fit6);AIC(fit9)
  
  
### Predict ####################################################################  

  # For prediction
  predictrisk <- risk |> filter(mage%in%15:49 & fage%in%15:59)
  
  # For prediction, interactions
  predictrisk <- predictrisk |> mutate(i_age=mage * fage,
                                       i_difm=mage * diff,
                                       i_diff=fage * diff)
  
  # Squared terms
  predictrisk <- predictrisk |> mutate(mage2=mage^2,
                       fage2=fage^2,
                       diff2=diff^2,
                       i_age2=i_age^2,
                       i_difm2=i_difm^2,
                       i_diff2=i_diff^2)
  
  # Dummies
  predictrisk <- predictrisk |> mutate(dmage=cut_width(mage,width=5,boundary=15),
                       dfage=cut_width(fage,width=5,boundary=15),
                       ddiff=cut_width(diff,width=5,boundary=-45),
                       piecemf=interaction(dmage,dfage),
                       piecemd=interaction(dmage,ddiff),
                       piecefd=interaction(dfage,ddiff))
  

  # Assign results 
  risk1 <- risk2 <- risk3 <- risk4 <- risk5 <- risk6 <- risk7 <- risk8 <- risk9 <- predictrisk
  risk1$low <- predict(fit1,predictrisk,type="response")
  risk2$low <- predict(fit2,predictrisk,type="response")
  risk3$low <- predict(fit3,predictrisk,type="response")
  risk4$low <- predict(fit4,predictrisk,type="response")
  risk5$low <- predict(fit5,predictrisk,type="response")
  risk6$low <- predict(fit6,predictrisk,type="response")
  risk7$low <- predict(fit7,predictrisk,type="response")
  risk8$low <- predict(fit8,predictrisk,type="response")
  risk9$low <- predict(fit9,predictrisk,type="response")
  
  # Give names
  risknames <- c("M+P, linear","M+P, non-linear","M+P, piecewise",
                 "M+D, linear","M+D, non-linear","M+D, piecewise",
                 "P+D, linear","P+D, non-linear","P+D, piecewise",
                 "Observed")
  
  risk1$type <- risknames[1]
  risk2$type <- risknames[2]
  risk3$type <- risknames[3]
  risk4$type <- risknames[4]
  risk5$type <- risknames[5]
  risk6$type <- risknames[6]
  risk7$type <- risknames[7]
  risk8$type <- risknames[8]
  risk9$type <- risknames[9]
  risk$type <- risknames[10]
  
  # Combine
  variables <- names(risk)
  combined <- rbind(risk1[,variables],
                    risk2[,variables],
                    risk3[,variables],
                    risk4[,variables],
                    risk5[,variables],
                    risk6[,variables],
                    risk7[,variables],
                    risk8[,variables],
                    risk9[,variables],
                    risk)
  combined$type <- factor(combined$type,
                          levels=risknames)
  
  # Plot, figure 3
  fig4 <- combined |> filter(mage%in%15:49 & fage%in%15:59 ) |>  
    ggplot( ) +
    geom_tile(aes(mage, fage, fill= low)) +
    facet_wrap(~type,ncol=3)+
    scale_y_continuous(breaks=seq(15,55,10),limits=c(15,55))+
    labs(x="Maternal age",y="Paternal age")+
    scale_fill_viridis(discrete=F,rescaler = rescaler)+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  # Save
  ggsave(plot=fig4,file="Results/fig3.png",
         width=12,
         height=12,
         bg="white")
  
  
### Non-linear with- and without interactions ##################################
  
  # Comparing AIC
  AIC(fit10);AIC(fit11);AIC(fit12)
  AIC(fit3);AIC(fit6);AIC(fit9)

  # For results
  risk12 <- risk11 <- risk10 <- predictrisk
  
  # Predict
  risk10$low <- predict(fit10,predictrisk,type="response")
  risk11$low <- predict(fit11,predictrisk,type="response")
  risk12$low <- predict(fit12,predictrisk,type="response")
  
  # Names
  newrisknames <- c("M+P, non-linear, w/o M*P",
                    "M+D, non-linear, w/o M*D",
                    "P+D, non-linear, w/o P*D") 
  
  # Type
  risk10$type <- newrisknames[1]
  risk11$type <- newrisknames[2]
  risk12$type <- newrisknames[3]
  
  # Combine
  combined2 <- rbind(risk1[,variables],
                    risk2[,variables],
                    risk3[,variables],
                    risk4[,variables],
                    risk5[,variables],
                    risk6[,variables],
                    risk7[,variables],
                    risk8[,variables],
                    risk9[,variables],
                    risk10[,variables],
                    risk11[,variables],
                    risk12[,variables],
                    risk)
  combined2$type <- factor(combined2$type,
                           levels=c("M+P, linear","M+P, non-linear","M+P, non-linear, w/o M*P","M+P, piecewise",
                                    "M+D, linear","M+D, non-linear","M+D, non-linear, w/o M*D","M+D, piecewise",
                                    "P+D, linear","P+D, non-linear","P+D, non-linear, w/o P*D","P+D, piecewise",
                                    "Observed"))
  
  # Plot
  selecttype <- c(newrisknames,"M+P, non-linear","M+D, non-linear","P+D, non-linear")
  fig5 <- combined2 |> filter(type%in%selecttype) |> 
    filter(mage%in%15:49 & fage%in%15:59 ) |>  
    ggplot( ) +
    geom_tile(aes(mage, fage, fill= low)) +
    facet_wrap(~type,ncol=2)+
    scale_y_continuous(breaks=seq(15,55,10),limits=c(15,55))+
    labs(x="Maternal age",y="Paternal age")+
    scale_fill_viridis(discrete=F,rescaler = rescaler)+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  ggsave(plot=fig5,file="Results/fig_S2.png",
         width=12,
         height=12,
         bg="white")
  