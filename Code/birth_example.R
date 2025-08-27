### Birth register data ########################################################

### Loading packages ###########################################################

  library(tidyverse)
  library(data.table)
  library(httr)
  library(hrbrthemes)
  library(viridis)
  library(patchwork)


### Function for scale #########################################################

  # https://stackoverflow.com/questions/48424682/how-do-i-limit-the-range-of-the-viridis-colour-scale
  rescaler <- function(x, to = c(0, 1), from = NULL) {
      ifelse(x<0.15, 
             scales::rescale(x,
                             to = to,
                             from = c(min(x, na.rm = TRUE), 0.15)),
             1)}

  
### Loading data ###############################################################

  ## Data available from:
  ## https://www.nber.org/data/vital-statistics-natality-data.html
  
  # Data for 2023
  url <- "https://data.nber.org/nvss/natality/csv/2023/natality2023us.csv"
  # Documentation:
  # https://data.nber.org/nvss/natality/inputs/pdf/2023/UserGuide2023.pdf
  
  # Download file: the file is around 2GB, might take some time
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
         rename(mage=mager,
                fage=fagecomb,
                weight=dbwt)
  

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
    scale_y_continuous(breaks=seq(-45,25,10),limits=c(-45,60))+
    labs(x="Maternal age",y="Age difference")+
    scale_fill_viridis(discrete=F,rescaler = rescaler)+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  # Save
  fig3 <- fig1 + fig2
  ggsave(plot=fig3,file="Results/fig2.png")
  
  
### Models #####################################################################  
  
  # Generate further variables: interactions
  dat$diff <- dat$mage - dat$fage
  dat <- dat |> mutate(i_age=mage * fage,
                       i_dif=mage * diff,
                       mage2=mage^2,
                       fage2=fage^2,
                       diff2=diff^2,
                       i_age2=i_age^2,
                       i_dif2=i_dif^2)
  
  # Generate further variables: piecewise constant paternal age
  dat <- dat |> mutate(# First age group women
                         p_15_15=ifelse(mage%in%15:19 & fage%in%15:19,1,0),
                         p_15_20=ifelse(mage%in%15:19 & fage%in%20:24,1,0),
                         p_15_25=ifelse(mage%in%15:19 & fage%in%25:29,1,0),
                         p_15_30=ifelse(mage%in%15:19 & fage%in%30:34,1,0),
                         p_15_35=ifelse(mage%in%15:19 & fage%in%35:39,1,0),
                         p_15_40=ifelse(mage%in%15:19 & fage%in%40:44,1,0),
                         p_15_45=ifelse(mage%in%15:19 & fage%in%45:49,1,0),
                         p_15_50=ifelse(mage%in%15:19 & fage%in%50:54,1,0),
                         p_15_55=ifelse(mage%in%15:19 & fage%in%55:59,1,0),
                         # Second age group women
                         p_20_15=ifelse(mage%in%20:24 & fage%in%15:19,1,0),
                         p_20_20=ifelse(mage%in%20:24 & fage%in%20:24,1,0),
                         p_20_25=ifelse(mage%in%20:24 & fage%in%25:29,1,0),
                         p_20_30=ifelse(mage%in%20:24 & fage%in%30:34,1,0),
                         p_20_35=ifelse(mage%in%20:24 & fage%in%35:39,1,0),
                         p_20_40=ifelse(mage%in%20:24 & fage%in%40:44,1,0),
                         p_20_45=ifelse(mage%in%20:24 & fage%in%45:49,1,0),
                         p_20_50=ifelse(mage%in%20:24 & fage%in%50:54,1,0),
                         p_20_55=ifelse(mage%in%20:24 & fage%in%55:59,1,0),
                         # Third age group
                         p_25_15=ifelse(mage%in%25:29 & fage%in%15:19,1,0),
                         p_25_20=ifelse(mage%in%25:29 & fage%in%20:24,1,0),
                         p_25_25=ifelse(mage%in%25:29 & fage%in%25:29,1,0),
                         p_25_30=ifelse(mage%in%25:29 & fage%in%30:34,1,0),
                         p_25_35=ifelse(mage%in%25:29 & fage%in%35:39,1,0),
                         p_25_40=ifelse(mage%in%25:29 & fage%in%40:44,1,0),
                         p_25_45=ifelse(mage%in%25:29 & fage%in%45:49,1,0),
                         p_25_50=ifelse(mage%in%25:29 & fage%in%50:54,1,0),
                         p_25_55=ifelse(mage%in%25:29 & fage%in%55:59,1,0),
                         # Fourth age group 
                         p_30_15=ifelse(mage%in%30:34 & fage%in%15:19,1,0),
                         p_30_20=ifelse(mage%in%30:34 & fage%in%20:24,1,0),
                         p_30_25=ifelse(mage%in%30:34 & fage%in%25:29,1,0),
                         p_30_30=ifelse(mage%in%30:34 & fage%in%30:34,1,0),
                         p_30_35=ifelse(mage%in%30:34 & fage%in%35:39,1,0),
                         p_30_40=ifelse(mage%in%30:34 & fage%in%40:44,1,0),
                         p_30_45=ifelse(mage%in%30:34 & fage%in%45:49,1,0),
                         p_30_50=ifelse(mage%in%30:34 & fage%in%50:54,1,0),
                         p_30_55=ifelse(mage%in%30:34 & fage%in%55:59,1,0),
                         # Firth age group 
                         p_35_15=ifelse(mage%in%35:39 & fage%in%15:19,1,0),
                         p_35_20=ifelse(mage%in%35:39 & fage%in%20:24,1,0),
                         p_35_25=ifelse(mage%in%35:39 & fage%in%25:29,1,0),
                         p_35_30=ifelse(mage%in%35:39 & fage%in%30:34,1,0),
                         p_35_35=ifelse(mage%in%35:39 & fage%in%35:39,1,0),
                         p_35_40=ifelse(mage%in%35:39 & fage%in%40:44,1,0),
                         p_35_45=ifelse(mage%in%35:39 & fage%in%45:49,1,0),
                         p_35_50=ifelse(mage%in%35:39 & fage%in%50:54,1,0),
                         p_35_55=ifelse(mage%in%35:39 & fage%in%55:59,1,0),
                         # Sixth age group
                         p_40_15=ifelse(mage%in%40:44 & fage%in%15:19,1,0),
                         p_40_20=ifelse(mage%in%40:44 & fage%in%20:24,1,0),
                         p_40_25=ifelse(mage%in%40:44 & fage%in%25:29,1,0),
                         p_40_30=ifelse(mage%in%40:44 & fage%in%30:34,1,0),
                         p_40_35=ifelse(mage%in%40:44 & fage%in%35:39,1,0),
                         p_40_40=ifelse(mage%in%40:44 & fage%in%40:44,1,0),
                         p_40_45=ifelse(mage%in%40:44 & fage%in%45:49,1,0),
                         p_40_50=ifelse(mage%in%40:44 & fage%in%50:54,1,0),
                         p_40_55=ifelse(mage%in%40:44 & fage%in%55:59,1,0),
                         # Seventh age group 
                         p_45_15=ifelse(mage%in%45:49 & fage%in%15:19,1,0),
                         p_45_20=ifelse(mage%in%45:49 & fage%in%20:24,1,0),
                         p_45_25=ifelse(mage%in%45:49 & fage%in%25:29,1,0),
                         p_45_30=ifelse(mage%in%45:49 & fage%in%30:34,1,0),
                         p_45_35=ifelse(mage%in%45:49 & fage%in%35:39,1,0),
                         p_45_40=ifelse(mage%in%45:49 & fage%in%40:44,1,0),
                         p_45_45=ifelse(mage%in%45:49 & fage%in%45:49,1,0),
                         p_45_50=ifelse(mage%in%45:49 & fage%in%50:54,1,0),
                         p_45_55=ifelse(mage%in%45:49 & fage%in%55:59,1,0))
  
  # Generate further variables: piecewise constant age difference
  dat <- dat |> mutate(# First age group women
    d_15_44=ifelse(mage%in%15:19 & diff%in%-44:-40,1,0),
    d_15_39=ifelse(mage%in%15:19 & diff%in%-39:-35,1,0),
    d_15_34=ifelse(mage%in%15:19 & diff%in%-34:-30,1,0),
    d_15_29=ifelse(mage%in%15:19 & diff%in%-29:-25,1,0),
    d_15_24=ifelse(mage%in%15:19 & diff%in%-24:-20,1,0),
    d_15_19=ifelse(mage%in%15:19 & diff%in%-19:-15,1,0),
    d_15_14=ifelse(mage%in%15:19 & diff%in%-14:-10,1,0),
    d_15_09=ifelse(mage%in%15:19 & diff%in%-9 :-5 ,1,0),
    d_15_04=ifelse(mage%in%15:19 & diff%in%-4 :-1 ,1,0),
    d_15_00=ifelse(mage%in%15:19 & diff%in%0  :4  ,1,0),
    d_15_05=ifelse(mage%in%15:19 & diff%in%5  :9  ,1,0),
    d_15_10=ifelse(mage%in%15:19 & diff%in%10 :14 ,1,0),
    d_15_15=ifelse(mage%in%15:19 & diff%in%15 :19 ,1,0),
    d_15_20=ifelse(mage%in%15:19 & diff%in%15 :19 ,1,0),
    d_15_25=ifelse(mage%in%15:19 & diff%in%20 :24 ,1,0),
    # Second age group women
    d_20_44=ifelse(mage%in%20:24 & diff%in%-44:-40,1,0),
    d_20_39=ifelse(mage%in%20:24 & diff%in%-39:-35,1,0),
    d_20_34=ifelse(mage%in%20:24 & diff%in%-34:-30,1,0),
    d_20_29=ifelse(mage%in%20:24 & diff%in%-29:-25,1,0),
    d_20_24=ifelse(mage%in%20:24 & diff%in%-24:-20,1,0),
    d_20_19=ifelse(mage%in%20:24 & diff%in%-19:-15,1,0),
    d_20_14=ifelse(mage%in%20:24 & diff%in%-14:-10,1,0),
    d_20_09=ifelse(mage%in%20:24 & diff%in%-9 :-5 ,1,0),
    d_20_04=ifelse(mage%in%20:24 & diff%in%-4 :-1 ,1,0),
    d_20_00=ifelse(mage%in%20:24 & diff%in%0  :4  ,1,0),
    d_20_05=ifelse(mage%in%20:24 & diff%in%5  :9  ,1,0),
    d_20_10=ifelse(mage%in%20:24 & diff%in%10 :14 ,1,0),
    d_20_15=ifelse(mage%in%20:24 & diff%in%15 :19 ,1,0),
    d_20_20=ifelse(mage%in%20:24 & diff%in%15 :19 ,1,0),
    d_20_25=ifelse(mage%in%20:24 & diff%in%20 :24 ,1,0),
    # Third age group
    d_25_44=ifelse(mage%in%25:29 & diff%in%-44:-40,1,0),
    d_25_39=ifelse(mage%in%25:29 & diff%in%-39:-35,1,0),
    d_25_34=ifelse(mage%in%25:29 & diff%in%-34:-30,1,0),
    d_25_29=ifelse(mage%in%25:29 & diff%in%-29:-25,1,0),
    d_25_24=ifelse(mage%in%25:29 & diff%in%-24:-20,1,0),
    d_25_19=ifelse(mage%in%25:29 & diff%in%-19:-15,1,0),
    d_25_14=ifelse(mage%in%25:29 & diff%in%-14:-10,1,0),
    d_25_09=ifelse(mage%in%25:29 & diff%in%-9 :-5 ,1,0),
    d_25_04=ifelse(mage%in%25:29 & diff%in%-4 :-1 ,1,0),
    d_25_00=ifelse(mage%in%25:29 & diff%in%0  :4  ,1,0),
    d_25_05=ifelse(mage%in%25:29 & diff%in%5  :9  ,1,0),
    d_25_10=ifelse(mage%in%25:29 & diff%in%10 :14 ,1,0),
    d_25_15=ifelse(mage%in%25:29 & diff%in%15 :19 ,1,0),
    d_25_20=ifelse(mage%in%25:29 & diff%in%15 :19 ,1,0),
    d_25_25=ifelse(mage%in%25:29 & diff%in%20 :24 ,1,0),
    # Fourth age group 
    d_30_44=ifelse(mage%in%30:34 & diff%in%-44:-40,1,0),
    d_30_39=ifelse(mage%in%30:34 & diff%in%-39:-35,1,0),
    d_30_34=ifelse(mage%in%30:34 & diff%in%-34:-30,1,0),
    d_30_29=ifelse(mage%in%30:34 & diff%in%-29:-25,1,0),
    d_30_24=ifelse(mage%in%30:34 & diff%in%-24:-20,1,0),
    d_30_19=ifelse(mage%in%30:34 & diff%in%-19:-15,1,0),
    d_30_14=ifelse(mage%in%30:34 & diff%in%-14:-10,1,0),
    d_30_09=ifelse(mage%in%30:34 & diff%in%-9 :-5 ,1,0),
    d_30_04=ifelse(mage%in%30:34 & diff%in%-4 :-1 ,1,0),
    d_30_00=ifelse(mage%in%30:34 & diff%in%0  :4  ,1,0),
    d_30_05=ifelse(mage%in%30:34 & diff%in%5  :9  ,1,0),
    d_30_10=ifelse(mage%in%30:34 & diff%in%10 :14 ,1,0),
    d_30_15=ifelse(mage%in%30:34 & diff%in%15 :19 ,1,0),
    d_30_20=ifelse(mage%in%30:34 & diff%in%15 :19 ,1,0),
    d_30_25=ifelse(mage%in%30:34 & diff%in%20 :24 ,1,0),
    # Fifth age group 
    d_35_44=ifelse(mage%in%35:39 & diff%in%-44:-40,1,0),
    d_35_39=ifelse(mage%in%35:39 & diff%in%-39:-35,1,0),
    d_35_34=ifelse(mage%in%35:39 & diff%in%-34:-30,1,0),
    d_35_29=ifelse(mage%in%35:39 & diff%in%-29:-25,1,0),
    d_35_24=ifelse(mage%in%35:39 & diff%in%-24:-20,1,0),
    d_35_19=ifelse(mage%in%35:39 & diff%in%-19:-15,1,0),
    d_35_14=ifelse(mage%in%35:39 & diff%in%-14:-10,1,0),
    d_35_09=ifelse(mage%in%35:39 & diff%in%-9 :-5 ,1,0),
    d_35_04=ifelse(mage%in%35:39 & diff%in%-4 :-1 ,1,0),
    d_35_00=ifelse(mage%in%35:39 & diff%in%0  :4  ,1,0),
    d_35_05=ifelse(mage%in%35:39 & diff%in%5  :9  ,1,0),
    d_35_10=ifelse(mage%in%35:39 & diff%in%10 :14 ,1,0),
    d_35_15=ifelse(mage%in%35:39 & diff%in%15 :19 ,1,0),
    d_35_20=ifelse(mage%in%35:39 & diff%in%15 :19 ,1,0),
    d_35_25=ifelse(mage%in%35:39 & diff%in%20 :24 ,1,0),
    # Sixth age group
    d_40_44=ifelse(mage%in%40:44 & diff%in%-44:-40,1,0),
    d_40_39=ifelse(mage%in%40:44 & diff%in%-39:-35,1,0),
    d_40_34=ifelse(mage%in%40:44 & diff%in%-34:-30,1,0),
    d_40_29=ifelse(mage%in%40:44 & diff%in%-29:-25,1,0),
    d_40_24=ifelse(mage%in%40:44 & diff%in%-24:-20,1,0),
    d_40_19=ifelse(mage%in%40:44 & diff%in%-19:-15,1,0),
    d_40_14=ifelse(mage%in%40:44 & diff%in%-14:-10,1,0),
    d_40_09=ifelse(mage%in%40:44 & diff%in%-9 :-5 ,1,0),
    d_40_04=ifelse(mage%in%40:44 & diff%in%-4 :-1 ,1,0),
    d_40_00=ifelse(mage%in%40:44 & diff%in%0  :4  ,1,0),
    d_40_05=ifelse(mage%in%40:44 & diff%in%5  :9  ,1,0),
    d_40_10=ifelse(mage%in%40:44 & diff%in%10 :14 ,1,0),
    d_40_15=ifelse(mage%in%40:44 & diff%in%15 :19 ,1,0),
    d_40_20=ifelse(mage%in%40:44 & diff%in%15 :19 ,1,0),
    d_40_25=ifelse(mage%in%40:44 & diff%in%20 :24 ,1,0),
    # Seventh age group 
    d_45_44=ifelse(mage%in%45:49 & diff%in%-44:-40,1,0),
    d_45_39=ifelse(mage%in%45:49 & diff%in%-39:-35,1,0),
    d_45_34=ifelse(mage%in%45:49 & diff%in%-34:-30,1,0),
    d_45_29=ifelse(mage%in%45:49 & diff%in%-29:-25,1,0),
    d_45_24=ifelse(mage%in%45:49 & diff%in%-24:-20,1,0),
    d_45_19=ifelse(mage%in%45:49 & diff%in%-19:-15,1,0),
    d_45_14=ifelse(mage%in%45:49 & diff%in%-14:-10,1,0),
    d_45_09=ifelse(mage%in%45:49 & diff%in%-9 :-5 ,1,0),
    d_45_04=ifelse(mage%in%45:49 & diff%in%-4 :-1 ,1,0),
    d_45_00=ifelse(mage%in%45:49 & diff%in%0  :4  ,1,0),
    d_45_05=ifelse(mage%in%45:49 & diff%in%5  :9  ,1,0),
    d_45_10=ifelse(mage%in%45:49 & diff%in%10 :14 ,1,0),
    d_45_15=ifelse(mage%in%45:49 & diff%in%15 :19 ,1,0),
    d_45_20=ifelse(mage%in%45:49 & diff%in%15 :19 ,1,0),
    d_45_25=ifelse(mage%in%45:49 & diff%in%20 :24 ,1,0))
  
  # Estimate
  fit1 <- lm(low~mage+fage,data=dat)
  fit2 <- lm(low~mage+mage2+fage2+fage+i_age+i_age2,data=dat)
  fit3 <- lm(low~p_15_15+p_15_20+p_15_25+p_15_30+p_15_35+p_15_40+p_15_45+p_15_50+p_15_55+
               p_20_15+p_20_20+p_20_25+p_20_30+p_20_35+p_20_40+p_20_45+p_20_50+p_20_55+
               p_25_15+p_25_20+        p_25_30+p_25_35+p_25_40+p_25_45+p_25_50+p_25_55+
               p_30_15+p_30_20+p_30_25+p_30_30+p_30_35+p_30_40+p_30_45+p_30_50+p_30_55+
               p_35_15+p_35_20+p_35_25+p_35_30+p_35_35+p_35_40+p_35_45+p_35_50+p_35_55+
               p_40_15+p_40_20+p_40_25+p_40_30+p_40_35+p_40_40+p_40_45+p_40_50+p_40_55+
               p_45_15+p_45_20+p_45_25+p_45_30+p_45_35+p_45_40+p_45_45+p_45_50+p_45_55,data=dat)
  
  
  fit4 <- lm(low~mage+diff,data=dat)
  fit5 <- lm(low~mage+mage2+diff+diff2+i_dif+i_dif2,data=dat)
  fit6 <- lm(low~d_15_44+d_15_39+d_15_34+d_15_29+d_15_24+d_15_19+d_15_14+d_15_09+d_15_04+d_15_00+d_15_05+d_15_10+d_15_15+d_15_20+d_15_25+
               d_20_44+d_20_39+d_20_34+d_20_29+d_20_24+d_20_19+d_20_14+d_20_09+d_20_04+d_20_00+d_20_05+d_20_10+d_20_15+d_20_20+d_20_25+
               d_25_44+d_25_39+d_25_34+d_25_29+d_25_24+d_25_19+d_25_14+d_25_09+d_25_04+d_25_00+d_25_05+d_25_10+d_25_15+d_25_20+d_25_25+
               d_30_44+d_30_39+d_30_34+d_30_29+d_30_24+d_30_19+d_30_14+d_30_09+d_30_04        +d_30_05+d_30_10+d_30_15+d_30_20+d_30_25+
               d_35_44+d_35_39+d_35_34+d_35_29+d_35_24+d_35_19+d_35_14+d_35_09+d_35_04+d_35_00+d_35_05+d_35_10+d_35_15+d_35_20+d_35_25+
               d_40_44+d_40_39+d_40_34+d_40_29+d_40_24+d_40_19+d_40_14+d_40_09+d_40_04+d_40_00+d_40_05+d_40_10+d_40_15+d_40_20+d_40_25+
               d_45_44+d_45_39+d_45_34+d_45_29+d_45_24+d_45_19+d_45_14+d_45_09+d_45_04+d_45_00+d_45_05+d_45_10+d_45_15+d_45_20+d_45_25,data=dat)
  
  
### Predict ####################################################################  

  # For prediction    
  risk <- risk |> mutate(i_age=mage * fage,
                         i_dif=mage * diff,
                         mage2=mage^2,
                         fage2=fage^2,
                         diff2=diff^2,
                         i_age2=i_age^2,
                         i_dif2=i_dif^2)
  
  # Generate further variables: piecewise constant
  risk <- risk |> mutate(# First age group women
    p_15_15=ifelse(mage%in%15:19 & fage%in%15:19,1,0),
    p_15_20=ifelse(mage%in%15:19 & fage%in%20:24,1,0),
    p_15_25=ifelse(mage%in%15:19 & fage%in%25:29,1,0),
    p_15_30=ifelse(mage%in%15:19 & fage%in%30:34,1,0),
    p_15_35=ifelse(mage%in%15:19 & fage%in%35:39,1,0),
    p_15_40=ifelse(mage%in%15:19 & fage%in%40:44,1,0),
    p_15_45=ifelse(mage%in%15:19 & fage%in%45:49,1,0),
    p_15_50=ifelse(mage%in%15:19 & fage%in%50:54,1,0),
    p_15_55=ifelse(mage%in%15:19 & fage%in%55:59,1,0),
    # Second age group women
    p_20_15=ifelse(mage%in%20:24 & fage%in%15:19,1,0),
    p_20_20=ifelse(mage%in%20:24 & fage%in%20:24,1,0),
    p_20_25=ifelse(mage%in%20:24 & fage%in%25:29,1,0),
    p_20_30=ifelse(mage%in%20:24 & fage%in%30:34,1,0),
    p_20_35=ifelse(mage%in%20:24 & fage%in%35:39,1,0),
    p_20_40=ifelse(mage%in%20:24 & fage%in%40:44,1,0),
    p_20_45=ifelse(mage%in%20:24 & fage%in%45:49,1,0),
    p_20_50=ifelse(mage%in%20:24 & fage%in%50:54,1,0),
    p_20_55=ifelse(mage%in%20:24 & fage%in%55:59,1,0),
    # Third age group
    p_25_15=ifelse(mage%in%25:29 & fage%in%15:19,1,0),
    p_25_20=ifelse(mage%in%25:29 & fage%in%20:24,1,0),
    p_25_25=ifelse(mage%in%25:29 & fage%in%25:29,1,0),
    p_25_30=ifelse(mage%in%25:29 & fage%in%30:34,1,0),
    p_25_35=ifelse(mage%in%25:29 & fage%in%35:39,1,0),
    p_25_40=ifelse(mage%in%25:29 & fage%in%40:44,1,0),
    p_25_45=ifelse(mage%in%25:29 & fage%in%45:49,1,0),
    p_25_50=ifelse(mage%in%25:29 & fage%in%50:54,1,0),
    p_25_55=ifelse(mage%in%25:29 & fage%in%55:59,1,0),
    # Fourth age group 
    p_30_15=ifelse(mage%in%30:34 & fage%in%15:19,1,0),
    p_30_20=ifelse(mage%in%30:34 & fage%in%20:24,1,0),
    p_30_25=ifelse(mage%in%30:34 & fage%in%25:29,1,0),
    p_30_30=ifelse(mage%in%30:34 & fage%in%30:34,1,0),
    p_30_35=ifelse(mage%in%30:34 & fage%in%35:39,1,0),
    p_30_40=ifelse(mage%in%30:34 & fage%in%40:44,1,0),
    p_30_45=ifelse(mage%in%30:34 & fage%in%45:49,1,0),
    p_30_50=ifelse(mage%in%30:34 & fage%in%50:54,1,0),
    p_30_55=ifelse(mage%in%30:34 & fage%in%55:59,1,0),
    # Firth age group 
    p_35_15=ifelse(mage%in%35:39 & fage%in%15:19,1,0),
    p_35_20=ifelse(mage%in%35:39 & fage%in%20:24,1,0),
    p_35_25=ifelse(mage%in%35:39 & fage%in%25:29,1,0),
    p_35_30=ifelse(mage%in%35:39 & fage%in%30:34,1,0),
    p_35_35=ifelse(mage%in%35:39 & fage%in%35:39,1,0),
    p_35_40=ifelse(mage%in%35:39 & fage%in%40:44,1,0),
    p_35_45=ifelse(mage%in%35:39 & fage%in%45:49,1,0),
    p_35_50=ifelse(mage%in%35:39 & fage%in%50:54,1,0),
    p_35_55=ifelse(mage%in%35:39 & fage%in%55:59,1,0),
    # Sixth age group
    p_40_15=ifelse(mage%in%40:44 & fage%in%15:19,1,0),
    p_40_20=ifelse(mage%in%40:44 & fage%in%20:24,1,0),
    p_40_25=ifelse(mage%in%40:44 & fage%in%25:29,1,0),
    p_40_30=ifelse(mage%in%40:44 & fage%in%30:34,1,0),
    p_40_35=ifelse(mage%in%40:44 & fage%in%35:39,1,0),
    p_40_40=ifelse(mage%in%40:44 & fage%in%40:44,1,0),
    p_40_45=ifelse(mage%in%40:44 & fage%in%45:49,1,0),
    p_40_50=ifelse(mage%in%40:44 & fage%in%50:54,1,0),
    p_40_55=ifelse(mage%in%40:44 & fage%in%55:59,1,0),
    # Seventh age group 
    p_45_15=ifelse(mage%in%45:49 & fage%in%15:19,1,0),
    p_45_20=ifelse(mage%in%45:49 & fage%in%20:24,1,0),
    p_45_25=ifelse(mage%in%45:49 & fage%in%25:29,1,0),
    p_45_30=ifelse(mage%in%45:49 & fage%in%30:34,1,0),
    p_45_35=ifelse(mage%in%45:49 & fage%in%35:39,1,0),
    p_45_40=ifelse(mage%in%45:49 & fage%in%40:44,1,0),
    p_45_45=ifelse(mage%in%45:49 & fage%in%45:49,1,0),
    p_45_50=ifelse(mage%in%45:49 & fage%in%50:54,1,0),
    p_45_55=ifelse(mage%in%45:49 & fage%in%55:59,1,0))
  
  risk <- risk |> mutate(# First age group women
    d_15_44=ifelse(mage%in%15:19 & diff%in%-44:-40,1,0),
    d_15_39=ifelse(mage%in%15:19 & diff%in%-39:-35,1,0),
    d_15_34=ifelse(mage%in%15:19 & diff%in%-34:-30,1,0),
    d_15_29=ifelse(mage%in%15:19 & diff%in%-29:-25,1,0),
    d_15_24=ifelse(mage%in%15:19 & diff%in%-24:-20,1,0),
    d_15_19=ifelse(mage%in%15:19 & diff%in%-19:-15,1,0),
    d_15_14=ifelse(mage%in%15:19 & diff%in%-14:-10,1,0),
    d_15_09=ifelse(mage%in%15:19 & diff%in%-9 :-5 ,1,0),
    d_15_04=ifelse(mage%in%15:19 & diff%in%-4 :-1 ,1,0),
    d_15_00=ifelse(mage%in%15:19 & diff%in%0  :4  ,1,0),
    d_15_05=ifelse(mage%in%15:19 & diff%in%5  :9  ,1,0),
    d_15_10=ifelse(mage%in%15:19 & diff%in%10 :14 ,1,0),
    d_15_15=ifelse(mage%in%15:19 & diff%in%15 :19 ,1,0),
    d_15_20=ifelse(mage%in%15:19 & diff%in%15 :19 ,1,0),
    d_15_25=ifelse(mage%in%15:19 & diff%in%20 :24 ,1,0),
    # Second age group women
    d_20_44=ifelse(mage%in%20:24 & diff%in%-44:-40,1,0),
    d_20_39=ifelse(mage%in%20:24 & diff%in%-39:-35,1,0),
    d_20_34=ifelse(mage%in%20:24 & diff%in%-34:-30,1,0),
    d_20_29=ifelse(mage%in%20:24 & diff%in%-29:-25,1,0),
    d_20_24=ifelse(mage%in%20:24 & diff%in%-24:-20,1,0),
    d_20_19=ifelse(mage%in%20:24 & diff%in%-19:-15,1,0),
    d_20_14=ifelse(mage%in%20:24 & diff%in%-14:-10,1,0),
    d_20_09=ifelse(mage%in%20:24 & diff%in%-9 :-5 ,1,0),
    d_20_04=ifelse(mage%in%20:24 & diff%in%-4 :-1 ,1,0),
    d_20_00=ifelse(mage%in%20:24 & diff%in%0  :4  ,1,0),
    d_20_05=ifelse(mage%in%20:24 & diff%in%5  :9  ,1,0),
    d_20_10=ifelse(mage%in%20:24 & diff%in%10 :14 ,1,0),
    d_20_15=ifelse(mage%in%20:24 & diff%in%15 :19 ,1,0),
    d_20_20=ifelse(mage%in%20:24 & diff%in%15 :19 ,1,0),
    d_20_25=ifelse(mage%in%20:24 & diff%in%20 :24 ,1,0),
    # Third age group
    d_25_44=ifelse(mage%in%25:29 & diff%in%-44:-40,1,0),
    d_25_39=ifelse(mage%in%25:29 & diff%in%-39:-35,1,0),
    d_25_34=ifelse(mage%in%25:29 & diff%in%-34:-30,1,0),
    d_25_29=ifelse(mage%in%25:29 & diff%in%-29:-25,1,0),
    d_25_24=ifelse(mage%in%25:29 & diff%in%-24:-20,1,0),
    d_25_19=ifelse(mage%in%25:29 & diff%in%-19:-15,1,0),
    d_25_14=ifelse(mage%in%25:29 & diff%in%-14:-10,1,0),
    d_25_09=ifelse(mage%in%25:29 & diff%in%-9 :-5 ,1,0),
    d_25_04=ifelse(mage%in%25:29 & diff%in%-4 :-1 ,1,0),
    d_25_00=ifelse(mage%in%25:29 & diff%in%0  :4  ,1,0),
    d_25_05=ifelse(mage%in%25:29 & diff%in%5  :9  ,1,0),
    d_25_10=ifelse(mage%in%25:29 & diff%in%10 :14 ,1,0),
    d_25_15=ifelse(mage%in%25:29 & diff%in%15 :19 ,1,0),
    d_25_20=ifelse(mage%in%25:29 & diff%in%15 :19 ,1,0),
    d_25_25=ifelse(mage%in%25:29 & diff%in%20 :24 ,1,0),
    # Fourth age group 
    d_30_44=ifelse(mage%in%30:34 & diff%in%-44:-40,1,0),
    d_30_39=ifelse(mage%in%30:34 & diff%in%-39:-35,1,0),
    d_30_34=ifelse(mage%in%30:34 & diff%in%-34:-30,1,0),
    d_30_29=ifelse(mage%in%30:34 & diff%in%-29:-25,1,0),
    d_30_24=ifelse(mage%in%30:34 & diff%in%-24:-20,1,0),
    d_30_19=ifelse(mage%in%30:34 & diff%in%-19:-15,1,0),
    d_30_14=ifelse(mage%in%30:34 & diff%in%-14:-10,1,0),
    d_30_09=ifelse(mage%in%30:34 & diff%in%-9 :-5 ,1,0),
    d_30_04=ifelse(mage%in%30:34 & diff%in%-4 :-1 ,1,0),
    d_30_00=ifelse(mage%in%30:34 & diff%in%0  :4  ,1,0),
    d_30_05=ifelse(mage%in%30:34 & diff%in%5  :9  ,1,0),
    d_30_10=ifelse(mage%in%30:34 & diff%in%10 :14 ,1,0),
    d_30_15=ifelse(mage%in%30:34 & diff%in%15 :19 ,1,0),
    d_30_20=ifelse(mage%in%30:34 & diff%in%15 :19 ,1,0),
    d_30_25=ifelse(mage%in%30:34 & diff%in%20 :24 ,1,0),
    # Fifth age group 
    d_35_44=ifelse(mage%in%35:39 & diff%in%-44:-40,1,0),
    d_35_39=ifelse(mage%in%35:39 & diff%in%-39:-35,1,0),
    d_35_34=ifelse(mage%in%35:39 & diff%in%-34:-30,1,0),
    d_35_29=ifelse(mage%in%35:39 & diff%in%-29:-25,1,0),
    d_35_24=ifelse(mage%in%35:39 & diff%in%-24:-20,1,0),
    d_35_19=ifelse(mage%in%35:39 & diff%in%-19:-15,1,0),
    d_35_14=ifelse(mage%in%35:39 & diff%in%-14:-10,1,0),
    d_35_09=ifelse(mage%in%35:39 & diff%in%-9 :-5 ,1,0),
    d_35_04=ifelse(mage%in%35:39 & diff%in%-4 :-1 ,1,0),
    d_35_00=ifelse(mage%in%35:39 & diff%in%0  :4  ,1,0),
    d_35_05=ifelse(mage%in%35:39 & diff%in%5  :9  ,1,0),
    d_35_10=ifelse(mage%in%35:39 & diff%in%10 :14 ,1,0),
    d_35_15=ifelse(mage%in%35:39 & diff%in%15 :19 ,1,0),
    d_35_20=ifelse(mage%in%35:39 & diff%in%15 :19 ,1,0),
    d_35_25=ifelse(mage%in%35:39 & diff%in%20 :24 ,1,0),
    # Sixth age group
    d_40_44=ifelse(mage%in%40:44 & diff%in%-44:-40,1,0),
    d_40_39=ifelse(mage%in%40:44 & diff%in%-39:-35,1,0),
    d_40_34=ifelse(mage%in%40:44 & diff%in%-34:-30,1,0),
    d_40_29=ifelse(mage%in%40:44 & diff%in%-29:-25,1,0),
    d_40_24=ifelse(mage%in%40:44 & diff%in%-24:-20,1,0),
    d_40_19=ifelse(mage%in%40:44 & diff%in%-19:-15,1,0),
    d_40_14=ifelse(mage%in%40:44 & diff%in%-14:-10,1,0),
    d_40_09=ifelse(mage%in%40:44 & diff%in%-9 :-5 ,1,0),
    d_40_04=ifelse(mage%in%40:44 & diff%in%-4 :-1 ,1,0),
    d_40_00=ifelse(mage%in%40:44 & diff%in%0  :4  ,1,0),
    d_40_05=ifelse(mage%in%40:44 & diff%in%5  :9  ,1,0),
    d_40_10=ifelse(mage%in%40:44 & diff%in%10 :14 ,1,0),
    d_40_15=ifelse(mage%in%40:44 & diff%in%15 :19 ,1,0),
    d_40_20=ifelse(mage%in%40:44 & diff%in%15 :19 ,1,0),
    d_40_25=ifelse(mage%in%40:44 & diff%in%20 :24 ,1,0),
    # Seventh age group 
    d_45_44=ifelse(mage%in%45:49 & diff%in%-44:-40,1,0),
    d_45_39=ifelse(mage%in%45:49 & diff%in%-39:-35,1,0),
    d_45_34=ifelse(mage%in%45:49 & diff%in%-34:-30,1,0),
    d_45_29=ifelse(mage%in%45:49 & diff%in%-29:-25,1,0),
    d_45_24=ifelse(mage%in%45:49 & diff%in%-24:-20,1,0),
    d_45_19=ifelse(mage%in%45:49 & diff%in%-19:-15,1,0),
    d_45_14=ifelse(mage%in%45:49 & diff%in%-14:-10,1,0),
    d_45_09=ifelse(mage%in%45:49 & diff%in%-9 :-5 ,1,0),
    d_45_04=ifelse(mage%in%45:49 & diff%in%-4 :-1 ,1,0),
    d_45_00=ifelse(mage%in%45:49 & diff%in%0  :4  ,1,0),
    d_45_05=ifelse(mage%in%45:49 & diff%in%5  :9  ,1,0),
    d_45_10=ifelse(mage%in%45:49 & diff%in%10 :14 ,1,0),
    d_45_15=ifelse(mage%in%45:49 & diff%in%15 :19 ,1,0),
    d_45_20=ifelse(mage%in%45:49 & diff%in%15 :19 ,1,0),
    d_45_25=ifelse(mage%in%45:49 & diff%in%20 :24 ,1,0))
  
  # Assign results 
  risk1 <- risk2 <- risk3 <- risk4 <- risk5 <- risk6 <- risk
  risk1$low <- predict(fit1,risk)
  risk2$low <- predict(fit2,risk)
  risk3$low <- predict(fit3,risk)
  risk4$low <- predict(fit4,risk)
  risk5$low <- predict(fit5,risk)
  risk6$low <- predict(fit6,risk)
  
  # Give names
  risknames <- c("Observed","Pat. age, linear","Pat. age, interaction","Pat. age, piecewise constant",
                 "Age diff., linear","Age diff., interaction","Age diff., piecewise constant")
  
  risk$type <- risknames[1]
  risk1$type <- risknames[2]
  risk2$type <- risknames[3]
  risk3$type <- risknames[4]
  risk4$type <- risknames[5]
  risk5$type <- risknames[6]
  risk6$type <- risknames[7]
  
  # Combine
  combined <- rbind(risk,risk1,risk2,risk3,risk4,risk5,risk6)
  combined$type <- factor(combined$type,
                          levels=risknames)
  
  # Plot
  fig4 <- combined |> filter(mage%in%15:49 & fage%in%15:59 & type!="Observed") |>  
    ggplot( ) +
    geom_tile(aes(mage, fage, fill= low)) +
    facet_wrap(~type)+
    scale_y_continuous(breaks=seq(15,55,10),limits=c(15,55))+
    labs(x="Maternal age",y="Paternal age")+
    scale_fill_viridis(discrete=F,rescaler = rescaler)+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  # Save
  ggsave(plot=fig4,file="Results/fig3.png")