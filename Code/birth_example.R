### Birth register data ########################################################

### Loading packages ###########################################################

  library(tidyverse)
  library(data.table)
  library(httr)
  library(hrbrthemes)
  library(viridis)
  library(patchwork)


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
  risk$low[count$count<50] <- NA
  
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
    scale_fill_viridis(discrete=F,guide="none")+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  # Plot 2
  fig2 <- risk |> filter(mage%in%15:49 & fage%in%15:59) |>  
    ggplot( ) +
    geom_tile(aes(mage, diff, fill= low)) +
    geom_line(data=data.frame(x=15:49,y=15:49-37),aes(x,y))+
    scale_y_continuous(breaks=seq(-45,25,10),limits=c(-45,60))+
    labs(x="Maternal age",y="Age difference")+
    scale_fill_viridis(discrete=F)+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  fig3 <- fig1 + fig2
  
  
### Models #####################################################################  
  