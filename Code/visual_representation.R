### Packages ##################################################################

  library(dplyr)
  library(ggplot2)
  library(hrbrthemes)
  library(viridis)
  library(patchwork)



### Visual representation of the problem #######################################
  
  # Data
  dat <- expand.grid(X=1:3,Y=1:3)
  dat$Z <- as.factor(letters[1:9])
  dat$D <- dat$X-dat$Y
  dat$label <- paste0("(",dat$X,",",dat$Y,",",dat$D,")")
  
  # Age + age
  fig1 <- dat |>  ggplot( ) +
    geom_tile(aes(X, Y, fill= Z)) +
    scale_y_continuous(breaks=c(1,2,3),limits=c(-3,4))+
    geom_label(aes(X, Y,label=label),alpha=0.7)+
    labs(x="Maternal age",y="Paternal age")+
    scale_fill_viridis(discrete=T,guide="none")+
    theme_ipsum() +
    theme(panel.grid.minor = element_blank())
  
  # Age + age difference
  fig2 <- dat |>  ggplot( ) +
    geom_tile(aes(X, D, fill= Z)) +
    scale_y_continuous(breaks=seq(-2,2,1),limits=c(-3,4))+
    scale_fill_viridis(discrete=T,guide="none")+
    geom_label(aes(X, D,label=label),alpha=0.7)+
    labs(x="Maternal age",y="Age difference")+
    theme_ipsum()+
    theme(panel.grid.minor = element_blank())
  
  # Age + age difference
  fig3 <- dat |>  ggplot( ) +
    geom_tile(aes(Y, D, fill= Z)) +
    scale_y_continuous(breaks=seq(-2,2,1),limits=c(-3,4))+
    scale_fill_viridis(discrete=T,guide="none")+
    geom_label(aes(Y, D,label=label),alpha=0.7)+
    labs(x="Paternal age",y="Age difference")+
    theme_ipsum()+
    theme(panel.grid.minor = element_blank())
  
  
  # Plot
  fig_combined <- fig1 | fig2 | fig3
  
  # Save
  ggsave(plot=fig_combined,
         file="Results/fig1.png",
         width=10,
         height=10)
  
  