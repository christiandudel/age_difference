intercept <- 0.07
mage <- 0.00015
fage <- 0.00030

sizes <- seq(50,2500,by=5)
replications <- 500

result1 <- numeric(length(sizes))
result2 <- numeric(length(sizes))

for(size in sizes) {
  
  cat(".")

  tmp1 <- numeric(replications)
  tmp2 <- numeric(replications)
  
    for(rep in 1:replications) {

      agem <- sample(20:45,size=size,rep=T)
      agef <- sample(20:45,size=size,rep=T)
      
      dat <- data.frame(agem=agem,
                        agef=agef)
      
      dat$predicted <- intercept + dat$agem*mage + dat$agef*fage + abs(rnorm(sd=0.02,n=size))
      
      dat$diff <- dat$agem - dat$agef
      
      fit1 <- lm(predicted~agem+agef,data=dat)
      fit2 <- lm(predicted~agem+diff,data=dat)
  
      tmp1[rep] <- (summary(fit1))$coefficients["agem",4]
      tmp2[rep] <- (summary(fit2))$coefficients["agem",4]
      
  }
  
  result1[which(sizes==size)] <- mean(tmp1)
  result2[which(sizes==size)] <- mean(tmp2)
  
}

plot(sizes,result1,type="l",ylim=c(0,1))
lines(sizes,result2,col="red")
abline(h=0.05)
