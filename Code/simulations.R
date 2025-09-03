### Linear probability model 

intercept <- 0.07
mage <- 0.0005
fage <- 0.00075

sizes <- seq(50,25000,by=50)
replications <- 100

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
      
      dat$predicted <- intercept + dat$agem*mage + dat$agef*fage + abs(rnorm(sd=0.01,n=size))
      dat$prob <- runif(n=size)
      dat$outcome <- as.numeric(dat$predicted>=dat$prob)
      
      dat$diff <- dat$agem - dat$agef
      
      fit1 <- lm(outcome~agem+agef,data=dat)
      fit2 <- lm(outcome~agem+diff,data=dat)
  
      tmp1[rep] <- (summary(fit1))$coefficients["agem",4]
      tmp2[rep] <- (summary(fit2))$coefficients["agem",4]
      
  }
  
  result1[which(sizes==size)] <- mean(tmp1)
  result2[which(sizes==size)] <- mean(tmp2)
  
}

plot(sizes,result1,type="l",ylim=c(0,1))
lines(sizes,result2,col="red")
abline(h=0.05)
