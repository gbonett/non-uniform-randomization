# Non-uniform randomization, followed by traditional experimental design

#This is a function with N, number of trials, and beta(x,x) as inputs.
#It will return the tstat2 for the non-uniform and traditional methods.



lm_normout_interaction_sim <- function(N=30,size=10000,beta_shape=1){

#delta = treatment effect
delta<-1

#initialize 
tstat3<-c()
tstat2<-c()
tstat1<-c()

the_coef3<-c()
the_coef2<-c()
the_coef1<-c()

for(i in 1:size){

  #priority score follows a beta distribution
  priority <- rbeta(N,beta_shape,beta_shape)
  random <- runif(N)

  #assignment based on priority score
  group <- priority>=random

  #outcome variable is normal and correlated with priority score, and no interaction effect.	
  outcome <- rnorm(N)
  outcome[group] <- outcome[group] + delta*(priority[group]-.5)^2

  #generate linear models
  ss3_lm <- lm(outcome ~ group + priority + group*priority)
  ss2_lm <- lm(outcome ~ group + priority)
  ss1_lm <- lm(outcome ~ group)

  #calculate T statistic for each linear model
  tstat3<-rbind(tstat3,summary(ss3_lm)$coef[,1]/summary(ss3_lm)$coef[,2])
  tstat2<-rbind(tstat2,summary(ss2_lm)$coef[,1]/summary(ss2_lm)$coef[,2])
  tstat1<-rbind(tstat1,summary(ss1_lm)$coef[,1]/summary(ss1_lm)$coef[,2])

  #coefficiencts for each linear model
  the_coef3<-rbind(the_coef3,summary(ss3_lm)$coef[,1])
  the_coef2<-rbind(the_coef2,summary(ss2_lm)$coef[,1])
  the_coef1<-rbind(the_coef1,summary(ss1_lm)$coef[,1])

}


#all of this is printed to the screen
cat("Case with correlation and interaction effect. \n \n")
cat("N=", N, "\n")
cat("size=", size , "\n")

cat("Output from non-uniform randomization experimental design \n")

cat("Model 3 contains:      ", names(coef(ss3_lm)), "\n" )
cat("Model 3 coef estimates:", sprintf("%1.6f",colMeans(the_coef3)) , "\n" )
cat("Model 3 t statistics:  ", sprintf("%1.6f",colMeans(tstat3)), "\n \n" )


cat("Model 2 contains:      ", names(coef(ss2_lm)), "\n" )
cat("Model 2 coef estimates:", sprintf("%1.6f", colMeans(the_coef2)) , "\n" )
cat("Model 2 t statistics:  ", sprintf("%1.6f", colMeans(tstat2)), "\n \n" )


cat("Model 1 contains:      ", names(coef(ss1_lm)), "\n" )
cat("Model 1 coef estimates:", sprintf("%1.6f", colMeans(the_coef1)) , "\n" )
cat("Model 1 t statistics:  ", sprintf("%1.6f", colMeans(tstat1)), "\n \n" )


#saves the tstat2 to return later
non_unif_t <- colMeans(tstat2)[2]

#Random Selection with treatment effect interaction
tstat3<-c()
tstat2<-c()
tstat1<-c()

the_coef3<-c()
the_coef2<-c()
the_coef1<-c()

for(i in 1:size){

  priority <- runif(N)
  random <- runif(N)
  coinflip <- c(rep(.5,times=30))

  #selection uses coinflip, not priority score
  group <- coinflip>=random
  outcome <- rnorm(N)
  outcome[group] <- outcome[group] + delta*(priority[group]-.5)^2

  #generate linear model
  ss3_lm <- lm(outcome ~ group + priority + group*priority)
  ss2_lm <- lm(outcome ~ group + priority)
  ss1_lm <- lm(outcome ~ group)

  #calculate T statistic for each linear model
  tstat3<-rbind(tstat3,summary(ss3_lm)$coef[,1]/summary(ss3_lm)$coef[,2])
  tstat2<-rbind(tstat2,summary(ss2_lm)$coef[,1]/summary(ss2_lm)$coef[,2])
  tstat1<-rbind(tstat1,summary(ss1_lm)$coef[,1]/summary(ss1_lm)$coef[,2])

  #coefficients for each linear model
  the_coef3<-rbind(the_coef3,summary(ss3_lm)$coef[,1])
  the_coef2<-rbind(the_coef2,summary(ss2_lm)$coef[,1])
  the_coef1<-rbind(the_coef1,summary(ss1_lm)$coef[,1])

}


cat("Output from traditional experimental design \n")
cat("N=", N, "\n")
cat("size=", size , "\n")

cat("Model 3 contains:      ", names(coef(ss3_lm)), "\n" )
cat("Model 3 coef estimates:", sprintf("%1.6f", colMeans(the_coef3)) , "\n" )
cat("Model 3 t statistics:  ", sprintf("%1.6f", colMeans(tstat3)), "\n \n" )


cat("Model 2 contains:      ", names(coef(ss2_lm)), "\n" )
cat("Model 2 coef estimates:", sprintf("%1.6f", colMeans(the_coef2)) , "\n" )
cat("Model 2 t statistics:  ", sprintf("%1.6f", colMeans(tstat2)), "\n \n" )


cat("Model 1 contains:      ", names(coef(ss1_lm)), "\n" )
cat("Model 1 coef estimates:", sprintf("%1.6f", colMeans(the_coef1)) , "\n" )
cat("Model 1 t statistics:  ", sprintf("%1.6f", colMeans(tstat1)), "\n \n" )


#here's the functions output
unif_t <- colMeans(tstat1)[2]
results <- c(beta_shape, unif_t, non_unif_t)

return(results)
}

