# Covar Selection with treatment effect interaction
N<-30
size<-100000
delta<-1

tstat3<-c()
tstat2<-c()
tstat1<-c()

the_coef3<-c()
the_coef2<-c()
the_coef1<-c()

for(i in 1:size){

  priority <- runif(N)
  random <- runif(N)
  assignment <- priority>=random
  outcome <- rnorm(N)
  outcome <- outcome + priority
  outcome[assignment]<-outcome[assignment]+delta*(1-priority)[assignment]

  #generate linear models
  ss3_lm <- lm(outcome ~ assignment + priority + assignment*priority)
  ss2_lm <- lm(outcome ~ assignment + priority)
  ss1_lm <- lm(outcome ~ assignment)

  #calculate T statistic for each linear model
  tstat3<-rbind(tstat3,summary(ss3_lm)$coef[,1]/summary(ss3_lm)$coef[,2])
  tstat2<-rbind(tstat2,summary(ss2_lm)$coef[,1]/summary(ss2_lm)$coef[,2])
  tstat1<-rbind(tstat1,summary(ss1_lm)$coef[,1]/summary(ss1_lm)$coef[,2])

  #coefficiencts for each linear model
  the_coef3<-rbind(the_coef3,summary(ss3_lm)$coef[,1])
  the_coef2<-rbind(the_coef2,summary(ss2_lm)$coef[,1])
  the_coef1<-rbind(the_coef1,summary(ss1_lm)$coef[,1])

}

cat("Case with correlation and interaciton effect. \n \n")

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


#Random Selection with treatment effect interaction
N<-30
size<-100000
delta<-1

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
  assignment <- coinflip>=random
  outcome <- rnorm(N)
  outcome <- outcome + priority
  outcome[assignment]<-outcome[assignment]+delta*(1-priority)[assignment]

  #generate linear model
  ss3_lm <- lm(outcome ~ assignment + priority + assignment*priority)
  ss2_lm <- lm(outcome ~ assignment + priority)
  ss1_lm <- lm(outcome ~ assignment)

  #calculate T statistic for each linear model
  tstat3<-rbind(tstat3,summary(ss3_lm)$coef[,1]/summary(ss3_lm)$coef[,2])
  tstat2<-rbind(tstat2,summary(ss2_lm)$coef[,1]/summary(ss2_lm)$coef[,2])
  tstat1<-rbind(tstat1,summary(ss1_lm)$coef[,1]/summary(ss1_lm)$coef[,2])

  #coefficiencts for each linear model
  the_coef3<-rbind(the_coef3,summary(ss3_lm)$coef[,1])
  the_coef2<-rbind(the_coef2,summary(ss2_lm)$coef[,1])
  the_coef1<-rbind(the_coef1,summary(ss1_lm)$coef[,1])

}


cat("Output from traditional experimental design \n")
cat("Model 3 contains:      ", names(coef(ss3_lm)), "\n" )
cat("Model 3 coef estimates:", sprintf("%1.6f", colMeans(the_coef3)) , "\n" )
cat("Model 3 t statistics:  ", sprintf("%1.6f", colMeans(tstat3)), "\n \n" )


cat("Model 2 contains:      ", names(coef(ss2_lm)), "\n" )
cat("Model 2 coef estimates:", sprintf("%1.6f", colMeans(the_coef2)) , "\n" )
cat("Model 2 t statistics:  ", sprintf("%1.6f", colMeans(tstat2)), "\n \n" )


cat("Model 1 contains:      ", names(coef(ss1_lm)), "\n" )
cat("Model 1 coef estimates:", sprintf("%1.6f", colMeans(the_coef1)) , "\n" )
cat("Model 1 t statistics:  ", sprintf("%1.6f", colMeans(tstat1)), "\n \n" )

