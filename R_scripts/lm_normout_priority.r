# Covar Selection with priority score correlation and no treatment effect interaction
N<-30
size<-40000
B2_coef<-3.5
delta<-1

tstat3<-c()
tstat2<-c()
tstat1<-c()

coefs3<-c()
coefs2<-c()
coefs1<-c()

for(i in 1:size){

  priority <- runif(N)
  random <- runif(N)
  group <- priority>=random
  outcome <- rnorm(N)
  outcome <- outcome + B2_coef*priority
  outcome[group] <- outcome[group] + delta

  #mean center priority score
  priority <- priority - .5

  #generate linear models
  ss3_lm <- lm(outcome ~ group + priority + group*priority)
  ss2_lm <- lm(outcome ~ group + priority)
  ss1_lm <- lm(outcome ~ group)

  #calculate T statistic for each linear model
  tstat3<-rbind(tstat3,summary(ss3_lm)$coef[,1]/summary(ss3_lm)$coef[,2])
  tstat2<-rbind(tstat2,summary(ss2_lm)$coef[,1]/summary(ss2_lm)$coef[,2])
  tstat1<-rbind(tstat1,summary(ss1_lm)$coef[,1]/summary(ss1_lm)$coef[,2])

  #coefficiencts for each linear model
  coefs3<-rbind(coefs3,summary(ss3_lm)$coef[,1])
  coefs2<-rbind(coefs2,summary(ss2_lm)$coef[,1])
  coefs1<-rbind(coefs1,summary(ss1_lm)$coef[,1])

}

cat("Case with priority score correlation and no interaciton effect. \n \n")
cat("N=", N, "\n")
cat("size=", size , "\n")

cat("Output from non-uniform randomization experimental design \n")

cat("Model 3 contains:      ", names(coef(ss3_lm)), "\n" )
cat("Model 3 coef estimates:", sprintf("%1.6f",colMeans(coefs3)) , "\n" )
cat("Model 3 t statistics:  ", sprintf("%1.6f",colMeans(tstat3)), "\n \n" )


cat("Model 2 contains:      ", names(coef(ss2_lm)), "\n" )
cat("Model 2 coef estimates:", sprintf("%1.6f", colMeans(coefs2)) , "\n" )
cat("Model 2 t statistics:  ", sprintf("%1.6f", colMeans(tstat2)), "\n \n" )


cat("Model 1 contains:      ", names(coef(ss1_lm)), "\n" )
cat("Model 1 coef estimates:", sprintf("%1.6f", colMeans(coefs1)) , "\n" )
cat("Model 1 t statistics:  ", sprintf("%1.6f", colMeans(tstat1)), "\n \n" )


#Random Selection with treatment effect interaction
tstat3<-c()
tstat2<-c()
tstat1<-c()

coefs3<-c()
coefs2<-c()
coefs1<-c()

for(i in 1:size){

  priority <- runif(N)
  random <- runif(N)
  coinflip <- c(rep(.5,times=30))

  #selection uses coinflip, not priority score
  group <- coinflip>=random
  outcome <- rnorm(N)
  outcome <- outcome + B2_coef*priority
  outcome[group] <- outcome[group] + delta
  
  #mean center priority score
  priority <- priority - .5

  #generate linear model
  ss3_lm <- lm(outcome ~ group + priority + group*priority)
  ss2_lm <- lm(outcome ~ group + priority)
  ss1_lm <- lm(outcome ~ group)

  #calculate T statistic for each linear model
  tstat3<-rbind(tstat3,summary(ss3_lm)$coef[,1]/summary(ss3_lm)$coef[,2])
  tstat2<-rbind(tstat2,summary(ss2_lm)$coef[,1]/summary(ss2_lm)$coef[,2])
  tstat1<-rbind(tstat1,summary(ss1_lm)$coef[,1]/summary(ss1_lm)$coef[,2])

  #coefficiencts for each linear model
  coefs3<-rbind(coefs3,summary(ss3_lm)$coef[,1])
  coefs2<-rbind(coefs2,summary(ss2_lm)$coef[,1])
  coefs1<-rbind(coefs1,summary(ss1_lm)$coef[,1])

}


cat("Output from traditional experimental design \n")
cat("N=", N, "\n")
cat("size=", size , "\n")

cat("Model 3 contains:      ", names(coef(ss3_lm)), "\n" )
cat("Model 3 coef estimates:", sprintf("%1.6f", colMeans(coefs3)) , "\n" )
cat("Model 3 t statistics:  ", sprintf("%1.6f", colMeans(tstat3)), "\n \n" )


cat("Model 2 contains:      ", names(coef(ss2_lm)), "\n" )
cat("Model 2 coef estimates:", sprintf("%1.6f", colMeans(coefs2)) , "\n" )
cat("Model 2 t statistics:  ", sprintf("%1.6f", colMeans(tstat2)), "\n \n" )


cat("Model 1 contains:      ", names(coef(ss1_lm)), "\n" )
cat("Model 1 coef estimates:", sprintf("%1.6f", colMeans(coefs1)) , "\n" )
cat("Model 1 t statistics:  ", sprintf("%1.6f", colMeans(tstat1)), "\n \n" )

