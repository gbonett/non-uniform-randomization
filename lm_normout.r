# Covar Selection with treatment effect interaction
N<-30
size<-20000
delta<-1
mat3<-c()
mat2<-c()
mat1<-c()

for(i in 1:size){

  priority <- runif(N)
  random <- runif(N)
  assignment <- priority>=random
  outcome <- rnorm(N)
  outcome[assignment]<-outcome[assignment]+delta*priority[assignment]

  #generate linear model
  ss3_lm <- lm(outcome ~ assignment + priority + assignment*priority)
  ss2_lm <- lm(outcome ~ assignment + priority)
  ss1_lm <- lm(outcome ~ assignment)
  mat3<-rbind(mat3,summary(ss3_lm)$coef[,1]/summary(ss3_lm)$coef[,2])
  mat2<-rbind(mat2,summary(ss2_lm)$coef[,1]/summary(ss2_lm)$coef[,2])
  mat1<-rbind(mat1,summary(ss1_lm)$coef[,1]/summary(ss1_lm)$coef[,2])

}

names(coef(lm))
 mean(mat3[,1])
 mean(mat3[,2])
 mean(mat3[,3])
 mean(mat3[,4])
 
 mean(mat2[,1])
 mean(mat2[,2])
 mean(mat2[,3])

 mean(mat1[,1])
 mean(mat1[,2])



#Random Selection with treatment effect interaction
N<-30
size<-20000
delta<-1
mat3<-c()
mat2<-c()
mat1<-c()

for(i in 1:size){

  priority <- runif(N)
  random <- runif(N)
  coinflip <- c(rep(.5,times=30))
  #selection uses coinflip, not priority score
  assignment <- coinflip>=random
  outcome <- rnorm(N)
  outcome[assignment]<-outcome[assignment]+delta*priority[assignment]

  #generate linear model
  ss3_lm <- lm(outcome ~ assignment + priority + assignment*priority)
  ss2_lm <- lm(outcome ~ assignment + priority)
  ss1_lm <- lm(outcome ~ assignment)
  mat3<-rbind(mat3,summary(ss3_lm)$coef[,1]/summary(ss3_lm)$coef[,2])
  mat2<-rbind(mat2,summary(ss2_lm)$coef[,1]/summary(ss2_lm)$coef[,2])
  mat1<-rbind(mat1,summary(ss1_lm)$coef[,1]/summary(ss1_lm)$coef[,2])

}

names(coef(lm))
mean(mat3[,1])
mean(mat3[,2])
mean(mat3[,3])
mean(mat3[,4])
 
mean(mat2[,1])
mean(mat2[,2])
mean(mat2[,3])
 
mean(mat1[,1])
mean(mat1[,2])
