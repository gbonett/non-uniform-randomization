# Covar Selection with treatment effect interaction
N<-30
size<-1000000
delta<-1

treject<-c()
out.varassign<-c()
out.varnot<-c()
out.meanassign<-c()

for(i in 1:size){
  priority <- runif(N)
  random <- runif(N)
  assignment <- priority>=random
  outcome <- rnorm(N)
  outcome[assignment]<-outcome[assignment]+delta*priority[assignment]
  out.varassign[i]<-var(outcome[assignment])
  out.varnot[i]<-var(outcome[!assignment])
  out.meanassign[i]<-mean(outcome[assignment])
  tstat<-t.test(outcome[assignment],outcome[!assignment],var.equal=T)$statistic
  tcrit<-qt(.975,df=N-2)
  treject[i]<-abs(tstat)>=tcrit
}

sum(treject)/size
mean(out.varassign)
mean(out.varnot)
mean(out.meanassign)

#Random Selection with treatment effect interaction
N<-30
size<-1000000
delta<-1

treject<-c()
out.varassign<-c()
out.varnot<-c()
out.meanassign<-c()

for(i in 1:size){
  priority <- runif(N)
  random <- runif(N)
  coinflip <- c(rep(.5,times=30))
  #selection uses coinflip, not priority score
  assignment <- coinflip>=random
  outcome <- rnorm(N)
  outcome[assignment]<-outcome[assignment]+delta*priority[assignment]
  out.varassign[i]<-var(outcome[assignment])
  out.varnot[i]<-var(outcome[!assignment])
  out.meanassign[i]<-mean(outcome[assignment])
  tstat<-t.test(outcome[assignment],outcome[!assignment],var.equal=T)$statistic
  tcrit<-qt(.975,df=N-2)
  treject[i]<-abs(tstat)>=tcrit
}
sum(treject)/size
mean(out.varassign)
mean(out.varnot)
mean(out.meanassign)
