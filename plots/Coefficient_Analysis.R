# Plots Coefficient Analysis
##  Index: main predictors validation, main predictors 2021, other predictors validation, other predictors 2021

## main predictors
### validation

par(mfrow = c(2,3))
# beta 2 2010
hist(run.2010$BUGSoutput$sims.list$beta2, main = "inc 2010", xlab = "Coefficient Value", xlim = c(min(run.2010$BUGSoutput$sims.list$beta2),max(run.2010$BUGSoutput$sims.list$beta2)),
     ylim = c(0,700)
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta2),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta2>0)/length(run.2021$BUGSoutput$sims.list$beta2),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta2),3))
       ))
# beta 2 2012
hist(run.2012$BUGSoutput$sims.list$beta2, main = "inc 2012", xlab = "Coefficient Value", xlim = c(min(run.2012$BUGSoutput$sims.list$beta2),max(run.2012$BUGSoutput$sims.list$beta2)),
     ylim = c(0,700)
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta2),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta2>0)/length(run.2012$BUGSoutput$sims.list$beta2),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta2),3))
       ))
# beta 2 2017
hist(run.2017$BUGSoutput$sims.list$beta2, main = "inc 2017", xlab = "Coefficient Value", xlim = c(min(run.2017$BUGSoutput$sims.list$beta2),max(run.2017$BUGSoutput$sims.list$beta2)),
     ylim = c(0,700)
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta2),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta2>0)/length(run.2017$BUGSoutput$sims.list$beta2),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta2),3))
       ))
par(mfrow = c(2,3))

# beta 4 2010
hist(run.2010$BUGSoutput$sims.list$beta4, main = "past vote share 2010", xlab = "Coefficient Value", xlim = c(min(run.2010$BUGSoutput$sims.list$beta4), max(run.2010$BUGSoutput$sims.list$beta4)),
     ylim = c(0,500),
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta4),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta4>0)/length(run.2021$BUGSoutput$sims.list$beta4),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta4),3))
       ))
# beta 4 2012
hist(run.2012$BUGSoutput$sims.list$beta4, main = "past vote share 2012", xlab = "Coefficient Value", xlim = c(min(run.2012$BUGSoutput$sims.list$beta4),max(run.2012$BUGSoutput$sims.list$beta4)),
     ylim = c(0,500)
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta4),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta4>0)/length(run.2012$BUGSoutput$sims.list$beta4),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta4),3))
       ))
# beta 4 2017
hist(run.2017$BUGSoutput$sims.list$beta4, main = "past vote share 2017", xlab = "Coefficient Value", xlim = c(min(run.2017$BUGSoutput$sims.list$beta4),max(run.2017$BUGSoutput$sims.list$beta4)),
     ylim = c(0,500)
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta4),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta4>0)/length(run.2017$BUGSoutput$sims.list$beta4),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta4),3))
       ))
par(mfrow = c(2,3))
# beta 6 2010
hist(run.2010$BUGSoutput$sims.list$beta6, main = "GDP Growth 2010", xlab = "Coefficient Value",xlim = c(min(run.2010$BUGSoutput$sims.list$beta6),max(run.2010$BUGSoutput$sims.list$beta6)),
     ylim = c(0,800)
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta6),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta6>0)/length(run.2010$BUGSoutput$sims.list$beta6),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta6),3))
       ))

# beta 6 2012
hist(run.2012$BUGSoutput$sims.list$beta6, main = "GDP Growth 2012", xlab = "Coefficient Value",xlim = c(min(run.2012$BUGSoutput$sims.list$beta6),max(run.2012$BUGSoutput$sims.list$beta6)),
     ylim = c(0,800)
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta6),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta6>0)/length(run.2012$BUGSoutput$sims.list$beta6),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta6),3))
       ))

# beta 6 2017
hist(run.2017$BUGSoutput$sims.list$beta6, main = "GDP Growth 2017", xlab = "Coefficient Value",xlim = c(min(run.2017$BUGSoutput$sims.list$beta6),max(run.2017$BUGSoutput$sims.list$beta6)),
     ylim = c(0,800)
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta6),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend("topright",
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta6>0)/length(run.2017$BUGSoutput$sims.list$beta6),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta6),3))
       ))




### 2021

par(mfrow = c(2,3))
# beta 2
hist(run.2021$BUGSoutput$sims.list$beta2, main = "inc", xlab = "Coefficient Value", xlim = c(min(run.2021$BUGSoutput$sims.list$beta2),max(run.2021$BUGSoutput$sims.list$beta2)),
     ylim = c(0,500)
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta2),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('Pr(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta2>0)/length(run.2021$BUGSoutput$sims.list$beta2),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta2),3))
       ))
# beta4
hist(run.2021$BUGSoutput$sims.list$beta4, main = "past vote share", xlab = "Coefficient Value", xlim = c(min(run.2021$BUGSoutput$sims.list$beta4),max(run.2021$BUGSoutput$sims.list$beta4)),
     ylim = c(0,500)
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta4),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('Pr(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta4>0)/length(run.2021$BUGSoutput$sims.list$beta4),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta4),3))
       ))
# beta 6
hist(run.2021$BUGSoutput$sims.list$beta6, main = "GDP Growth", xlab = "Coefficient Value",xlim = c(min(run.2021$BUGSoutput$sims.list$beta6),max(run.2021$BUGSoutput$sims.list$beta6))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta6),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('Pr(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta6>0)/length(run.2021$BUGSoutput$sims.list$beta6),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta6),3))
       ))


## other variables 
### validation 2010

par(mfrow = c(2,3))
# beta 1 2010
hist(run.2010$BUGSoutput$sims.list$beta1, main = "voters 2010", xlab = "beta 1", xlim = c(min(run.2010$BUGSoutput$sims.list$beta1),max(run.2010$BUGSoutput$sims.list$beta1))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta1),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta2>0)/length(run.2010$BUGSoutput$sims.list$beta1),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta1),3))
       ))

# beta 3
hist(run.2010$BUGSoutput$sims.list$beta3, main = "inc cabinet 2010", xlab = "beta 3", xlim = c(min(run.2010$BUGSoutput$sims.list$beta3),max(run.2010$BUGSoutput$sims.list$beta3))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta3),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta3>0)/length(run.2010$BUGSoutput$sims.list$beta3),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta3),3))
       ))

# beta 5
hist(run.2010$BUGSoutput$sims.list$beta5, main = "past2 vote share 2010", xlab = "beta 5", xlim = c(min(run.2010$BUGSoutput$sims.list$beta5),max(run.2010$BUGSoutput$sims.list$beta5))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta5),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta5>0)/length(run.2010$BUGSoutput$sims.list$beta5),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta5),3))
       ))
par(mfrow = c(2,3))
# beta 7
hist(run.2010$BUGSoutput$sims.list$beta7, main = "inflation rate 2010", xlab = "beta 7", xlim = c(min(run.2010$BUGSoutput$sims.list$beta7),max(run.2010$BUGSoutput$sims.list$beta7))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta7),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta7>0)/length(run.2010$BUGSoutput$sims.list$beta7),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta7),3))
       ))

# beta 8
hist(run.2010$BUGSoutput$sims.list$beta8, main = "population 2010", xlab = "beta 8", xlim = c(min(run.2010$BUGSoutput$sims.list$beta8),max(run.2010$BUGSoutput$sims.list$beta8))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta8),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta8>0)/length(run.2010$BUGSoutput$sims.list$beta8),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta8),3))
       ))

# beta 9
hist(run.2010$BUGSoutput$sims.list$beta9, main ="male share 2010", xlab = "beta 9", xlim = c(min(run.2010$BUGSoutput$sims.list$beta9),max(run.2010$BUGSoutput$sims.list$beta9))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta9),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta9>0)/length(run.2010$BUGSoutput$sims.list$beta9),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta9),3))
       ))
par(mfrow = c(2,3))
# beta 10
hist(run.2010$BUGSoutput$sims.list$beta10, main = "married 2010", xlab = "beta 10", xlim = c(min(run.2010$BUGSoutput$sims.list$beta10),max(run.2010$BUGSoutput$sims.list$beta10))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta10),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta10>0)/length(run.2010$BUGSoutput$sims.list$beta10),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta10),3))
       ))
# beta 11
hist(run.2010$BUGSoutput$sims.list$beta11, main = "widowed 2010", xlab = "beta 11", xlim = c(min(run.2010$BUGSoutput$sims.list$beta11),max(run.2010$BUGSoutput$sims.list$beta11))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta11),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta11>0)/length(run.2010$BUGSoutput$sims.list$beta11),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta11),3))
       ))

# beta 12
hist(run.2010$BUGSoutput$sims.list$beta12, main = "divorced 2010", xlab = "beta 12", xlim = c(min(run.2010$BUGSoutput$sims.list$beta12),max(run.2021$BUGSoutput$sims.list$beta12))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta12),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta12>0)/length(run.2010$BUGSoutput$sims.list$beta12),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta12),3))
       ))
par(mfrow = c(2,3))
# beta 13
hist(run.2010$BUGSoutput$sims.list$beta13, main = "from20to39 2010", xlab = "beta 13", xlim = c(min(run.2010$BUGSoutput$sims.list$beta13),max(run.2010$BUGSoutput$sims.list$beta13))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta13),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta13>0)/length(run.2010$BUGSoutput$sims.list$beta13),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta13),3))
       ))
# beta 14
hist(run.2010$BUGSoutput$sims.list$beta14, main = "from40to65 2010", xlab = "beta 14", xlim = c(min(run.2010$BUGSoutput$sims.list$beta14),max(run.2010$BUGSoutput$sims.list$beta14))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta14),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta14>0)/length(run.2010$BUGSoutput$sims.list$beta14),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta14),3))
       ))

# beta 15
hist(run.2010$BUGSoutput$sims.list$beta15, main = "from65to79 2010", xlab = "beta 15", xlim = c(min(run.2010$BUGSoutput$sims.list$beta15),max(run.2010$BUGSoutput$sims.list$beta15))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta15),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta15>0)/length(run.2010$BUGSoutput$sims.list$beta15),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta15),3))
       ))
par(mfrow = c(2,3))            
# beta 16
hist(run.2010$BUGSoutput$sims.list$beta16, main = "80orolder 2010", xlab = "beta 16", xlim = c(min(run.2010$BUGSoutput$sims.list$beta16),max(run.2010$BUGSoutput$sims.list$beta16))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta16),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):', round(sum(run.2010$BUGSoutput$sims.list$beta16>0)/length(run.2010$BUGSoutput$sims.list$beta16),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta16),3))
       )) 
# beta 17
hist(run.2010$BUGSoutput$sims.list$beta17, main = "demographic pressure 2010", xlab = "beta 17", xlim = c(min(run.2010$BUGSoutput$sims.list$beta17),max(run.2010$BUGSoutput$sims.list$beta17))
)
abline(v = mean(run.2010$BUGSoutput$sims.list$beta17),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2010$BUGSoutput$sims.list$beta17>0)/length(run.2010$BUGSoutput$sims.list$beta17),3)),
         paste('mean:',round(mean(run.2010$BUGSoutput$sims.list$beta17),3))
       ))  






### validation 2012

par(mfrow = c(2,3))
# beta 1 2012
hist(run.2012$BUGSoutput$sims.list$beta1, main = "voters 2012", xlab = "beta 1", xlim = c(min(run.2012$BUGSoutput$sims.list$beta1),max(run.2012$BUGSoutput$sims.list$beta1))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta1),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta2>0)/length(run.2012$BUGSoutput$sims.list$beta1),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta1),3))
       ))

# beta 3
hist(run.2012$BUGSoutput$sims.list$beta3, main = "inc cabinet 2012", xlab = "beta 3", xlim = c(min(run.2012$BUGSoutput$sims.list$beta3),max(run.2012$BUGSoutput$sims.list$beta3))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta3),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta3>0)/length(run.2012$BUGSoutput$sims.list$beta3),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta3),3))
       ))

# beta 5
hist(run.2012$BUGSoutput$sims.list$beta5, main = "past2 vote share 2012", xlab = "beta 5", xlim = c(min(run.2012$BUGSoutput$sims.list$beta5),max(run.2012$BUGSoutput$sims.list$beta5))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta5),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta5>0)/length(run.2012$BUGSoutput$sims.list$beta5),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta5),3))
       ))
par(mfrow = c(2,3))
# beta 7
hist(run.2012$BUGSoutput$sims.list$beta7, main = "inflation rate 2012", xlab = "beta 7", xlim = c(min(run.2012$BUGSoutput$sims.list$beta7),max(run.2012$BUGSoutput$sims.list$beta7))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta7),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta7>0)/length(run.2012$BUGSoutput$sims.list$beta7),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta7),3))
       ))

# beta 8
hist(run.2012$BUGSoutput$sims.list$beta8, main = "population 2012", xlab = "beta 8", xlim = c(min(run.2012$BUGSoutput$sims.list$beta8),max(run.2012$BUGSoutput$sims.list$beta8))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta8),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta8>0)/length(run.2012$BUGSoutput$sims.list$beta8),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta8),3))
       ))

# beta 9
hist(run.2012$BUGSoutput$sims.list$beta9, main ="male share 2012", xlab = "beta 9", xlim = c(min(run.2012$BUGSoutput$sims.list$beta9),max(run.2012$BUGSoutput$sims.list$beta9))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta9),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta9>0)/length(run.2012$BUGSoutput$sims.list$beta9),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta9),3))
       ))
par(mfrow = c(2,3))
# beta 10
hist(run.2012$BUGSoutput$sims.list$beta10, main = "married 2012", xlab = "beta 10", xlim = c(min(run.2012$BUGSoutput$sims.list$beta10),max(run.2012$BUGSoutput$sims.list$beta10))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta10),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta10>0)/length(run.2012$BUGSoutput$sims.list$beta10),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta10),3))
       ))
# beta 11
hist(run.2012$BUGSoutput$sims.list$beta11, main = "widowed 2012", xlab = "beta 11", xlim = c(min(run.2012$BUGSoutput$sims.list$beta11),max(run.2012$BUGSoutput$sims.list$beta11))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta11),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta11>0)/length(run.2012$BUGSoutput$sims.list$beta11),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta11),3))
       ))

# beta 12
hist(run.2012$BUGSoutput$sims.list$beta12, main = "divorced 2012", xlab = "beta 12", xlim = c(min(run.2012$BUGSoutput$sims.list$beta12),max(run.2021$BUGSoutput$sims.list$beta12))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta12),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta12>0)/length(run.2012$BUGSoutput$sims.list$beta12),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta12),3))
       ))
par(mfrow = c(2,3))
# beta 13
hist(run.2012$BUGSoutput$sims.list$beta13, main = "from20to39 2012", xlab = "beta 13", xlim = c(min(run.2012$BUGSoutput$sims.list$beta13),max(run.2012$BUGSoutput$sims.list$beta13))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta13),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta13>0)/length(run.2012$BUGSoutput$sims.list$beta13),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta13),3))
       ))
# beta 14
hist(run.2012$BUGSoutput$sims.list$beta14, main = "from40to65 2012", xlab = "beta 14", xlim = c(min(run.2012$BUGSoutput$sims.list$beta14),max(run.2012$BUGSoutput$sims.list$beta14))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta14),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta14>0)/length(run.2012$BUGSoutput$sims.list$beta14),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta14),3))
       ))

# beta 15
hist(run.2012$BUGSoutput$sims.list$beta15, main = "from65to79 2012", xlab = "beta 15", xlim = c(min(run.2012$BUGSoutput$sims.list$beta15),max(run.2012$BUGSoutput$sims.list$beta15))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta15),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta15>0)/length(run.2012$BUGSoutput$sims.list$beta15),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta15),3))
       ))
par(mfrow = c(2,3))            
# beta 16
hist(run.2012$BUGSoutput$sims.list$beta16, main = "80orolder 2012", xlab = "beta 16", xlim = c(min(run.2012$BUGSoutput$sims.list$beta16),max(run.2012$BUGSoutput$sims.list$beta16))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta16),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):', round(sum(run.2012$BUGSoutput$sims.list$beta16>0)/length(run.2012$BUGSoutput$sims.list$beta16),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta16),3))
       )) 
# beta 17
hist(run.2012$BUGSoutput$sims.list$beta17, main = "demographic pressure 2012", xlab = "beta 17", xlim = c(min(run.2012$BUGSoutput$sims.list$beta17),max(run.2012$BUGSoutput$sims.list$beta17))
)
abline(v = mean(run.2012$BUGSoutput$sims.list$beta17),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2012$BUGSoutput$sims.list$beta17>0)/length(run.2012$BUGSoutput$sims.list$beta17),3)),
         paste('mean:',round(mean(run.2012$BUGSoutput$sims.list$beta17),3))
       ))  




### validation 2017

par(mfrow = c(2,3))
# beta 1 2017
hist(run.2017$BUGSoutput$sims.list$beta1, main = "voters 2017", xlab = "beta 1", xlim = c(min(run.2017$BUGSoutput$sims.list$beta1),max(run.2017$BUGSoutput$sims.list$beta1))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta1),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta2>0)/length(run.2017$BUGSoutput$sims.list$beta1),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta1),3))
       ))

# beta 3
hist(run.2017$BUGSoutput$sims.list$beta3, main = "inc cabinet 2017", xlab = "beta 3", xlim = c(min(run.2017$BUGSoutput$sims.list$beta3),max(run.2017$BUGSoutput$sims.list$beta3))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta3),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta3>0)/length(run.2017$BUGSoutput$sims.list$beta3),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta3),3))
       ))

# beta 5
hist(run.2017$BUGSoutput$sims.list$beta5, main = "past2 vote share 2017", xlab = "beta 5", xlim = c(min(run.2017$BUGSoutput$sims.list$beta5),max(run.2017$BUGSoutput$sims.list$beta5))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta5),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta5>0)/length(run.2017$BUGSoutput$sims.list$beta5),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta5),3))
       ))
par(mfrow = c(2,3))
# beta 7
hist(run.2017$BUGSoutput$sims.list$beta7, main = "inflation rate 2017", xlab = "beta 7", xlim = c(min(run.2017$BUGSoutput$sims.list$beta7),max(run.2017$BUGSoutput$sims.list$beta7))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta7),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta7>0)/length(run.2017$BUGSoutput$sims.list$beta7),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta7),3))
       ))

# beta 8
hist(run.2017$BUGSoutput$sims.list$beta8, main = "population 2017", xlab = "beta 8", xlim = c(min(run.2017$BUGSoutput$sims.list$beta8),max(run.2017$BUGSoutput$sims.list$beta8))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta8),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta8>0)/length(run.2017$BUGSoutput$sims.list$beta8),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta8),3))
       ))

# beta 9
hist(run.2017$BUGSoutput$sims.list$beta9, main ="male share 2017", xlab = "beta 9", xlim = c(min(run.2017$BUGSoutput$sims.list$beta9),max(run.2017$BUGSoutput$sims.list$beta9))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta9),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta9>0)/length(run.2017$BUGSoutput$sims.list$beta9),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta9),3))
       ))
par(mfrow = c(2,3))
# beta 10
hist(run.2017$BUGSoutput$sims.list$beta10, main = "married 2017", xlab = "beta 10", xlim = c(min(run.2017$BUGSoutput$sims.list$beta10),max(run.2017$BUGSoutput$sims.list$beta10))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta10),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta10>0)/length(run.2017$BUGSoutput$sims.list$beta10),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta10),3))
       ))
# beta 11
hist(run.2017$BUGSoutput$sims.list$beta11, main = "widowed 2017", xlab = "beta 11", xlim = c(min(run.2017$BUGSoutput$sims.list$beta11),max(run.2017$BUGSoutput$sims.list$beta11))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta11),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta11>0)/length(run.2017$BUGSoutput$sims.list$beta11),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta11),3))
       ))

# beta 12
hist(run.2017$BUGSoutput$sims.list$beta12, main = "divorced 2017", xlab = "beta 12", xlim = c(min(run.2017$BUGSoutput$sims.list$beta12),max(run.2021$BUGSoutput$sims.list$beta12))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta12),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta12>0)/length(run.2017$BUGSoutput$sims.list$beta12),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta12),3))
       ))
par(mfrow = c(2,3))
# beta 13
hist(run.2017$BUGSoutput$sims.list$beta13, main = "from20to39 2017", xlab = "beta 13", xlim = c(min(run.2017$BUGSoutput$sims.list$beta13),max(run.2017$BUGSoutput$sims.list$beta13))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta13),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta13>0)/length(run.2017$BUGSoutput$sims.list$beta13),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta13),3))
       ))
# beta 14
hist(run.2017$BUGSoutput$sims.list$beta14, main = "from40to65 2017", xlab = "beta 14", xlim = c(min(run.2017$BUGSoutput$sims.list$beta14),max(run.2017$BUGSoutput$sims.list$beta14))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta14),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta14>0)/length(run.2017$BUGSoutput$sims.list$beta14),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta14),3))
       ))

# beta 15
hist(run.2017$BUGSoutput$sims.list$beta15, main = "from65to79 2017", xlab = "beta 15", xlim = c(min(run.2017$BUGSoutput$sims.list$beta15),max(run.2017$BUGSoutput$sims.list$beta15))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta15),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta15>0)/length(run.2017$BUGSoutput$sims.list$beta15),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta15),3))
       ))
par(mfrow = c(2,3))            
# beta 16
hist(run.2017$BUGSoutput$sims.list$beta16, main = "80orolder 2017", xlab = "beta 16", xlim = c(min(run.2017$BUGSoutput$sims.list$beta16),max(run.2017$BUGSoutput$sims.list$beta16))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta16),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):', round(sum(run.2017$BUGSoutput$sims.list$beta16>0)/length(run.2017$BUGSoutput$sims.list$beta16),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta16),3))
       )) 
# beta 17
hist(run.2017$BUGSoutput$sims.list$beta17, main = "demographic pressure 2017", xlab = "beta 17", xlim = c(min(run.2017$BUGSoutput$sims.list$beta17),max(run.2017$BUGSoutput$sims.list$beta17))
)
abline(v = mean(run.2017$BUGSoutput$sims.list$beta17),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2017$BUGSoutput$sims.list$beta17>0)/length(run.2017$BUGSoutput$sims.list$beta17),3)),
         paste('mean:',round(mean(run.2017$BUGSoutput$sims.list$beta17),3))
       ))  

### validation 2021
par(mfrow = c(2,3))
# beta 1 2021
hist(run.2021$BUGSoutput$sims.list$beta1, main = "voters 2021", xlab = "beta 1", xlim = c(min(run.2021$BUGSoutput$sims.list$beta1),max(run.2021$BUGSoutput$sims.list$beta1))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta1),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta2>0)/length(run.2021$BUGSoutput$sims.list$beta1),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta1),3))
       ))

# beta 3
hist(run.2021$BUGSoutput$sims.list$beta3, main = "inc cabinet 2021", xlab = "beta 3", xlim = c(min(run.2021$BUGSoutput$sims.list$beta3),max(run.2021$BUGSoutput$sims.list$beta3))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta3),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta3>0)/length(run.2021$BUGSoutput$sims.list$beta3),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta3),3))
       ))

# beta 5
hist(run.2021$BUGSoutput$sims.list$beta5, main = "past2 vote share 2021", xlab = "beta 5", xlim = c(min(run.2021$BUGSoutput$sims.list$beta5),max(run.2021$BUGSoutput$sims.list$beta5))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta5),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta5>0)/length(run.2021$BUGSoutput$sims.list$beta5),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta5),3))
       ))
par(mfrow = c(2,3))
# beta 7
hist(run.2021$BUGSoutput$sims.list$beta7, main = "inflation rate 2021", xlab = "beta 7", xlim = c(min(run.2021$BUGSoutput$sims.list$beta7),max(run.2021$BUGSoutput$sims.list$beta7))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta7),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta7>0)/length(run.2021$BUGSoutput$sims.list$beta7),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta7),3))
       ))

# beta 8
hist(run.2021$BUGSoutput$sims.list$beta8, main = "population 2021", xlab = "beta 8", xlim = c(min(run.2021$BUGSoutput$sims.list$beta8),max(run.2021$BUGSoutput$sims.list$beta8))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta8),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta8>0)/length(run.2021$BUGSoutput$sims.list$beta8),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta8),3))
       ))

# beta 9
hist(run.2021$BUGSoutput$sims.list$beta9, main ="male share 2021", xlab = "beta 9", xlim = c(min(run.2021$BUGSoutput$sims.list$beta9),max(run.2021$BUGSoutput$sims.list$beta9))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta9),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta9>0)/length(run.2021$BUGSoutput$sims.list$beta9),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta9),3))
       ))
par(mfrow = c(2,3))
# beta 10
hist(run.2021$BUGSoutput$sims.list$beta10, main = "married 2021", xlab = "beta 10", xlim = c(min(run.2021$BUGSoutput$sims.list$beta10),max(run.2021$BUGSoutput$sims.list$beta10))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta10),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta10>0)/length(run.2021$BUGSoutput$sims.list$beta10),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta10),3))
       ))
# beta 11
hist(run.2021$BUGSoutput$sims.list$beta11, main = "widowed 2021", xlab = "beta 11", xlim = c(min(run.2021$BUGSoutput$sims.list$beta11),max(run.2021$BUGSoutput$sims.list$beta11))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta11),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta11>0)/length(run.2021$BUGSoutput$sims.list$beta11),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta11),3))
       ))

# beta 12
hist(run.2021$BUGSoutput$sims.list$beta12, main = "divorced 2021", xlab = "beta 12", xlim = c(min(run.2021$BUGSoutput$sims.list$beta12),max(run.2021$BUGSoutput$sims.list$beta12))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta12),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta12>0)/length(run.2021$BUGSoutput$sims.list$beta12),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta12),3))
       ))
par(mfrow = c(2,3))
# beta 13
hist(run.2021$BUGSoutput$sims.list$beta13, main = "from20to39 2021", xlab = "beta 13", xlim = c(min(run.2021$BUGSoutput$sims.list$beta13),max(run.2021$BUGSoutput$sims.list$beta13))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta13),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta13>0)/length(run.2021$BUGSoutput$sims.list$beta13),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta13),3))
       ))
# beta 14
hist(run.2021$BUGSoutput$sims.list$beta14, main = "from40to65 2021", xlab = "beta 14", xlim = c(min(run.2021$BUGSoutput$sims.list$beta14),max(run.2021$BUGSoutput$sims.list$beta14))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta14),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta14>0)/length(run.2021$BUGSoutput$sims.list$beta14),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta14),3))
       ))

# beta 15
hist(run.2021$BUGSoutput$sims.list$beta15, main = "from65to79 2021", xlab = "beta 15", xlim = c(min(run.2021$BUGSoutput$sims.list$beta15),max(run.2021$BUGSoutput$sims.list$beta15))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta15),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta15>0)/length(run.2021$BUGSoutput$sims.list$beta15),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta15),3))
       ))
par(mfrow = c(2,3))            
# beta 16
hist(run.2021$BUGSoutput$sims.list$beta16, main = "80orolder 2021", xlab = "beta 16", xlim = c(min(run.2021$BUGSoutput$sims.list$beta16),max(run.2021$BUGSoutput$sims.list$beta16))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta16),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):', round(sum(run.2021$BUGSoutput$sims.list$beta16>0)/length(run.2021$BUGSoutput$sims.list$beta16),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta16),3))
       )) 
# beta 17
hist(run.2021$BUGSoutput$sims.list$beta17, main = "demographic pressure 2021", xlab = "beta 17", xlim = c(min(run.2021$BUGSoutput$sims.list$beta17),max(run.2021$BUGSoutput$sims.list$beta17))
)
abline(v = mean(run.2021$BUGSoutput$sims.list$beta17),lty = 2)
abline(v = 0,lty = 1,col = 'red')
legend('topright',
       legend = c(
         paste('P(beta>0):',round(sum(run.2021$BUGSoutput$sims.list$beta17>0)/length(run.2021$BUGSoutput$sims.list$beta17),3)),
         paste('mean:',round(mean(run.2021$BUGSoutput$sims.list$beta17),3))
       ))  




