# convergence plots

plot(run.2010$BUGSoutput$summary[,"Rhat"],ylim = c(0.95,1.15), main = "Convergence run 2010", ylab = "Value", xlab = "Iterations" )
abline(h = 1.1,col = 'green')

plot(run.2012$BUGSoutput$summary[,"Rhat"],ylim = c(0.95,1.15), main = "Convergence run 2012", ylab = "Value", xlab = "Iterations" )
abline(h = 1.1,col = 'green')

plot(run.2017$BUGSoutput$summary[,"Rhat"],ylim = c(0.95,1.15), main = "Convergence run 2017", ylab = "Value", xlab = "Iterations" )
abline(h = 1.1,col = 'green')

plot(run.2021$BUGSoutput$summary[,"Rhat"],ylim = c(0.95,1.15), main = "Convergence run 2021", ylab = "Value", xlab = "Iterations" )
abline(h = 1.1,col = 'green')