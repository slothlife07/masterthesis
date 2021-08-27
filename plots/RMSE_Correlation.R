# plots rmse & correlation
plot(run.2010$BUGSoutput$mean$Y.pred,test.2010$VoteShare, ylim = c(0,0.5),
     xlim =  c(0,0.5), main = "Prediction Performance 2010", xlab = "Prediction", ylab ="Result")
abline(0,1)
legend("topleft",legend = c(paste("Correlation:",round(cor(test.2010$VoteShare,run.2010$BUGSoutput$mean$Y.pred), digits = 3)),paste("RMSE", round(sqrt(mean((test.2010$VoteShare-run.2010$BUGSoutput$mean$Y.pred)^2 )),digits = 3))))


plot(run.2012$BUGSoutput$mean$Y.pred, test.2012$VoteShare, ylim = c(0,0.5),
     xlim =  c(0,0.5), main = "Prediction Performance 2012", xlab = "Prediction", ylab ="Result")
abline(0,1)
legend("topleft",legend = c(paste("Correlation:",round(cor(test.2012$VoteShare,run.2012$BUGSoutput$mean$Y.pred), digits = 3)),paste("RMSE", round(sqrt(mean((test.2012$VoteShare-run.2012$BUGSoutput$mean$Y.pred)^2 )),digits = 3))))


plot(run.2017$BUGSoutput$mean$Y.pred, test.2017$VoteShare, ylim = c(0,0.5), xlim =  c(0,0.5), main = "Prediction Performance 2017", xlab = "Prediction", ylab ="Result")
abline(0,1)
legend("topleft",legend = c(paste("Correlation:",round(cor(test.2017$VoteShare,run.2017$BUGSoutput$mean$Y.pred),digits = 3)),paste("RMSE", round(sqrt(mean((test.2017$VoteShare-run.2017$BUGSoutput$mean$Y.pred)^2 )),digits = 3))))

# 2021

plot(run.2021$BUGSoutput$mean$Y.pred, test.2021$VoteShare, ylim = c(0,0.5),
     xlim =  c(0,0.5), main = "Prediction Performance 2021", xlab = "Prediction", ylab ="Result")
abline(0,1)
legend("topleft",legend = c(paste("Correlation:",round(cor(test.2021$VoteShare,run.2021$BUGSoutput$mean$Y.pred), digits = 3)),paste("RMSE", round(sqrt(mean((test.2021$VoteShare-run.2021$BUGSoutput$mean$Y.pred)^2 )), digits = 3))))