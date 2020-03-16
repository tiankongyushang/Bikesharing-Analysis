bike<-read.table("bikeshare.txt", header=TRUE,sep=",")
bikeNew=bike[,c(3,4,7,8,10,11,13,14,17)]
summary(bikeNew)
bikesummary=bikeNew[,c(6,7,8,9)]
summary(bikesummary)
cor(bikeNew)
attach(bikeNew)
seasonnew=season-1
holidaynew=holiday-1
weathersitnew=weathersit-1
lm1=lm(cnt~seasonnew+yr+holidaynew+weekday+weathersitnew+temp+hum+windspeed)
install.packages("leaps")
require(leaps)
subset=regsubsets(cnt~seasonnew+yr+holidaynew+weekday+weathersitnew+temp+hum+windspeed,method="exhaustive",nbest=1,data=bikeNew)
sum_subset=summary(subset)
sum_subset$which
p_full=9
p=2:p_full
RSS_p=sum_subset$rss
totalSS=sum((cnt-mean(cnt))^2)
R2_p=1-RSS_p/totalSS
R2_p
plot(p,R2_p,xlab="Number of betas",ylab="R-squared")
RSS_p
n=nrow(bikeNew)
n
R2_adj=1-(RSS_p/(n-p))/(totalSS/(n-1))
R2_adj
plot(p,R2_adj,xlab="Number of betas",ylab="Adjusted R-squared")
lm_full=lm(cnt~seasonnew+yr+holidaynew+weekday+weathersitnew+temp+hum+windspeed)
sigma_hat_full=summary(lm_full)$sigma
C_p=RSS_p/(sigma_hat_full^2)+2*p-n
C_p
plot(p,C_p,xlab="Number of betas",ylab="Mallow's Cp")
abline(0,1)
aic_p=n*log(RSS_p/n)+2*p
aic_p
plot(p,aic_p,xlab="Number of betas",ylab="AIC")
bic_p=n*log(RSS_p/n)+p*log(n)
bic_p
plot(p,bic_p,xlab="Number of betas",ylab="BIC")
cbind(sum_subset$which,R2_adj,C_p,aic_p,bic_p)