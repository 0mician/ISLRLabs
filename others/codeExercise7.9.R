library(MASS) 
library(boot) 
library(splines) 
attach(Boston)
#9a
r=range(dis)
disgrid=data.frame(dis=seq(r[1],r[2],0.1)) 
cub = lm(nox~poly(dis,3,raw=TRUE),data=Boston)
summary(cub)
plot(dis,nox,pch=20,col="grey")
lines(t(disgrid),predict(cub,newdata=disgrid))

predcub = predict(cub,newdata=disgrid,se=T) 
plot(dis,nox,xlim=r,cex=.5,col="darkgrey",pch=20)
title("Cubic polynomial regression") 
lines(disgrid$dis,predcub$fit,lwd=2,col="blue")
lines(disgrid$dis,predcub$fit+2*predcub$se.fit,lwd=2,col="blue",lty=2)
lines(disgrid$dis,predcub$fit-2*predcub$se.fit,lwd=2,col="blue",lty=2)

#9b

#polynomial RSS 
rss = rep(0,10)
plot(dis,nox,pch=20,col="grey")
 
for (i in 1:10){
	fit = lm(nox~poly(dis,i),data=Boston) 
	pred = predict(fit,Boston,se=T)
	rss[i] = sum((nox-pred$fit)^2)
	lines(t(disgrid),predict(fit,newdata=disgrid),col=i)
}
legend("topright",legend=1:10,col=1:19,lty=1)
plot(rss,type="b",xlab="Polynomial degree",ylab="RSS") 
title("RSS versus polynomial degree")

#9c

cverr = rep(0,10) 
set.seed(200)
for (i in 1:10){
	fit = glm(nox~poly(dis,i),data=Boston)
	cverr[i] = cv.glm(Boston,fit,K=10)$delta[1] 
}

plot(cverr,type="b",xlab="Polynomial degree",ylab="CV error") 
title("Cross validation error")
which.min(cverr)

#9d
attr(bs(dis,df=4),"knots")
median(dis)
regspl = lm(nox~bs(dis,df=4),data=Boston)
summary(regspl)
rspred = predict(regspl,newdata=disgrid,se=T) 
plot(dis,nox,col="grey",pch=20)
abline(v=attr(bs(dis,df=4),"knots"),lty=2,col="black")
lines(t(disgrid),rspred$fit,lwd=2)
title("Regression spline (4df)")
regspl2 = lm(nox~bs(dis,knots=c(5)),data=Boston)
summary(regspl2)
rspred2 = predict(regspl2,newdata=disgrid,se=T) 
abline(v=5,col="blue",lty=2)
lines(t(disgrid),rspred2$fit,lwd=2,col="blue")


#9e 
#regression spline RSS 
rss2 = rep(0,10)
plot(dis,nox,pch=20,col="grey")
for (i in 3:12){
	fit = lm(nox~bs(dis,df=i),data=Boston) 
	pred = predict(fit,Boston,se=T)
	rss2[i-2] = sum((nox-pred$fit)^2)
	lines(t(disgrid),predict(fit,newdata=disgrid),col=i)
	
}
legend("topright",legend=3:12,col=1:19,lty=1)
plot(3:12,rss2,type="b",xlab="Degrees of freedom",ylab="RSS",pch=19) 
title("RSS for cubic regression spline over increasing df")

cverr2 = rep(0,13) 
set.seed(10)
for (i in 3:15){
	fit = glm(nox~bs(dis,df=i),data=Boston)
	cverr2[i-2] = cv.glm(Boston,fit,K=10)$delta[1] 
}
plot(3:15,cverr2,type="b",xlab="Degrees of freedom",ylab="CV error",pch=19)
title("CV error Cubic Regression Spline")
which.min(cverr2)+2
