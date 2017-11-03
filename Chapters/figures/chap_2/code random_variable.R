# Children's IQ scores are normally distributed with a
# mean of 100 and a standard deviation of 15. What
# proportion of children are expected to have an IQ between
# 80 and 120?

mean=100; sd=15
lb=80; ub=120

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="IQ Values", ylab="",
     main="", axes=F, ylim=c(0,0.035))

i <- x >= lb & x <= ub
lines(x, hx)
#polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))
#mtext(result,3)
axis(1, at=seq(40, 160, 20), pos=0) 
axis(2, at=seq(0, 0.04, 0.005), pos=40) 

# create sample data
set.seed(12)
sample.x <- rnorm(20, mean=100, sd=15)

library(MASS)
fit<-fitdistr(sample.x, "normal") 
mean=fit$estimate[1]; sd=fit[1]$estimate[2]

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)
#plot(x, hx, type="n", xlab="IQ Values", ylab="",
#     main="", axes=F)
lines(x, hx, col="red")

# create sample data
set.seed(12)
sample.x <- rnorm(200, mean=100, sd=15)

library(MASS)
fit<-fitdistr(sample.x, "normal") 
mean=fit$estimate[1]; sd=fit[1]$estimate[2]

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)
#plot(x, hx, type="n", xlab="IQ Values", ylab="",
#     main="", axes=F)
lines(x, hx, col="blue")


d <- density(sample.x) # returns the density data
#plot(d) # plots the results 
plot(d, type="n", xlab="IQ Values", ylab="",
     main="", axes=F)
points(sample.x)
lines(d)
axis(1, at=seq(40, 160, 20), pos=0) 
axis(2, at=seq(0, 0.04, 0.005), pos=40) 

