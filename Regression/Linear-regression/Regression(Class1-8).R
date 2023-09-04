require(datasets)
datasets::attitude
str(attitude)
library(ggplot2)
ggplot(data=attitude,mapping = aes(x=rating,y=complaints))+geom_abline()+geom_smooth()
qplot(rating,complaints,data=attitude)+geom_abline()
#1st class
#simple linear regrgeom_smooth()

x<-c(3,3,5,6,8,8)
y<-c(1.4,1.5,2.2,2.4,2.8,3.2)
fm<-lm(y~x)
fm
s<-summary(fm)
s
plot(x,y)
abline(fm)
e<-s$residuals
e<-resid(fm)
e<-round(e,3)
e
a<-anova(fm)
a
qqnorm(e)
rsq<-s$r.squared
ad.rsq<-s$adj.r.squared
rsq
ad.rsq
b0<-s$coefficients[1,1]
b1<-s$coefficients[2,1]
b0
b1
n<-length(x)
mse<-sum(resid(fm)^2)/(n-2)
mse
s
sqrt(mse)
s$sigma
s$residuals
standard.error.beta1<-s$sigma/sqrt(sum((x-mean(x))^2))
standard.error.beta1
se.bata0<-s$sigma*sqrt(1/n+mean(x)/sum((x-mean(x))^2))
se.bata0
se<-sqrt(mse*((1/n)+(x-mean(x))^2/sum((x-mean(x))^2)))
se
sum(se)/length(x)
t<-qt(.025,df=(n-2),lower.tail=F)
predict(fm,interval="confidence",level=.95)
predict(fm)+t*se
predict(fm)
confint(fm,level = .95)
mean(y)+qt(.025,n-2,lower.tail = F)*se

#2nd class
#multiple linear regression
head(trees)
fm<-lm(Volume~Girth+Height,data=trees)
fm
s<-summary(fm)
s
anova<-anova(fm)
anova
e<-round(resid(fm),3)
e
v<-trees$Volume
plot(v,e)
r.sq<-s$r.squared
r.sq
ad.r<-s$adj.r.squared
CI<-predict(fm,interval = "confidence",level = .95)
CI
predicted<-predict(fm)
confint(fm,inteval="confidence",level=.95)
new<-data.frame(Girth<-c(2,3,4),Height<-c(5,7,8))
predict(fm,newdata=new,interval="confidence")
predict(fm,newdata = new,interval='confidence',level = .99)
fcal<-anova$F
fcal
f<-qf(.05,2,length(v)-3,lower.tail = F)
f
for(i in 1:2){
  fc<-fcal[i]
  if(fc>f){
    print("H0 is rejected ")
  }else{print("H0 is not rejected")}}
#model adequacy check
#kolmogorov-smirnov test

k<-ks.test(e,"pnorm",mean<-mean(e),sd<-sd(e))
k
p<-1-k$p.value
p
if(p<.05){
  print("H0 is rejected")
}else{print("H0 is not rejected")}
qqnorm(e)
lines(e)
abline(a=0,b=2.99)
#3rd class
#polynomial regression
#R cookbook
#The R Book-Wiley publication
#polynomial y=b0+b1x+b2x^2+....+c
y<-trees$Volume
x1<-trees$Height
x2<-trees$Girth
fm<-lm(y~poly(x2,degree=4))
lm(y~poly(x2,degree = 5))
fm
s<-summary(fm)
s
plot(y~x2)
lines(x2,y,col='red')
plot(y~scale(x2),col="red",main="polynomial line",xlab="fitted value",ylab="Volume")
lines(y~scale(x2),data=trees,col="green")  

#multicollinearity
set.seed(100)
x1<-rgamma(100,2)
rgamma(n=5,shape=2,scale = 2)
x2<-rgamma(100,2)
x3<-x1+2*x2+rnorm(100,0,1)
y<-4*x1-3*x2+2*x3+rnorm(100,0,1)
testdata<-data.frame(y,x1,x2,x3)
multinom<-lm(y~x1+x2+x3,data=testdata)
multinom
library(DAAG)
library(DAAG)
vif<-vif(multinom)
vif
i<-1
for (i in 1:3){
  v<-vif[i]
  if(v<2){
    print(paste(v,": They are not correlated"))
  }else{if(v<6){
    print(paste(v,":  moderately correlated"))
  }else{if(v<11){
    print(paste(v,":  highly correlated"))
  } else{print(paste(v,":  seriously correlated"))}
  }
  }
}

#4th class
# non linear model

#non linear model 
#estimale the parameter theta from the following non linear model 
#y=e^-theta*x+e
x<-c(1,4,16)
y<-c(.8,.45,.04)
non.model<-nls(y~exp(-theta*x),start=list(theta=.5))
nls(y~exp(-theta*x),start= list(theta=.4))
non.model
model<-function(x,y,theta){
  exp(-theta*x)}

non.model<-nls(y~model(x,y,theta),start=list(theta=.5))
s<-summary(non.model)
s
predict(non.model)
confint(non.model)
theta<-s$parameters[1,1]
theta
se.theta<-s$parameters[1,2]
se.theta
z.val<-qnorm(.025,0,1,lower.tail = FALSE)
up.ci<-theta+z.val*se.theta
up.ci
lw.ci<-theta-z.val*se.theta
lw.ci


#y=a-b*e^-cx
x<-runif(100,0,30)
y<-120-110*exp(-.05*x)+rnorm(100,0,1)
fm<-nls(y~a-b*exp(-c*x),start=list(a=100,b=60,c=.04))
s<-summary(fm)
s

#y=theta1+(.49+theta1)*exp(-theta2*(x+theta2))
x<-c(1,4,16)
y<-c(.8,.45,.04)
fm<-nls(y~(theta1+(.49+theta1)*exp(-theta2*(x+theta2))),start=list(theta1=.01,theta2=.5))

s<-summary(fm)
s

#5th class
#05\01\2020
#maximum likelihood estimation
#for normal distribution
est<-function(x,theta){
  
  maxlin<-function(x,theta){
    n<-length(x)
    mu<-theta[1]
    sigma<-theta[2]
    F<-(1/(sqrt(2*pi)*sigma))*exp((-1/(2*sigma^2))*(x-mu)^2)
    L<-prod(F)
    logL<-log(L)
    #logL<--(n/2)*log(2*pi*sigma^2)-(1/2/sigma^2)*sum((x-mu)^2)
    return(-logL)
  }
  #maximize
  est<-optim(theta,x=x,maxlin,method="BFGS",hessian=TRUE)
 
  return(est)
}

x<-c(1,2,3)
theta<-c(1,2)
est<-est(x,theta)
est

par<-est$par
mu<-par[1]
sigma<-par[2]
sigma2<-sigma^2
par
mu
sigma
sigma2

#for binomial distribution

est<-function(x,theta){
  
  maxlin<-function(x,theta){
    n<-theta[1]
    p<-theta[2]
    L<-prod(choose(n,x)*p^x*(1-p)^(n-x))
    logL<-sum(log(choose(n,x))+sum(x)*log(p)+sum((n-x))*log(1-p))
    return(-logL)
  }
  #maximize
  est<-optim(theta,x=rbinom(100,20,.4),maxlin,method="BFGS",hessian=TRUE)
  return(est)
}
est
x<-c(6,7,3,6,8,5,6,7)
p<-.4
n<-40
theta<-c(n,p)

est(x,theta)
est
est<-function(x,theta){
  maxli<-function(x,theta){
    n<-theta[1]
    p<-theta[2]
    fx<-choose(n,x)*(p^x)*((1-p)^(n-x))
    likelihood<-prod(fx)
    loglikelihood<-log(likelihood,base=exp(1))
    return(-loglikelihood)
  }
  #maximize
  mle<-optim(theta,x,fn=maxli,method="BFGS",hessian=TRUE)
  return(mle) 
  
}
theta<-c(20,.7)
t<-est(x=rbinom(20,15,.4),theta=theta)
summary(t)
anova(t)

#6th class

#multiple linear regression midel 
est<-function(y,x,theta){
  y<-as.matrix(y)
  x<-cbind(1,x)
  n<-nrow(x)
  k<-ncol(x)
  p<-k+1
  
  maxlink<-function(y,x,theta=rep(2,p)){
    beta<-theta[1:k]
    sigma<-theta[k+1]
    e<-y-x%*%beta
    logL<-.5*n*log(2*pi)-.5*n*log(sigma)-((t(e)%*%e)/(2*sigma))
    return(-logL)
  }
  
  est<-optim(par = theta,fn=maxlink,x=x,y=y,method="BFGS",hessian=T)
  return(est)
  
}

x1<-c(24,24,14,43,70,47,35,25,62,12)
x2<-12:21
x<-cbind(x1,x2)
x


y<-c(23,33,53,63,32,12,53,90,80,35)
theta<-c(1,1,1,1)
fm<-est(y,x,theta)
fm
covar<-fm$hessian
var<-diag(covar)
sigma<-var[3]
lm(y~x1+x2)
st.sigma<-sqrt(var[3])
z.val<-qnorm(.025,lower.tail=F)
up<-sigma+z.val*st.sigma
up
lw<-sigma-z.val*st.sigma
lw
  

#7th class

#logistic regression
no.yes<-c("No","Yes")
smoking<-gl(2,1,1000,no.yes)
smoking
sm<-as.logical(smoking)
obesity<-gl(2,2,1000,no.yes)
obesity
snoring<-gl(2,4,1000,no.yes)
snoring
n.hyp<-rbinom(1000,500,.2)
n1.total<-rbinom(500,2000,.4)
n2.total<-rbinom(500,2000,.5)
n.total<-c(n1.total,n2.total)

hyp.res<-cbind(n.hyp,n.total-n.hyp)
hyp.res
data.f<-data.frame(smoking,obesity,snoring,n.hyp,n.total,hyp.res)
str(data.f)
m1<-glm(hyp.res~smoking+obesity+snoring,family = binomial(link = "logit"))
m1
summary(m1)
plot(data.f)

library(ggplot2)
qplot(n.hyp,smoking,data=data.f,color=snoring,geom=c("point","smooth"))
qplot(rating,complaints,data=attitude,color=rating)+geom_abline()+geom_smooth()
names(attitude)
new<-data.frame(probability.of.hyp=m1$fitted.values,n.hyp)
new<-new[order(new$probability.of.hyp,decreasing = F),]
new
new$rank<-1:nrow(new)
library(ggplot2)
library(cowplot)
ggplot(data=new,aes(x=rank,y=probability.of.hyp))
geom_point(aes(color=hyp.res),alpha=1,shape=4,stroke=2)
xlab("index")
ylab("predicted probability of hypertension")
ggsave("hypertension probability2.pdf")


#8th class

#spiline regression/piecewise regression
library(splines)
library(Ecdat)
data("Clothing")
str(Clothing)
data("iris3")
install.packages("TH.data")
data("bodyfat",package = "TH.data")
data("rivers")
str(iris3)
plot(iris3)
model<-lm(Sepal.Length ~bs(Petal.Length,knots=c(4.2,5.3,6.4)),data=iris)
model
plot(model)
plot(iris$Petal.Length, iris$Sepal.Length)
x<-attitude$rating
quantile(x,p=c(.25,.5,.75))

knots <- quantile(iris$Petal.Length , p = c(0.25, 0.5, 0.75))

model<-lm(Sepal.Length~bs(Petal.Length,knots=knots),data=iris)
  summary(model)
model
iris
ggplot(iris, aes(Petal.Length,Sepal.Length )) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3)) 

model1<-lm(tsales~bs(inv2,knots = quantile(inv2,p=c(.25,.5,.75))),data = Clothing)
model1

plot(Clothing)
plot(Clothing$inv2,Clothing$tsales)
ggplot(Clothing, aes(inv2,tsales ) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3)) 
plot(model1)

