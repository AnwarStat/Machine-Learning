#sample from exponential distribution

set.seed(20)
u<-runif(20,0,1)
u
x<--(1/2)*(log(1-u))
x
rexp(20,rate=2)

#random sample from uniform distribution

set.seed(2)
#f(x)=1/theta ; 0<x<theta
#F(x)=x/theta ;0<x<theta
#x=F(x)*theta , where F(x)~U(0,1)

FX<-runif(25,min = 0,max = 1)
FX
theta<-5
gen<-FX*theta
gen
set.seed(2)
org.sample<-runif(25,min = 0,max = theta)
org.sample
abs(org.sample-gen)


#draw a random sample of size 10 from binomial distribution with size,n=12, probability, P=.7
n<-12
p<-.7
i<-0:n
u<-runif(10,min=0,max=1)
round(u,3)
CF<-pbinom(q=i,size=n,prob=p)
round(CF,3)
Y<-rep(0,10) 
for (j in 1:10){
  for (k in 1:13){
    if(u[j]<=CF[k]){
      Y[j]<-Y[j]
    }else{Y[j]<-k
    }
  }
}
Y

for (j in 1:10){
  for (k in 1:13){
    if(u[j]>CF[k]){
      Y[j]<-k
    }else{Y[j]<-Y[j]
    }
  }
}

#draw a random sample of size 20 from a poisson distribution
i<-0:100
u<-runif(20,min=0,max=1)
round(u,3)
CF<-ppois(q=i,lambda = 7)
round(CF,3)
Y<-rep(0,20) 
for (j in 1:20){
  for (k in 1:101){
    if(u[j]>CF[k]){
      Y[j]<-k
    }else{Y[j]<-Y[j]
    }
  }
}
Y
rpois(20,7)
#Random sample of size 20 from following distributions

sample(x=c(0,1),size=20,replace = T,prob = c(.4,.6)) #Bernoulli But Confused

rgeom(n=20,prob=.4) #Geometric Dist.

rnbinom(n=20,size=12,prob=.4) #Negative Binomial Dist.

rgamma(n=20,shape=2,rate=4) #Gamma Dist.(alpha=2,beta=4)

rbeta(n=20,shape1 = 2,shape2 = 4) #Beta Dist. 1st kind(u=2,v=4)

rnorm(n=20,mean = 0,sd=1) #Normal Dist.N(0,1)

rlnorm(n=20,meanlog = 0,sdlog = 1) #Log-Normal Dist.

?rgamm


#Central Limit Theorem.

a<-replicate(12,rbinom(20,1,.5))

#From Bernoulli Distribution
x1<-replicate(4000,sample(1:6,50,replace = T))
x2<-x1==6

x<-colMeans(x2)

z<-(x-1/6)/sqrt((1/6)*(5/6)/50)

y<-abs(z)<1
mean(y)
y<-abs(z)<2
mean(y)
y<-abs(z)<3
mean(y)

#install.packages("purrr")
library(purrr)
rdunif(10,12,14)
par(mfrow=c(2,1))



check.Nomr<-function(z){
 y1<-abs(z)<1
 y1<-mean(y1)
 y2<-abs(z)<2
 y2<-mean(y2)
 y3<-abs(z)<3
 y3<-mean(y3)
 print(paste("Within One SD=",paste0(round(y1*100,2),"%")
             ,",   Within Two SD=",paste0(round(y2*100,2),"%"),
             ",   Within Three SD=",paste0(round(y3*100,2),"%")))
}
#Descrete Uniform
set.seed(1)
x<-replicate(2000,rdunif(n=50,b=10,a=1))
xvar<-colMeans(x)
z<-(xvar-(11/2))/sqrt(((10^2-1)/12)/50)
check.Nomr(z)

#Exponential(2)
set.seed(1)
x<-replicate(1000,rexp(30,rate = 1/2))
xvar=colMeans(x)
z<-(xvar-2)/sqrt(4/30)
check.Nomr(z)
?hist
#For small sample

set.seed(1)
x<-replicate(10000,rnorm(10,2,4))
xvar<-apply(x,2,mean)
s<-apply(x,2,sd)
t<-(xvar-2)/(s/sqrt(10))  #When Variance Unknown
z<-(xvar-2)/sqrt(16/10)   #When Variance Known
check.Nomr(z)
check.Nomr(t)

round(pt(-5:5,9),5)
x<-c(mean(t<(-5)),mean(t<(-4)),mean(t<(-3)),mean(t<(-2)),mean(t<(-1)),
     mean(t<0),mean(t<1),mean(t<2),mean(t<3),mean(t<4),mean(t<5))
round(x,5)


#For large sample

set.seed(1)
x<-replicate(10000,rnorm(40,2,4))
xvar<-apply(x,2,mean)
s<-apply(x,2,sd)
t<-(xvar-2)/(s/sqrt(40))  #When Variance Unknown
z<-(xvar-2)/sqrt(16/40)   #When Variance Known
check.Nomr(z)
check.Nomr(t)

round(pt(-5:5,39),5)
x<-c(mean(t<(-5)),mean(t<(-4)),mean(t<(-3)),mean(t<(-2)),mean(t<(-1)),
    mean(t<0),mean(t<1),mean(t<2),mean(t<3),mean(t<4),mean(t<5))
round(x,5)

x<-replicate(1200,rnorm(1,2,4))
z<-(x-2)/0
z

set.seed(1)
X<-replicate(4000,rf(20,df1=5,df2=150))
Chi<-5*X
round(pchisq(seq(0,20,4),5),3)
x<-c(mean(Chi<0),mean(Chi<4),mean(Chi<8),mean(Chi<12),mean(Chi<16),mean(Chi<20))
round(x,3)

X<-replicate(4000,rf(20,df1=150,df2=5))
Chi<-5*X
round(pchisq(seq(0,20,4),5),3)
x<-c(mean(Chi<0),mean(Chi<4),mean(Chi<8),mean(Chi<12),mean(Chi<16),mean(Chi<20))
round(x,3)
