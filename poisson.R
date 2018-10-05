nplan<-1000
lambda=5
frutos<-rpois(nplan,lambda)
frutos
par(mfrow=c(1,2))
plot(dbinom(seq(1,50, by  =1),  size  =50,  prob  =  0.09),  type ="h", ylab = "Probabilidade", main = "Distribuição Binomial") 
plot(dpois(seq(1,50, by =1), lambda = 50*0.09), type ="h", ylab = "Probabilidade", main = "Distribuição Poisson") 

library(bbmle); library(car); library(sads)
set.seed(1234) 
nplan<-1000 
lambda=5 
frutos<-rpois(nplan,lambda) 
frutos
dpois(0,lambda) 
sum(frutos==0)/length(frutos) 
mfrut<-max(frutos) 
fa<-factor(frutos, levels=0:mfrut) 
prob.obs<-table(fa)/nplan 
par(las=1) 
plot(0:mfrut,prob.obs, xlab="Numero de frutos",ylab="Probabilidade", type="h", lwd=5) 
prob.tr<-dpois(0:mfrut, lambda) 
points(0:mfrut,prob.tr, pch=21, col="red") 



dados <- c(24, 27, 23, 28, 26, 24, 17, 23, 24, 27)
a<- as.data.frame( sapply(dados, dpois, 16), row.names = NULL, optional = FALSE); 
names(a) <- "p"
prod(a)


# week on = 1, week off = 0
week.status <- c(1, 1, 0, 0)
calls <- c(2, 6, 2, 3)
model <- glm(calls ~ week.status, family = poisson())
model
# or change the poisson() after family to quasipoisson() 
# or use the neg binomial glm from the MASS package



getEventProb(nrolls = 1,
             ndicePerRoll = 2,
             nsidesPerDie = 4,
             eventList = list(4, 3, c(1,2)),
             orderMatters = FALSE)

