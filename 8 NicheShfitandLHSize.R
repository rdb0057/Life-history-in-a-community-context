rm(list=ls())
library('arm')
library('lattice')
library('latticeExtra')
require("reshape2")
require("gridExtra")
require('plyr')
# source("~./R/packages.R")

user <- Sys.info()['user']
directory <- paste("C:/Users/",user,"/Box/Bassar Docs/Science/Projects/Current/Niche Shifts and the Life History/Model Code/For Publication",sep='')
setwd(directory)



################################################################################
#Resource Parameters
################################################################################

#number of mesh points
nR <- 100

#maximum and minimum trophic levels
minT <- 0; maxT <- 100;

#boundary points b and mesh points y
bR <- minT + c(0:nR) * (maxT - minT) /nR;
yR <-  0.5*(bR[1:nR] + bR[2:(nR+1)]);    #midpoints (i.e. height)
hR <- (maxT - minT) / nR      #width

#Resource parameters
res.growth <- function(yR,uniform){
  K.res <- 26*exp(-0.01*(yR-(maxT/2)))
  r.res <- 0.82*exp(-0.01*(yR-(maxT/2)))

  if(uniform==T){
    K.res[] <- mean(K.res)
    r.res[] <- mean(r.res)
  }
  return(list(r.res, K.res))
}

r.res <- res.growth(yR=yR,uniform=F)[[1]]
K.res <- res.growth(yR=yR,uniform=F)[[2]]


#===============================================================================
#Set sizes for consumer
#===============================================================================
#Number of Meshpoints
n=100 

#This sets the maximum and minimum sizes for the analysis
minsize<-2; maxsize<-40; 
L<-minsize; U<-maxsize;

# boundary points b and mesh points y
b<-L+c(0:n)*(U-L)/n;
y<-0.5*(b[1:n]+b[2:(n+1)]);
h <- (U-L)/n


source("./9 popdynfuncSize.R")


#Default parameters
#Niche and feeding parameters
phi.int = -0.25#0.1#-0.05,                          #Consumer feeding rate at size at birth
phi.type = seq(0,0.05,length.out=2)                 #slope of consumer feeding rate with size
sum.params = 10                                     #Scaling term, do not change
mean.niche = 20                                #Mean of the niche 1-100
rho.type = seq(0,0.20,length.out=2)                 #Slope of niche change with size of consumer

#Conversion of resources to consumers
delta = 0.15                                        #Conversion efficiency
all.R.equal = F                                     #If T, then all resources benefit the consumer the same way, scaled by 0.75
beta.bene = 0.01
beta.DC = 0.025         


#Consumer Life History parameter
eta.z = 0.5                                        #Slope of proportion of resources devoted towards maintenance/survival
psi.0 = 0                                           #Intercept of growth fecundity trade-off
psi.z = -0.05                                       #Slope of growth fecundity tradeoff
psi.mat = seq(6.5,30,length.out=75)                  #Size at Maturity

#Other parameters
beta.d = 6.5                                        #Mean offspring size
d.var = 0.25                                        #variance in offspring size
beta.s = rep(mean(-4.5 + 0.01 * (y-6)),n)               #Background survival
kappa.s = 5*((1/y)^(1/10))                           #scales survival units to resource units
kappa.g = 0.03#300                                       #scales growth and reproduction to resource units
kappa.m = (1/beta.d)
g.var = 0.25                                         #Variance in growth
min.surv <- 0.5    
                                    #Include indirect effects on community
years = 20000                                       #Years to run

                                 #minimum allocation to survival


################################################################################
#no pred
################################################################################
#Analysis specific parameters
pred = 0                                   #predator population starting sizes
size.dep.pred = F                          #If true then predation is size dependent


#Substitutable
all.R.equal = T                                     #If T, then all resources benefit the consumer the same way, scaled by 0.75
r.res <- res.growth(yR=yR,uniform=T)[[1]]
K.res <- res.growth(yR=yR,uniform=T)[[2]]

source("./10 Run Model Size.R")
source("./11 Invasion Figures Size.R")
#save the invasion growth rates for later
invasion.base <- invasion
maximum11.base <- maximum11
maximum21.base <- maximum21
maximum12.base <- maximum12
maximum22.base <- maximum22
S.eq.base <- S.eq
lamb.inv.df1 <- lamb.inv.df


#Not Substitutable, different benefit
all.R.equal = F                                     #If T, then all resources benefit the consumer the same way, scaled by 0.75
r.res <- res.growth(yR=yR,uniform=T)[[1]]
K.res <- res.growth(yR=yR,uniform=T)[[2]]

source("./10 Run Model Size.R")
source("./11 Invasion Figures Size.R")
#save the invasion growth rates for later
lamb.inv.df2 <- lamb.inv.df
maximum11.2 <- maximum11
maximum21.2 <- maximum21
maximum12.2 <- maximum12
maximum22.2 <- maximum22
S.eq.2 <- S.eq


#Not Substitutable, different abundance
all.R.equal = T                                     #If T, then all resources benefit the consumer the same way, scaled by 0.75
r.res <- res.growth(yR=yR,uniform=F)[[1]]
K.res <- res.growth(yR=yR,uniform=F)[[2]]

source("./10 Run Model Size.R")
source("./11 Invasion Figures Size.R")
#save the invasion growth rates for later
lamb.inv.df3 <- lamb.inv.df
maximum11.3 <- maximum11
maximum21.3 <- maximum21
maximum12.3 <- maximum12
maximum22.3 <- maximum22
S.eq.3 <- S.eq



#Not Substitutable, different benefit and abundance
all.R.equal = F                                     #If T, then all resources benefit the consumer the same way, scaled by 0.75
r.res <- res.growth(yR=yR,uniform=F)[[1]]
K.res <- res.growth(yR=yR,uniform=F)[[2]]

source("./10 Run Model Size.R")
source("./11 Invasion Figures Size.R")
#save the invasion growth rates for later
lamb.inv.df4 <- lamb.inv.df
maximum11.4 <- maximum11
maximum21.4 <- maximum21
maximum12.4 <- maximum12
maximum22.4 <- maximum22
S.eq.4 <- S.eq




#Alternative Figure
xlab <- list(label='Size at Maturity Resident',cex=0.5)
ylab <- list(label='Size at Maturity Invader',cex=0.5)
source("./12 Invasion Figures Size 2.R")
jpeg(file = "./Figures/Figure S2.jpeg",width = 9, height = 7, units='in', res=600)

grid.arrange(plot11,plot12,plot13,plot14,
             plot21,plot22,plot23,plot24,
             plot31,plot32,plot33,plot34,
             plot41,plot42, plot43,plot44,
             nrow=4,ncol=4,
             top="\n",
             left="\n")

panel.text(2900, 150, expression("Organismal interactions with resource"), cex = 1, font = 1)

panel.text(950, 300, expression(phi==0~rho==0), cex = 1, font = 1)
panel.text(2200, 300, expression(phi==0.05~rho==0), cex = 1, font = 1)
panel.text(3500, 300, expression(phi==0~rho==0.20), cex = 1, font = 1)
panel.text(4800, 300, expression(phi==0.05~rho==0.20), cex = 1, font = 1)

panel.text(100, 2000, expression(Resource~properties), cex = 1, font = 1,srt=90)
panel.text(200, 650, expression(Substitutable), cex = 1, font = 1,srt=90)
panel.text(200, 1600, expression(Diff~B), cex = 1, font = 1,srt=90)
panel.text(200, 2600, expression(Diff~K), cex = 1, font = 1,srt=90)
panel.text(200, 3600, expression(Diff~K~and~B), cex = 1, font = 1,srt=90)

panel.text(550, 450, expression("A)"), cex = 1, font = 1)
panel.text(1800, 450, expression("B)"), cex = 1, font = 1)
panel.text(3100, 450, expression("C)"), cex = 1, font = 1)
panel.text(4350, 450, expression("D)"), cex = 1, font = 1)

panel.text(550, 1425, expression("E)"), cex = 1, font = 1)
panel.text(1800, 1425, expression("F)"), cex = 1, font = 1)
panel.text(3100, 1425, expression("G)"), cex = 1, font = 1)
panel.text(4350, 1425, expression("H)"), cex = 1, font = 1)

panel.text(550, 2400, expression("I)"), cex = 1, font = 1)
panel.text(1800, 2400, expression("J)"), cex = 1, font = 1)
panel.text(3100, 2400, expression("K)"), cex = 1, font = 1)
panel.text(4350, 2400, expression("L)"), cex = 1, font = 1)

panel.text(550, 3375, expression("M)"), cex = 1, font = 1)
panel.text(1800, 3375, expression("N)"), cex = 1, font = 1)
panel.text(3100, 3375, expression("O)"), cex = 1, font = 1)
panel.text(4350, 3375, expression("P)"), cex = 1, font = 1)

dev.off()


jpeg(file = "./Figures/Figure 8.jpeg",width = 7, height = 9, units='in', res=600)

ylab <- 'Survival (S(a))'
xlab <- 'Body size (z)'
ylim <- c(0.6,1)

par(mfrow=c(4,3),mar = c(4,4,1,2),cex=1)
plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab=ylab,xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.base[maximum21.base,2,1,,])~y,col='blue',type='p',pch=16)
title(main="A)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.base[maximum12.base,1,2,,])~y,col='blue',type='p',pch=16)
title(main="B)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.base[maximum22.base,2,2,,])~y,col='blue',type='p',pch=16)
points(diag(S.eq.base[1,2,2,,])~y,col='orange',type='p',pch=16)
title(main="C)",adj = 0.1, line = 0,cex=0.5)




plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab=ylab,xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.2[maximum21.2,2,1,,])~y,col='blue',type='p',pch=16)
title(main="D)",adj = 0.1, line = 0,cex=0.5)


plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.2[maximum12.2,1,2,,])~y,col='blue',type='p',pch=16)
title(main="E)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.2[maximum22.2,2,2,,])~y,col='blue',type='p',pch=16)
title(main="F)",adj = 0.1, line = 0,cex=0.5)




plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab=ylab,xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.3[maximum21.3,2,1,,])~y,col='blue',type='p',pch=16)
title(main="G)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.3[maximum12.3,1,2,,])~y,col='blue',type='p',pch=16)
title(main="H)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.3[maximum22.3,2,2,,])~y,col='blue',type='p',pch=16)
title(main="I)",adj = 0.1, line = 0,cex=0.5)



plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab=ylab,xlab=xlab,col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.4[maximum21.4,2,1,,])~y,col='blue',type='p',pch=16)
title(main="J)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab='',xlab=xlab,col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.4[maximum12.4,1,2,,])~y,col='blue',type='p',pch=16)
title(main="K)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~y,ylab='',xlab=xlab,col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.4[6,2,2,,])~y,col='blue',type='p',pch=16)
points(diag(S.eq.4[maximum22.4,2,2,,])~y,col='orange',type='p',pch=16)
title(main="L)",adj = 0.1, line = 0,cex=0.5)

dev.off()














