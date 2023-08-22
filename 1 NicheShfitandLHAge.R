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
#Resource Parameters and Functions
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
res.growth <- function(yR,K,r,uniform){
  K.res <- K*exp(-0.01*(yR-(maxT/2)))
  r.res <- r*exp(-0.01*(yR-(maxT/2)))

  if(uniform==T){
    K.res[] <- mean(K.res)
    r.res[] <- mean(r.res)
  }
  return(list(r.res, K.res))
}


################################################################################
#Consumer Parameters and Functions
################################################################################
#===============================================================================
#Set ages for consumer
#===============================================================================

a = c(1:6)#These are the ages 
n = length(a)


#===============================================================================
#Default parameters
#===============================================================================
#Niche and feeding parameters
phi.int = -0.275                                   #Consumer feeding rate at age at birth
phi.type =seq(0,0.2,length.out=2)                  #slope of consumer feeding rate with age
sum.params = 10                                    #Scaling term, do not change
mean.niche = 20                                    #Mean of the niche 1-100
rho.type = seq(0,0.8,length.out=2)                 #Slope of niche change with age of consumer
h <- 0


#Conversion of resources to consumers
delta = 0.15                                        #Conversion efficiency
beta.bene = 0.01

#Consumer Life History parameter
eta.type = a#seq(6,22,length.out=75)                #Intercept of proportion of resources devoted towards maintenance/survival

#Other parameters
beta.s = -4.4#rep(mean(-4.5 + 0.01 * (y-6)),n)      #Background survival
kappa.s = rep(mean(5*((1/(6+4*(a-1)))^(1/10)) ),n)               #scales survival units to resource units
kappa.m = (1/6.5)
min.surv <- 0.5                                     #minimum allocation to survival

years = 20000                                       #Years to run


#Run source file that contains functions to run
source("./2 popdynfuncAge.R")


################################################################################
#Figures 1, 2, 3-Life history predictions
################################################################################
#Analysis specific parameters
spp2 <- F
#===============================================================================
#Resources Substitutable
#===============================================================================

all.R.equal = T                                     #If T, then all resources benefit the consumer the same way
r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[2]]

source("./3 Run Model Age.R")
source("./4 Calcmaxs.R")

#save the invasion growth rates for later
invasion.base <- invasion
maximum11.base <- maximum11
maximum21.base <- maximum21
maximum12.base <- maximum12
maximum22.base <- maximum22
S.eq.base <- S.eq
lamb.inv.df1 <- lamb.inv.df

#===============================================================================
#contrast with interaction coefficient approach-Figure S1
#===============================================================================

source("./7 manip inter coeff.R")

jpeg(file = "./Figures/Figure S1.jpeg",width = 9, height = 7, units='in', res=600)
grid.arrange(plot1,plot2,plot3,plot4,
             plot5,plot6,plot7,plot8,
             plot9,plot10,plot11,plot12,
                     nrow=3,ncol=4,
             top="\n",
             left="\n")
panel.text(2900, 150, expression("Organismal interactions with resource"), cex = 1, font = 1)

panel.text(950, 300, expression(phi==0~rho==0), cex = 1, font = 1)
panel.text(2200, 300, expression(phi==0.2~rho==0), cex = 1, font = 1)
panel.text(3500, 300, expression(phi==0~rho==0.8), cex = 1, font = 1)
panel.text(4800, 300, expression(phi==0.2~rho==0.8), cex = 1, font = 1)

panel.text(100, 2000, expression("Organismal interactions affect"), cex = 1, font = 1,srt=90)
panel.text(200, 850, expression(DI~and~DD), cex = 1, font = 1,srt=90)
panel.text(200, 2200, expression(DD~only), cex = 1, font = 1,srt=90)
panel.text(200, 3600, expression(DI~only), cex = 1, font = 1,srt=90)

panel.text(550, 450, expression("A)"), cex = 1, font = 1)
panel.text(1800, 450, expression("B)"), cex = 1, font = 1)
panel.text(3050, 450, expression("C)"), cex = 1, font = 1)
panel.text(4350, 450, expression("D)"), cex = 1, font = 1)

panel.text(550, 1750, expression("E)"), cex = 1, font = 1)
panel.text(1800, 1750, expression("F)"), cex = 1, font = 1)
panel.text(3050, 1750, expression("G)"), cex = 1, font = 1)
panel.text(4350, 1750, expression("H)"), cex = 1, font = 1)

panel.text(550, 3050, expression("I)"), cex = 1, font = 1)
panel.text(1800, 3050, expression("J)"), cex = 1, font = 1)
panel.text(3050, 3050, expression("K)"), cex = 1, font = 1)
panel.text(4350, 3050, expression("L)"), cex = 1, font = 1)

dev.off()




#===============================================================================
#Not Substitutable, different benefit
#===============================================================================
all.R.equal = F                                     #If T, then all resources benefit the consumer the same way
r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[2]]

source("./3 Run Model Age.R")
source("./4 Calcmaxs.R")
#save the invasion growth rates for later
lamb.inv.df2 <- lamb.inv.df
maximum11.2 <- maximum11
maximum21.2 <- maximum21
maximum12.2 <- maximum12
maximum22.2 <- maximum22
S.eq.2 <- S.eq

#===============================================================================
#Not Substitutable, different abundance
#===============================================================================

all.R.equal = T                                     #If T, then all resources benefit the consumer the same way
r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[2]]

source("./3 Run Model Age.R")
source("./4 Calcmaxs.R")
#save the invasion growth rates for later
lamb.inv.df3 <- lamb.inv.df
maximum11.3 <- maximum11
maximum21.3 <- maximum21
maximum12.3 <- maximum12
maximum22.3 <- maximum22
S.eq.3 <- S.eq


#===============================================================================
#Not Substitutable, different benefit and abundance
#===============================================================================

all.R.equal = F                                     #If T, then all resources benefit the consumer the same way
r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[2]]

source("./3 Run Model Age.R")
source("./4 Calcmaxs.R")
#save the invasion growth rates for later
lamb.inv.df4 <- lamb.inv.df
maximum11.4 <- maximum11
maximum21.4 <- maximum21
maximum12.4 <- maximum12
maximum22.4 <- maximum22
S.eq.4 <- S.eq



#===============================================================================
#Figure 1
#===============================================================================

xlab <- list(label='Age at Maturity Resident',cex=0.5)
ylab <- list(label='Age at Maturity Invader',cex=0.5)

source("./5 Invasion Figures Age.R")
jpeg(file = "./Figures/Figure 1.jpeg",width = 9, height = 7, units='in', res=600)

grid.arrange(plot11,plot12,plot13,plot14,
             plot21,plot22,plot23,plot24,
             plot31,plot32,plot33,plot34,
             plot41,plot42, plot43,plot44,
             nrow=4,ncol=4,
             top="\n",
             left="\n")

panel.text(2900, 150, expression("Organismal interactions with resource"), cex = 1, font = 1)

panel.text(950, 300, expression(phi==0~rho==0), cex = 1, font = 1)
panel.text(2200, 300, expression(phi==0.2~rho==0), cex = 1, font = 1)
panel.text(3500, 300, expression(phi==0~rho==0.8), cex = 1, font = 1)
panel.text(4800, 300, expression(phi==0.2~rho==0.8), cex = 1, font = 1)

panel.text(100, 2000, expression(Resource~properties), cex = 1, font = 1,srt=90)
panel.text(200, 650, expression(Substitutable), cex = 1, font = 1,srt=90)
panel.text(200, 1600, expression(Diff~B), cex = 1, font = 1,srt=90)
panel.text(200, 2600, expression(Diff~K), cex = 1, font = 1,srt=90)
panel.text(200, 3600, expression(Diff~K~and~B), cex = 1, font = 1,srt=90)

panel.text(550, 450, expression("A)"), cex = 1, font = 1)
panel.text(1800, 450, expression("B)"), cex = 1, font = 1)
panel.text(3050, 450, expression("C)"), cex = 1, font = 1)
panel.text(4350, 450, expression("D)"), cex = 1, font = 1)

panel.text(550, 1425, expression("E)"), cex = 1, font = 1)
panel.text(1800, 1425, expression("F)"), cex = 1, font = 1)
panel.text(3050, 1425, expression("G)"), cex = 1, font = 1)
panel.text(4350, 1425, expression("H)"), cex = 1, font = 1)

panel.text(550, 2400, expression("I)"), cex = 1, font = 1)
panel.text(1800, 2400, expression("J)"), cex = 1, font = 1)
panel.text(3050, 2400, expression("K)"), cex = 1, font = 1)
panel.text(4350, 2400, expression("L)"), cex = 1, font = 1)

panel.text(550, 3375, expression("M)"), cex = 1, font = 1)
panel.text(1800, 3375, expression("N)"), cex = 1, font = 1)
panel.text(3050, 3375, expression("O)"), cex = 1, font = 1)
panel.text(4350, 3375, expression("P)"), cex = 1, font = 1)

dev.off()


#===============================================================================
#Figure 2
#===============================================================================

jpeg(file = "./Figures/Figure 2.jpeg",width = 7, height = 9, units='in', res=600)

ylab <- 'Survival (S(a))'
xlab <- 'Age (a)'

ylim <- c(0.6,1)

age <- a


par(mfrow=c(4,3),mar = c(4,4,1,2),cex=1)
plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab=ylab,xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.base[maximum21.base,2,1,,])~age,col='blue',type='p',pch=16)
title(main="A)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.base[maximum12.base,1,2,,])~age,col='blue',type='p',pch=16)
title(main="B)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.base[maximum22.base,2,2,,])~age,col='blue',type='p',pch=16)
points(diag(S.eq.base[1,2,2,,])~age,col='orange',type='p',pch=16)
title(main="C)",adj = 0.1, line = 0,cex=0.5)




plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab=ylab,xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.2[maximum21.2,2,1,,])~age,col='blue',type='p',pch=16)
title(main="D)",adj = 0.1, line = 0,cex=0.5)


plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.2[maximum12.2,1,2,,])~age,col='blue',type='p',pch=16)
title(main="E)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.2[maximum22.2,2,2,,])~age,col='blue',type='p',pch=16)
title(main="F)",adj = 0.1, line = 0,cex=0.5)




plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab=ylab,xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.3[maximum21.3,2,1,,])~age,col='blue',type='p',pch=16)
title(main="G)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.3[maximum12.3,1,2,,])~age,col='blue',type='p',pch=16)
title(main="H)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.3[maximum22.3,2,2,,])~age,col='blue',type='p',pch=16)
title(main="I)",adj = 0.1, line = 0,cex=0.5)



plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab=ylab,xlab=xlab,col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.4[maximum21.4,2,1,,])~age,col='blue',type='p',pch=16)
title(main="J)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab=xlab,col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.4[maximum12.4,1,2,,])~age,col='blue',type='p',pch=16)
title(main="K)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab=xlab,col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.4[6,2,2,,])~age,col='blue',type='p',pch=16)
points(diag(S.eq.4[maximum22.4,2,2,,])~age,col='orange',type='p',pch=16)
title(main="L)",adj = 0.1, line = 0,cex=0.5)

dev.off()




################################################################################
#Figures 3 and 4-Life history and competition with another species
################################################################################

spp2 <- T
#===============================================================================
#For species that shifts niche
#===============================================================================
#===============================================================================
#Substitutable
#===============================================================================
eta.type = a
all.R.equal = T

r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[2]]


#Niche and feeding parameters
phi.type = 0                #slope of consumer feeding rate with age
rho.type = 0.8                #Slope of niche change with age of consumer

phi2 <- 0.2
rho2 <- 0
eta.type2 <- eta.type[6]


source("./3 Run Model Age.R")


dimnames(invasion) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(phi.type),c(rho.type),c(rho.type))
lamb.inv.df <- reshape2::melt(invasion)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi.res','phi.inv','ro.res','ro.inv','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10
lamb.inv.df1 <- lamb.inv.df

toplot <- lamb.inv.df
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('eta.type.res'),summarise,test=sum(test))
test[which(test$test==length(eta.type)),]
test$max <- length(eta.type) - test$test
maximum1 <- which.min(test$max)
S.eq.1 <- S.eq


#===============================================================================
#Diff B
#===============================================================================

all.R.equal = F

r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[2]]


#Niche and feeding parameters
phi.type = 0                #slope of consumer feeding rate with age
rho.type = 0.8                #Slope of niche change with age of consumer

phi2 <- 0.2
rho2 <- 0
eta.type2 <- eta.type[6]



source("./3 Run Model Age.R")

# ages <- seq(1,length(y),length.out=length(eta.type))
dimnames(invasion) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(phi.type),c(rho.type),c(rho.type))
lamb.inv.df <- reshape2::melt(invasion)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi.res','phi.inv','ro.res','ro.inv','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10
lamb.inv.df2 <- lamb.inv.df

toplot <- lamb.inv.df
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('eta.type.res'),summarise,test=sum(test))
test[which(test$test==length(eta.type)),]
test$max <- length(eta.type) - test$test
maximum2 <- which.min(test$max)
S.eq.2 <- S.eq


#===============================================================================
#Diff pop size
#===============================================================================

all.R.equal = T

r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[2]]


#Niche and feeding parameters
phi.type = 0                #slope of consumer feeding rate with age
rho.type = 0.8                #Slope of niche change with age of consumer

phi2 <- 0.2
rho2 <- 0
eta.type2 <- eta.type[6]



source("./3 Run Model Age.R")

# ages <- seq(1,length(y),length.out=length(eta.type))
dimnames(invasion) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(phi.type),c(rho.type),c(rho.type))
lamb.inv.df <- reshape2::melt(invasion)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi.res','phi.inv','ro.res','ro.inv','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10
lamb.inv.df3 <- lamb.inv.df


toplot <- lamb.inv.df
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('eta.type.res'),summarise,test=sum(test))
test[which(test$test==length(eta.type)),]
test$max <- length(eta.type) - test$test
maximum3 <- which.min(test$max)
S.eq.3 <- S.eq




#===============================================================================
#diff pop size and B
#==============================================================================
all.R.equal = F

r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[2]]


#Niche and feeding parameters
phi.type = 0                #slope of consumer feeding rate with age
rho.type = 0.8                #Slope of niche change with age of consumer

phi2 <- 0.2
rho2 <- 0
eta.type2 <- eta.type[6]



source("./3 Run Model Age.R")

# ages <- seq(1,length(y),length.out=length(eta.type))
dimnames(invasion) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(phi.type),c(rho.type),c(rho.type))
lamb.inv.df <- reshape2::melt(invasion)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi.res','phi.inv','ro.res','ro.inv','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10
lamb.inv.df4 <- lamb.inv.df


toplot <- lamb.inv.df
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('eta.type.res'),summarise,test=sum(test))
test[which(test$test==length(eta.type)),]
test$max <- length(eta.type) - test$test
maximum4 <- which.min(test$max)
S.eq.4 <- S.eq










#===============================================================================
# For species that increases feeding rate
#==============================================================================
#===============================================================================
#substitutable
#==============================================================================

all.R.equal = T

r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[2]]


#Niche and feeding parameters
phi.type = 0.2                #slope of consumer feeding rate with age
rho.type = 0                #Slope of niche change with age of consumer

phi2 <- 0
rho2 <- 0.8
eta.type2 <- eta.type[1]



source("./3 Run Model Age.R")

# ages <- seq(1,length(y),length.out=length(eta.type))
dimnames(invasion) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(phi.type),c(rho.type),c(rho.type))
lamb.inv.df <- reshape2::melt(invasion)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi.res','phi.inv','ro.res','ro.inv','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10
lamb.inv.df5 <- lamb.inv.df

toplot <- lamb.inv.df
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('eta.type.res'),summarise,test=sum(test))
test[which(test$test==length(eta.type)),]
test$max <- length(eta.type) - test$test
maximum5 <- which.min(test$max)
S.eq.5 <- S.eq


#===============================================================================
#diff B
#==============================================================================


all.R.equal = F

r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=T)[[2]]


#Niche and feeding parameters
phi.type = 0.2                #slope of consumer feeding rate with age
rho.type = 0                #Slope of niche change with age of consumer

phi2 <- 0
rho2 <- 0.8
eta.type2 <- eta.type[6]



source("./3 Run Model Age.R")

# ages <- seq(1,length(y),length.out=length(eta.type))
dimnames(invasion) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(phi.type),c(rho.type),c(rho.type))
lamb.inv.df <- reshape2::melt(invasion)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi.res','phi.inv','ro.res','ro.inv','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10
lamb.inv.df6 <- lamb.inv.df

toplot <- lamb.inv.df
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('eta.type.res'),summarise,test=sum(test))
test[which(test$test==length(eta.type)),]
test$max <- length(eta.type) - test$test
maximum6 <- which.min(test$max)
S.eq.6 <- S.eq


#===============================================================================
#Diff pop size
#==============================================================================

all.R.equal = T

r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[2]]


#Niche and feeding parameters
phi.type = 0.2                #slope of consumer feeding rate with age
rho.type = 0                #Slope of niche change with age of consumer

phi2 <- 0
rho2 <- 0.8
eta.type2 <- eta.type[1]



source("./3 Run Model Age.R")

# ages <- seq(1,length(y),length.out=length(eta.type))
dimnames(invasion) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(phi.type),c(rho.type),c(rho.type))
lamb.inv.df <- reshape2::melt(invasion)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi.res','phi.inv','ro.res','ro.inv','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10
lamb.inv.df7 <- lamb.inv.df

toplot <- lamb.inv.df
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('eta.type.res'),summarise,test=sum(test))
test[which(test$test==length(eta.type)),]
test$max <- length(eta.type) - test$test
maximum7 <- which.min(test$max)
S.eq.7 <- S.eq


#===============================================================================
#diff pop size and B
#==============================================================================

all.R.equal = F

r.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[1]]
K.res <- res.growth(yR=yR,K=26,r=0.82,uniform=F)[[2]]


#Niche and feeding parameters
phi.type = 0.2                #slope of consumer feeding rate with age
rho.type = 0                #Slope of niche change with age of consumer

phi2 <- 0
rho2 <- 0.8
eta.type2 <- eta.type[1]



source("./3 Run Model Age.R")

# ages <- seq(1,length(y),length.out=length(eta.type))
dimnames(invasion) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(phi.type),c(rho.type),c(rho.type))
lamb.inv.df <- reshape2::melt(invasion)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi.res','phi.inv','ro.res','ro.inv','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10
lamb.inv.df8 <- lamb.inv.df


toplot <- lamb.inv.df
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('eta.type.res'),summarise,test=sum(test))
test[which(test$test==length(eta.type)),]
test$max <- length(eta.type) - test$test
maximum8 <- which.min(test$max)
S.eq.8 <- S.eq








#===============================================================================
#Figure 3
#==============================================================================
xlab <- list(label='Age at Maturity Resident',cex=0.5)
ylab <- list(label='Age at Maturity Invader',cex=0.5)

toplot <- lamb.inv.df1
plot12 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab="",
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum1 ~ maximum1, pch=19,col = 'black',cex=2))

toplot <- lamb.inv.df2
plot22 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab="",
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum2 ~ maximum2, pch=19,col = 'black',cex=2))

toplot <- lamb.inv.df3
plot32 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab="",
                    par.settings = theme.novpadding.left)#+ 
  # as.layer(xyplot(maximum3 ~ maximum3, pch=19,col = 'black',cex=2))

toplot <- lamb.inv.df4
plot42 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab="",
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum4 ~ maximum4, pch=19,col = 'black',cex=2))




toplot <- lamb.inv.df5
plot11 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum5 ~ maximum5, pch=19,col = 'black',cex=2))

toplot <- lamb.inv.df6
plot21 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum6 ~ maximum6, pch=19,col = 'black',cex=2))

toplot <- lamb.inv.df7
plot31 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum7 ~ maximum7, pch=19,col = 'black',cex=2))

toplot <- lamb.inv.df8
plot41 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum8 ~ maximum8, pch=19,col = 'black',cex=2))



jpeg(file = "./Figures/Figure 3.jpeg",width = 5, height = 7, units='in', res=600)

grid.arrange(plot11,plot12,#plot13,plot14,
             plot21,plot22,#plot23,plot24,
             plot31,plot32,#plot33,plot34,
             plot41,plot42, #plot43,plot44,
             nrow=4,ncol=2,
             top="\n",
             left="\n")

panel.text(1700, 150, expression("Species that"), cex = 1, font = 1)

panel.text(950, 300, expression('Increases feeding rate with age'), cex = 0.75, font = 1)
panel.text(2200, 300, expression('Shifts its resource niche with age'), cex =0.75, font = 1)

panel.text(100, 2000, expression(Resource~properties), cex = 1, font = 1,srt=90)
panel.text(200, 650, expression(Substitutable), cex = 0.75, font = 1,srt=90)
panel.text(200, 1600, expression(Diff~B), cex = 0.75, font = 1,srt=90)
panel.text(200, 2600, expression(Diff~K), cex = 0.75, font = 1,srt=90)
panel.text(200, 3600, expression(Diff~K~and~B), cex = 0.75, font = 1,srt=90)

panel.text(550, 450, expression("A)"), cex = 1, font = 1)
panel.text(1850, 450, expression("B)"), cex = 1, font = 1)


panel.text(550, 1425, expression("C)"), cex = 1, font = 1)
panel.text(1850, 1425, expression("D)"), cex = 1, font = 1)

panel.text(550, 2400, expression("E)"), cex = 1, font = 1)
panel.text(1850, 2400, expression("F)"), cex = 1, font = 1)

panel.text(550, 3375, expression("G)"), cex = 1, font = 1)
panel.text(1850, 3375, expression("H)"), cex = 1, font = 1)

dev.off()







#===============================================================================
#Figure 4
#==============================================================================
jpeg(file = "./Figures/Figure 4.jpeg",width = 7, height = 9, units='in', res=600)

ylab <- 'Survival (S(a))'
xlab <- 'Age (a)'
ylim <- c(0.6,1)

age <- c(1:n)


par(mfrow=c(4,2),mar = c(4,4,1,2),cex=1)
plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.4[maximum4,1,1,,])~age,col='blue',type='p',pch=16)
title(main="A)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.1[maximum1,1,1,,])~age,col='blue',type='p',pch=16)
title(main="B)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.5[maximum5,1,1,,])~age,col='blue',type='p',pch=16)
title(main="C)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.2[maximum2,1,1,,])~age,col='blue',type='p',pch=16)
title(main="D)",adj = 0.1, line = 0,cex=0.5)





plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.6[maximum6,1,1,,])~age,col='blue',type='p',pch=16)
title(main="E)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=c(0,1), bty='L',cex.axis=0.75)
points(diag(S.eq.3[maximum3,1,1,,])~age,col='blue',type='p',pch=16)
title(main="F)",adj = 0.1, line = 0,cex=0.5)


plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.8[maximum8,1,1,,])~age,col='blue',type='p',pch=16)
title(main="G)",adj = 0.1, line = 0,cex=0.5)

plot(diag(S.eq.base[maximum11.base,1,1,,])~age,ylab='',xlab='',col='black',type='p',pch=16,ylim=ylim, bty='L',cex.axis=0.75)
points(diag(S.eq.4[maximum4,1,1,,])~age,col='blue',type='p',pch=16)
title(main="H)",adj = 0.1, line = 0,cex=0.5)


dev.off()








