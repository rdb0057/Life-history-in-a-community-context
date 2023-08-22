
################################################################################
#1. Run the model
################################################################################

base <- run.Pop.Dyn(
  pred = pred,                           #predator population starting sizes
  
  #Niche and feeding parameters
  phi.int = phi.int,                           #Consumer feeding rate at size at birth
  phi.type = phi.type,               #slope of consumer feeding rate with size
  sum.params = sum.params,                            #Scaling term, do not change
  mean.niche = mean.niche,                      #Mean of the niche 1-100
  rho.type = rho.type,       #Slope of niche change with size of consumer
  
  #Conversion of resources to consumers
  delta = delta,#0.15,                              #Conversion efficiency
  all.R.equal = all.R.equal,                           #If T, then all resources benefit the consumer the same way, scaled by 0.75
  beta.bene = beta.bene,
  beta.DC = beta.DC,         
  
  #Consumer Life History parameter
  eta.z = eta.z,                                 #Slope of proportion of resources devoted towards maintenance/survival
  psi.0 = psi.0,                                  #Intercept of growth fecundity trade-off
  psi.z = psi.z,                              #Slope of growth fecundity tradeoff
  psi.mat = psi.mat,#seq(9,22,length.out=100),#15,                               #Size at Maturity
  
  #Other parameters
  beta.s = beta.s,      #Background survival
  kappa.s = kappa.s,                              #scales survival units to resource units
  kappa.g = kappa.g,                                  #scales growth and reproduction to resource units
  kappa.m = kappa.m,
  g.var = g.var,                               #Variance in growth
  beta.d = beta.d,                               #Mean offspring size
  d.var = d.var,                               #variance in offspring size
  years = years                               #Years to run
)

#====================================================================================================
#1.1 Collect Results
#====================================================================================================
years = base[['years']]
maxt <- base[['maxt']]
C <- base[['C']]
R <- base[['R']]

C.eq <- base[['C.eq']]
R.eq <- base[['R.eq']]

S.eq <- base[['S.eq']]
G.eq <- base[['G.eq']] 
g.eq <- base[['g.eq']]
s.eq <- base[['s.eq']]
m.eq <- base[['m.eq']]
d.eq <- base[['d.eq']]


M.eq <- base[['M.eq']]
D.eq <- base[['D.eq']]
K.eq <- base[['K.eq']]

invasion <- base[['invasion']]
omega <- base[['omega']] 
theta <- base[['theta']]
pred = base[['pred']]
mean.niche = base[['mean.niche']]
phi.type = base[['phi.type']]
rho.type = base[['rho.type']]
eta.type = base[['eta.type']]
psi.mat = base[['psi.mat']] 
psi <- base[['psi']]




