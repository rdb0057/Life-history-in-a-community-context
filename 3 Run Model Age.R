
################################################################################
#1. Run the model
################################################################################

base <- run.Pop.Dyn2sp(
  spp2 = spp2,
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
  
  #Consumer Life History parameter
  eta.type = eta.type,                               #Intercept of proportion of resources devoted towards maintenance/survival
  
  #Other parameters
  beta.s = beta.s,      #Background survival
  kappa.s = kappa.s,                              #scales survival units to resource units
  kappa.m = kappa.m,

  years = years                               #Years to run
)

#====================================================================================================
#1.1 Collect Results
#====================================================================================================
years = base[['years']]
maxt <- base[['maxt']]
C <- base[['C']]
R <- base[['R']]
P <- base[['P']]
C.eq <- base[['C.eq']]
R.eq <- base[['R.eq']]
P.eq <- base[['P.eq']] 
S.eq <- base[['S.eq']]
G.eq <- base[['G.eq']] 
g.eq <- base[['g.eq']]
s.eq <- base[['s.eq']]
m.eq <- base[['m.eq']]
d.eq <- base[['d.eq']]
g2.eq <- base[['g2.eq']]
s2.eq <- base[['s2.eq']]
m2.eq <- base[['m2.eq']]
d2.eq <- base[['d2.eq']]

comp <- base[['comp']]
g.0 <- base[['g.0']]
g.c <- base[['g.c']]
s.0 <- base[['s.0']]
s.c <- base[['s.c']]
m.0 <- base[['m.0']]
m.c <- base[['m.c']]


M.eq <- base[['M.eq']]
D.eq <- base[['D.eq']]
K.eq <- base[['K.eq']]
K.pred <- base[['K.pred']]
w <- base[['w']]
v <- base[['v']]

invasion <- base[['invasion']]
omega <- base[['omega']] 
gamma <- base[['gamma']]
theta <- base[['theta']]
pred = base[['pred']]
mean.niche.type = base[['mean.niche.type']]
phi.type = base[['phi.type']]
rho.type = base[['rho.type']]
eta.type = base[['eta.type']]
psi.mat = base[['psi.mat']] 
psi <- base[['psi']]




