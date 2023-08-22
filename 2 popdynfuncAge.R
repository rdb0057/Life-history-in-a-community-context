
run.Pop.Dyn2sp <- function(
  spp2,
    #Niche and feeding parameters
  phi.int,
  phi.type,                                   #slope of consumer feeding rate with size
  sum.params,                            #Scaling term, do not change
  mean.niche,                       #Mean of the niche 1-100
  rho.type,                              #Slope of niche change with size of consumer
  
  #Conversion of resources to consumers
  delta,                                 #Conversion efficiency
  all.R.equal,
  beta.bene,
  
  #Consumer Life History parameter
  eta.type,                               #Intercept of proportion of resources devoted towards maintenance/survival
  
  #Other parameters
  beta.s,                                 #Background survival
  kappa.s,                                #scales survival units to resource units
  kappa.m,
  years                                   #Years to run
){
  
  
  

  
  
  
  C <- array(NA, c(length(eta.type),length(phi.type),length(rho.type),years,n))
  R <- array(NA, c(length(eta.type),length(phi.type),length(rho.type),years,nR))
  C.eq <- array(NA, c(length(eta.type),length(phi.type),length(rho.type),n))
  R.eq <- array(NA, c(length(eta.type),length(phi.type),length(rho.type),nR))
  
  C2 <- array(NA, c(length(eta.type),length(phi.type),length(rho.type),years,n))
  C2.eq <- array(NA, c(length(eta.type),length(phi.type),length(rho.type),n))

  
  maxt <- array(NA, c(length(eta.type),length(phi.type),length(rho.type),years))
  invasion <- array(NA,c(length(eta.type),length(eta.type),length(phi.type),length(phi.type),length(rho.type),length(rho.type)))
  
  gamma <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n,nR))
  omega <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n,nR))
  theta <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n,nR))
  

  
  omega2 <- array(0,c(length(eta.type),length(phi.type),length(rho.type),n,nR))
  theta2 <- array(0,c(n,nR))

  S.eq <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n,n))
  M.eq <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n,n))
  s.eq <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n))
  m.eq <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n))
  
  s2.eq <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n))
  m2.eq <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n))
  
  K.eq <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),n,n))
  
  
  #===============================================================================
  #iterate through time and do invasion analysis across different mean niches
  #===============================================================================
  
  

      for (ro in 1:length(rho.type)){
        for (phi in 1:length(phi.type)){
 
          for (b in 1:length(eta.type)){
 
            
 
            t <- 1
            while (t < c(years - 1) & (( isTRUE(all.equal(R[b,phi,ro,t,],R[b,phi,ro,t-1,]))==FALSE |  isTRUE(all.equal(C2[b,phi,ro,t,],C2[b,phi,ro,t-1,]))==FALSE |isTRUE(all.equal(C[b,phi,ro,t,],C[b,phi,ro,t-1,]))==FALSE  ))==TRUE ){
              
              #===============================================================================
              #Initial population distributions
              #===============================================================================

              if(t==1){
                R[b,phi,ro,t,] <- K.res
                C[b,phi,ro,t,] <- C.trans <- rep(1,length(a))
                if(spp2==T){
                  C2[b,phi,ro,t,] <- C2.trans <- rep(1,length(a))
                }
                if(spp2==F){
                  C2[b,phi,ro,,] <- 0
                }
              }
    

             
              #===============================================================================
              #Calculate theta, resource use
              #===============================================================================
                  #Gamma is the feeding rate, can be age and resource dependent
                  
                  for (i in 1:n){
                    for (j in 1:nR){
                      gamma[b,phi,ro,i,j] <- exp(phi.int + phi.type[phi]*(a[i] - 1)) #/hR
                      
                    }
                  }
                  
                  mean.niche.scaled <- sum.params * mean.niche/maxT - 0.1 
                  for (i in 1:n){
                    shape <- (mean.niche.scaled + rho.type[ro]*(a[i] - 1))
                    if (shape<(sum.params/2)){
                      shape1 <- shape
                      shape2 <- sum.params / 2
                    }
                    if (shape>=(sum.params/2)){
                      shape1 <- sum.params / 2
                      shape2 <- sum.params - shape
                    }
                    
                    theta[b,phi,ro,i,] <- dbeta(yR/maxT, shape1=shape1, shape2=shape2, ncp = 0, log = FALSE) * hR/maxT #dnorm(yR, mean = mean.niche, sd = sd.niche)  * hR#/ sum(dnorm(yR, mean = mean.niche, sd = sd.niche) )
                    
                    theta[b,phi,ro,i,] <- theta[b,phi,ro,i,]/sum(theta[b,phi,ro,i,])
                  }
                  
                  shape <- (mean.niche.scaled + rho.type[ro]*(a[i] - 1))
                  if (shape<(sum.params/2)){
                    shape1 <- shape
                    shape2 <- sum.params / 2
                  }
                  if (shape>=(sum.params/2)){
                    shape1 <- sum.params / 2
                    shape2 <- sum.params - shape
                  }
                  mean.theta <- (dbeta(yR/maxT, shape1=shape1, shape2=shape2, ncp = 0, log = FALSE) * hR/maxT) %*% yR
                  
                  
                  hand <- array(h,c(n,nR))
                  
                  omega[b,phi,ro,,] <- (solve(diag(1 + as.vector((gamma[b,phi,ro,,]*hand)%*%(R[b,phi,ro,t,])),n,n)))%*%gamma[b,phi,ro,,]
                  
                  #benefit, increasing function of trophic level
                  B <- array(0,c(n,nR))
                  for (i in 1:n){
                    B[i,] <- 10*(beta.bene * yR) / (1 + beta.bene * yR) #
                  }
                  
                  if (all.R.equal==T){
                    B[,] <- mean(B) #* 0.75 #exp(beta.bene * yR)
                  }
                  
                  eta <- rep(0,n)
                  for (ii in 1:n){
                    if (ii>=b){
                      eta[ii] <- (1 - min.surv)
                    }
                  }
                  
                  #===============================================================================
                  #Demographic rates for resident consumer
                  #===============================================================================
                  #Survival Matrix
                  H.s <- kappa.s * (1-eta) * delta
                  u <-  beta.s + H.s * ( (B * omega[b,phi,ro,,] * theta[b,phi,ro,,]) %*% R[b,phi,ro,t,])
                  s <- invlogit(1.15 + 0.4*(a-1)) / (1 + exp(-u))
                  S <- diag(as.vector(s))
                  
                  #fecundity matrix
                  H.m <- kappa.m * eta * delta #* ( (1 - psi[b,phi,ro,])  )
                  m <-  (H.m *  (B * omega[b,phi,ro,,]  * theta[b,phi,ro,,]) %*% R[b,phi,ro,t,])  / 2
                  M <- array(0,c(n,n))
                  M[1,] <- m
                  
                  St <- t(array(0,c(n,n)))
                  for(i in 1:(n-1)){
                    St[i+1,i] <- 1
                  }
                  St[n,c((n-1),n)] <- 1
                  
                  K <- St %*% S +  M %*% S
              
              
              ########
              #Second Consumer
              ########
              if(spp2==T){
                #Gamma is the feeding rate, can be size and resource dependent
                gamma2 <- array(NA,c(n,nR))
                for (i in 1:n){
                  for (j in 1:nR){
                    gamma2[i,j] <- exp(phi.int + phi2*(a[i] - 1)) #/hR
                    
                  }
                }
                
                mean.niche.scaled <- sum.params * mean.niche/maxT - 0.1 
                for (i in 1:n){
                  shape <- (mean.niche.scaled + rho2*(a[i] - 1))
                  if (shape<(sum.params/2)){
                    shape1 <- shape
                    shape2 <- sum.params / 2
                  }
                  if (shape>=(sum.params/2)){
                    shape1 <- sum.params / 2
                    shape2 <- sum.params - shape
                  }
                  
                  theta2[i,] <- dbeta(yR/maxT, shape1=shape1, shape2=shape2, ncp = 0, log = FALSE) * hR/maxT #dnorm(yR, mean = mean.niche, sd = sd.niche)  * hR#/ sum(dnorm(yR, mean = mean.niche, sd = sd.niche) )
                  
                  theta2[i,] <- theta2[i,]/sum(theta2[i,])
                }
                
                shape <- (mean.niche.scaled + rho2*(a[i] - 1))
                if (shape<(sum.params/2)){
                  shape1 <- shape
                  shape2 <- sum.params / 2
                }
                if (shape>=(sum.params/2)){
                  shape1 <- sum.params / 2
                  shape2 <- sum.params - shape
                }
                mean.theta2 <- (dbeta(yR/maxT, shape1=shape1, shape2=shape2, ncp = 0, log = FALSE) * hR/maxT) %*% yR
                
                
                omega2[b,phi,ro,,] <- (solve(diag(1 + as.vector((gamma2*hand)%*%(R[b,phi,ro,t,])),n,n)))%*%gamma2
                
                
                
                eta2 <- rep(0,n)
                for (ii in 1:n){
                  if (ii>=b){
                    eta2[ii] <- (1 - min.surv)
                  }
                }
                
                #===============================================================================
                #Demographic rates for resident consumer
                #===============================================================================
                #Survival Matrix
                H.s <- kappa.s * (1-eta2) * delta
                u <-  beta.s + H.s * ( (B * omega2[b,phi,ro,,] * theta2[,]) %*% R[b,phi,ro,t,])
                s2 <- invlogit(1.15 + 0.4*(a-1)) / (1 + exp(-u))
                S2 <- diag(as.vector(s2))
                
                #fecundity matrix
                H.m <- kappa.m * eta2 * delta #* ( (1 - psi[b,phi,ro,])  )
                m2 <-  (H.m *  (B * omega2[b,phi,ro,,]  * theta2[,]) %*% R[b,phi,ro,t,])  / 2
                M2 <- array(0,c(n,n))
                M2[1,] <- m2
                
                
                K2 <- St %*% S2 +  M2 %*% S2
                
                C2[b,phi,ro,t+1,] <- K2 %*% C2[b,phi,ro,t,]
                C2.trans <- C2[b,phi,ro,years,] <- C2[b,phi,ro,t+1,]
              }
              
              #Project Resources forward in time
              
              R[b,phi,ro,t+1,] <- R[b,phi,ro,t,] * exp(r.res) / (1 + (exp(r.res) - 1) / K.res * R[b,phi,ro,t,] +
                                            C[b,phi,ro,t,] %*% (omega[b,phi,ro,,] * theta[b,phi,ro,,]) +
                                            C2[b,phi,ro,t,] %*% (omega2[b,phi,ro,,] * theta2))
              
              #Project Consumer forward in time
              C[b,phi,ro,t+1,] <- K %*% C[b,phi,ro,t,]
              
              
              
              R.trans <- R[b,phi,ro,years,] <- R[b,phi,ro,t+1,]
              C.trans <- C[b,phi,ro,years,] <- C[b,phi,ro,t+1,]
              
             
              maxt <- t-1
              t <- t+1
              
            }
            R.eq[b,phi,ro,] <- R[b,phi,ro,years,]
            C.eq[b,phi,ro,] <- C[b,phi,ro,years,]
            C2.eq[b,phi,ro,] <- C2[b,phi,ro,years,]
            S.eq[b,phi,ro,,] <- S
            M.eq[b,phi,ro,,] <- M
            K.eq[b,phi,ro,,] <- K
            s.eq[b,phi,ro,] <- s
            m.eq[b,phi,ro,] <- m
            
            #===================================================================
            #Invader Consumers
            #===================================================================
            
            for (b.inv in 1:length(eta.type)){
              for (ro.inv in 1:length(rho.type)){
                for (phi.inv in 1:length(phi.type)){
                  
                  
                  mean.niche.scaled.inv <- sum.params * mean.niche/maxT - 0.1 
                  theta.inv <- array(NA,c(n,nR))
                  for (i in 1:n){
                    shape <- (mean.niche.scaled.inv + rho.type[ro.inv]*(a[i] - 1))
                    if (shape<(sum.params/2)){
                      shape1 <- shape
                      shape2 <- sum.params / 2
                    }
                    if (shape>=(sum.params/2)){
                      shape1 <- sum.params / 2
                      shape2 <- sum.params - shape
                    }
                    # shape2 <- sum.params - shape1
                    theta.inv[i,] <- dbeta(yR/maxT, shape1=shape1, shape2=shape2, ncp = 0, log = FALSE) * hR/maxT #dnorm(yR, mean = mean.niche, sd = sd.niche)  * hR#/ sum(dnorm(yR, mean = mean.niche, sd = sd.niche) )
                    # print(c(shape1,shape2))
                    theta.inv[i,] <- theta.inv[i,]/sum(theta.inv[i,])
                  }
                  
                  
                  shape <- (mean.niche.scaled.inv)
                  if (shape<(sum.params/2)){
                    shape1 <- shape
                    shape2 <- sum.params / 2
                  }
                  if (shape>=(sum.params/2)){
                    shape1 <- sum.params / 2
                    shape2 <- sum.params - shape
                  }
                  mean.theta.inv <- (dbeta(yR/maxT, shape1=shape1, shape2=shape2, ncp = 0, log = FALSE) * hR/maxT) %*% yR
                  
                  #Gamma is the feeding rate, can be size and resource dependent
                  gamma.inv <- array(NA,c(n,nR))
                  for (i in 1:n){
                    for (j in 1:nR){
                      gamma.inv[i,j] <- exp(phi.int + phi.type[phi.inv]*(a[i] - 1)) #/hR
                      
                    }
                  }
                  
                  omega.inv <- (solve(diag(1 + as.vector((gamma.inv*hand)%*%(R.eq[b,phi,ro,])),n,n)))%*%gamma.inv
                  
                  eta.inv <- rep(0,n)
                  for (ii in 1:n){
                    if (ii>=b.inv){
                      eta.inv[ii] <- (1 - min.surv)
                    }
                  }
                  
                  
                  #Demographic Rates for Consumer
                  #Survival Matrix
                  H.s.inv <-  kappa.s * (1-eta.inv) * delta 
                  u <-  beta.s + H.s.inv * ( (B * omega.inv * theta.inv) %*% R.eq[b,phi,ro,])
                  s.inv <- invlogit(1.15 + 0.4*(a-1)) / (1 + exp(-u))
                  S.inv <- diag(as.vector(s.inv))
                  
                  #fecundity matrix
                  H.m.inv <- kappa.m * eta.inv * delta #( (1 - psi.inv) )
                  m.inv <-  H.m.inv * ( (B * omega.inv  * theta.inv) %*% R.eq[b,phi,ro,])  / 2
                  M.inv <- array(0,c(n,n))
                  M.inv[1,] <- m.inv
                  
                  K.inv <- St %*% S.inv +  M.inv %*% S.inv
                  
                  invasion[b,b.inv,phi,phi.inv,ro,ro.inv] <- Re(eigen(K.inv)$values[1])
                  
                  if(maxt==(years-3)){
                    omega.inv2 <- (solve(diag(1 + as.vector((gamma.inv*hand)%*%(R[b,phi,ro,t-1,])),n,n)))%*%gamma.inv
                    
                    #Demographic Rates for Consumer
                    #Survival Matrix
                    H.s.inv <-  kappa.s * (1-eta.inv) * delta 
                    u <-  beta.s + H.s.inv * ( (B * omega.inv * theta.inv) %*% R[b,phi,ro,t-1,])
                    s.inv <- invlogit(1.15 + 0.4*(a-1)) / (1 + exp(-u))
                    S.inv <- diag(as.vector(s.inv))
                    
                    #fecundity matrix
                    H.m.inv <- kappa.m * eta.inv * delta #( (1 - psi.inv) )
                    m.inv <-  H.m.inv * ( (B * omega.inv  * theta.inv) %*% R[b,phi,ro,t-1,])  / 2
                    M.inv <- array(0,c(n,n))
                    M.inv[1,] <- m.inv
                    
                    K.inv2 <- St %*% S.inv +  M.inv %*% S.inv
                    
                    invasion[b,b.inv,phi,phi.inv,ro,ro.inv] <- Re(eigen(K.inv%*%K.inv2)$values[1])
                    
                    
                  }
                  
                }
              }
            }
            
            
            print(c(round((eta.type[b]),3),round(phi.type[phi],3),round(rho.type[ro],3),round(sum(R.eq[b,phi,ro,]),3),round(sum(C.eq[b,phi,ro,]),4),round(sum(C2.eq[b,phi,ro,]),4),maxt))
            }
          
          
          }
        }
    
  
  
  
  results <-      list(years,maxt,C,R,C.eq,R.eq,S.eq,M.eq,s.eq,m.eq,s2.eq,m2.eq,K.eq,invasion,gamma,omega,theta,phi.type,rho.type,eta.type )
  names(results) <- c('years','maxt','C','R','C.eq','R.eq','S.eq','M.eq','s.eq','m.eq','s2.eq','m2.eq','K.eq','invasion','gamma','omega','theta','phi.type','rho.type','eta.type')
  return(results)
  
}

