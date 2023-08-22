
run.Pop.Dyn <- function(
                      pred,                                  #predator population starting sizes
                      
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
                      beta.DC,         
                      
                      #Consumer Life History parameter
                      # eta.type,                               #Intercept of proportion of resources devoted towards maintenance/survival
                      eta.z,                                  #Slope of proportion of resources devoted towards maintenance/survival
                      psi.0,                                  #Intercept of growth fecundity trade-off
                      psi.z,                                  #Slope of growth fecundity tradeoff
                      psi.mat,                                #Size at Maturity
                      
                      #Other parameters
                      beta.s,                                 #Background survival
                      kappa.s,                                #scales survival units to resource units
                      kappa.g,                                  #scales growth and reproduction to resource units
                      kappa.m,
                      g.var,                                  #Variance in growth
                      beta.d,                                 #Mean offspring size
                      d.var,                                  #variance in offspring size

                      years                                   #Years to run
){
  
  
  

  
  
  
  C <- array(NA, c(length(psi.mat),length(phi.type),length(rho.type),years,n))
  R <- array(NA, c(length(psi.mat),length(phi.type),length(rho.type),years,nR))
  P <- array(NA, c(length(psi.mat),length(phi.type),length(rho.type),years))
  C.eq <- array(NA, c(length(psi.mat),length(phi.type),length(rho.type),n))
  R.eq <- array(NA, c(length(psi.mat),length(phi.type),length(rho.type),nR))
  P.eq <- array(NA, c(length(psi.mat),length(phi.type),length(rho.type)))
  
  maxt <- array(NA, c(length(psi.mat),length(phi.type),length(rho.type),years))
  
  invasion <- array(NA,c(length(psi.mat),length(psi.mat),length(phi.type),length(phi.type),length(rho.type),length(rho.type)))
  
  
  omega <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n,nR))
  theta <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n,nR))
  psi <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n))
  
  S.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n,n))
  G.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n,n))
  M.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n,n))
  D.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n,n))
  g.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n))
  s.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n))
  m.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n))
  d.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n))
  
  g2.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n))
  s2.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n))
  m2.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n))
  d2.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n))
  

  K.eq <- array(NA,c(length(psi.mat),length(phi.type),length(rho.type),n,n))
  

  #===============================================================================
  #iterate through time and do invasion analysis across different mean niches
  #===============================================================================
  
  
 
    
      for (ro in 1:length(rho.type)){
        for (phi in 1:length(phi.type)){
       
          
          for (be in 1:length(psi.mat)){
            
 
            t <- 1
            while (t < c(years - 1) & (( isTRUE(all.equal(R[be,phi,ro,t,],R[be,phi,ro,t-1,]))==FALSE | isTRUE(all.equal(C[be,phi,ro,t,],C[be,phi,ro,t-1,]))==FALSE  ))==TRUE ){
              
              #===============================================================================
              #Initial population distributions
              #===============================================================================
              
                if(t==1){
                  R[be,phi,ro,t,] <- K.res
                  C[be,phi,ro,t,] <- C.trans <- (dnorm(y,10,1)*3 + dnorm(y,15,2)*2 + dnorm(y,22,2)*1)  / 10
                }
           
              #===============================================================================
              #Calculate theta, resource use
              #===============================================================================
              
              #Gamma is the feeding rate, can be size and resource dependent
              gamma <- array(NA,c(n,nR))
              for (i in 1:n){
                for (j in 1:nR){
                  gamma[i,j] <- exp(phi.int + phi.type[phi]*(y[i]-beta.d)) 
                  
                }
              }
              
              mean.niche.scaled <- sum.params * mean.niche/maxT 
              # theta <- array(NA,c(n,nR))
              for (i in 1:n){
                shape <- (mean.niche.scaled + rho.type[ro]*(y[i] - beta.d))
                if (shape<(sum.params/2)){
                  shape1 <- shape
                  shape2 <- sum.params / 2
                }
                if (shape>=(sum.params/2)){
                  shape1 <- sum.params / 2
                  shape2 <- sum.params - shape
                }
                # shape2 <- sum.params - shape1
                theta[be,phi,ro,i,] <- dbeta(yR/maxT, shape1=shape1, shape2=shape2, ncp = 0, log = FALSE) * hR/maxT 
              
                theta[be,phi,ro,i,] <- theta[be,phi,ro,i,]/sum(theta[be,phi,ro,i,])
                }
              
              shape <- (mean.niche.scaled + rho.type[ro]*(y[i] - beta.d))
              if (shape<(sum.params/2)){
                shape1 <- shape
                shape2 <- sum.params / 2
              }
              if (shape>=(sum.params/2)){
                shape1 <- sum.params / 2
                shape2 <- sum.params - shape
              }
              mean.theta <- (dbeta(yR/maxT, shape1=shape1, shape2=shape2, ncp = 0, log = FALSE) * hR/maxT) %*% yR
              

              #===================================================================
              #Resident Consumer Cost Benefits
              #===================================================================
              
              #new stuff
              hand <- array(0,c(n,nR))
              
              omega[be,phi,ro,,] <- (solve(diag(1 + as.vector((gamma*hand)%*%(R[be,phi,ro,t,])),n,n)))%*%gamma
              
              #benefit, increasing function of trophic level
              B <- array(0,c(n,nR))
              for (i in 1:n){
                B[i,] <- 10*(beta.bene * yR) / (1 + beta.bene * yR) #
              }
              
              if (all.R.equal==T){
                B[,] <- mean(B) 
              }
              
              
              #===============================================================================
              #Growth-Fecundity Trade-off and change in survival with size
              #===============================================================================
              
              
              eta <- min.surv / (1 + exp( - (0 + eta.z * (y - psi.mat[be])))) 
              
              psi[be,phi,ro,] <- ( 1 / ( 1 + exp( - (psi.0 + psi.z * (y - psi.mat[be]) ) ) ) )
              
             
              #===============================================================================
              #Demographic rates for resident consumer
              #===============================================================================
              
              H.s <- kappa.s * (1 - eta) * (1 - psi[be,phi,ro,]) * delta
              u <-  beta.s + H.s * ( (B * omega[be,phi,ro,,] * theta[be,phi,ro,,]) %*% R[be,phi,ro,t,])
              
              s <- invlogit(0.55 + 0.1*y) / (1 + exp(-u))
              
              H.g <- kappa.g * psi[be,phi,ro,] * (1 - eta) * delta
              g <-  ((10^(-4)*y^2.7 + H.g * ( (B * omega[be,phi,ro,,]  * theta[be,phi,ro,,]) %*% R[be,phi,ro,t,]) )/10^(-4))^(1/2.7) - y
              g2 <- ((10^(-4)*y^2.7 + as.vector(H.g * ( (B * omega[be,phi,ro,,]  * theta[be,phi,ro,,]) %*% K.res) - as.vector(H.g * ((omega[be,phi,ro,,]  * theta[be,phi,ro,,]*B) %*% diag(K.res/(exp(r.res)-1)) %*% t(omega[be,phi,ro,,]  * theta[be,phi,ro,,]))%*%C[be,phi,ro,t,])) ) / 10^(-4))^(1/2.7) - y

              H.m <- kappa.m * ( eta * delta )
              m <-  (H.m *  (B * omega[be,phi,ro,,]  * theta[be,phi,ro,,]) %*% R[be,phi,ro,t,])  / 2
              
              #Survival Matrix
              S <- diag(as.vector(s))
              
              #Growth Matrix
              G <- array(NA, c(n,n))
              for(i in 1:n){
                
                G[,i] <- dnorm(y, mean = g[i] + y[i], sd = sqrt(g.var)) * h   
                
              }
              G[n,c((n/2):n)] <- G[n,c((n/2):n)]+(1-colSums(G))[c((n/2):n)]
              
              #fecundity matrix
              M <- diag(as.vector(m))
              
              
              #offspring size matrix
              
              D <- array(NA, c(n,n))
              
              for(i in 1:n){
                
                D[,i] <- dnorm(y, mean = beta.d , sd = sqrt(d.var)) * h   
                
              }
              
              K <- G %*% S + D %*% M %*% S
              
              
              
              
              
              #Project Resources forward in time
              
              R[be,phi,ro,t+1,] <- R[be,phi,ro,t,] * exp(r.res) / (1 + (exp(r.res) - 1) / K.res * R[be,phi,ro,t,] + C[be,phi,ro,t,] %*% (omega[be,phi,ro,,] * theta[be,phi,ro,,]) )
              
              #Project Consumer forward in time
              C[be,phi,ro,t+1,] <- K %*% C[be,phi,ro,t,]
              
              R.trans <- R[be,phi,ro,years,] <- R[be,phi,ro,t+1,]
              C.trans <- C[be,phi,ro,years,] <- C[be,phi,ro,t+1,]
              
              maxt <- t-1
              t <- t+1
              # print(c(round(sum(R[be,phi,ro,t,]),3),round(sum(C[be,phi,ro,t,]),4),maxt))
            }
            R.eq[be,phi,ro,] <- R[be,phi,ro,years,]
            C.eq[be,phi,ro,] <- C[be,phi,ro,years,]
            
            S.eq[be,phi,ro,,] <- S
            G.eq[be,phi,ro,,] <- G
            M.eq[be,phi,ro,,] <- M
            D.eq[be,phi,ro,,] <- D
            K.eq[be,phi,ro,,] <- K
            g.eq[be,phi,ro,] <- g
            s.eq[be,phi,ro,] <- s
            m.eq[be,phi,ro,] <- m
            d.eq[be,phi,ro,] <- beta.d
            
            #===================================================================
            #Invader Consumers
            #===================================================================
            
    
              for (be.inv in 1:length(psi.mat)){
                 for (ro.inv in 1:length(rho.type)){
                    for (phi.inv in 1:length(phi.type)){
           
                    
                    mean.niche.scaled.inv <- sum.params * mean.niche/maxT 
                    theta.inv <- array(NA,c(n,nR))
                    for (i in 1:n){
                      shape <- (mean.niche.scaled.inv + rho.type[ro.inv]*(y[i] - beta.d))
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
                        gamma.inv[i,j] <- exp(phi.int + phi.type[phi.inv]*(y[i]-beta.d)) #/hR
                        
                      }
                    }
                    
                    omega.inv <- (solve(diag(1 + as.vector((gamma.inv*hand)%*%(R.eq[be,phi,ro,])),n,n)))%*%gamma.inv
                    #===============================================================================
                    #Growth-Fecundity Trade-off
                    #===============================================================================
                    eta.inv <- min.surv / (1 + exp( - (0 + eta.z * (y - psi.mat[be.inv])))) 
                    
                    psi.inv <- ( 1 / ( 1 + exp( - (psi.0 + psi.z * (y - psi.mat[be.inv]) ) ) ) )
                    
                    #Demographic Rates for Consumer
                    H.s.inv <-  kappa.s * (1 - eta.inv) * (1 - psi.inv) * delta 
                    u <-  beta.s + H.s.inv * ( (B * omega.inv * theta.inv) %*% R.eq[be,phi,ro,])
                    
                    s.inv <- invlogit(0.55 + 0.1*y) / (1 + exp(-u))
                    
                    H.g.inv <- kappa.g * psi.inv * (1 - eta.inv) * delta
                    g.inv <-  ((10^(-4)*y^2.7 + H.g.inv * ( (B * omega.inv  * theta.inv) %*% R.eq[be,phi,ro,])) / 10^(-4))^(1/2.7) - y
                    
                    
                    H.m.inv <- kappa.m * eta.inv * delta 
                    m.inv <- H.m.inv * ( (B * omega.inv  * theta.inv) %*% R.eq[be,phi,ro,])  / 2
                    
                    
                    #Survival Matrix
                    S.inv <- diag(as.vector(s.inv))
                    
                    #Growth Matrix
                    G.inv <- array(NA, c(n,n))
                    for(i in 1:n){
                      
                      G.inv[,i] <- dnorm(y, mean = g.inv[i] + y[i], sd = sqrt(g.var)) * h   
                      
                    }
                    G.inv[n,c((n/2):n)] <- G.inv[n,c((n/2):n)]+(1-colSums(G.inv))[c((n/2):n)]
                    #fecundity matrix
                    M.inv <- diag(as.vector(m.inv))
                    
                    
                    #offspring size matrix
                    
                    D.inv <- array(NA, c(n,n))
                    
                    for(i in 1:n){
                      
                      D.inv[,i] <- dnorm(y, mean = beta.d , sd = sqrt(d.var)) * h   
                      
                    }
                    
                    K.inv <- G.inv %*% S.inv + D.inv %*% M.inv %*% S.inv
                    
                    invasion[be,be.inv,phi,phi.inv,ro,ro.inv] <- Re(eigen(K.inv)$values[1])
                    

                    }
                  }
                }
         
            print(c(round(psi.mat[be],3),round(phi.type[phi],3),round(rho.type[ro],3),round(sum(R.eq[be,phi,ro,]),3),round(sum(C.eq[be,phi,ro,]),4),maxt))
            }
          }
        }
      
    
  
  
  
 results <-      list(years,maxt,C,R,C.eq,R.eq,S.eq,G.eq, M.eq,D.eq,g.eq,s.eq,m.eq,d.eq,K.eq,invasion,omega,theta,mean.niche,phi.type,rho.type,psi.mat,psi )
 names(results) <- c('years','maxt','C','R','C.eq','R.eq', 'S.eq','G.eq', 'M.eq','D.eq','g.eq','s.eq','m.eq','d.eq','K.eq','invasion','omega','theta','mean.niche','phi.type','rho.type','psi.mat','psi')
return(results)
  
}

