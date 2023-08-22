

#Create the Function
inter.coef <- function(terms){
  
  
  lam.inv <- array(NA,c(length(eta.type),length(eta.type),length(phi.type),length(rho.type)))
  N <- array(NA,c(length(eta.type),length(phi.type),length(rho.type),years,n))
  s.eq <- array(NA,c(3,length(eta.type),length(phi.type),length(rho.type),n))
  N.eq <- array(NA,c(3,length(eta.type),length(phi.type),length(rho.type),n))
  N[,,,1,] <- 1
  
  
  
  for (phi in 1:length(phi.type)){
    for (ro in 1:length(rho.type)){
      for (b in 1:length(eta.type)){
        
        
        t <- 1
        while (t < c(years - 1) & (( isTRUE(all.equal(N[b,phi,ro,t,],N[b,phi,ro,t-1,]))==FALSE  ))==TRUE ){
          
          #Survival Matrix
          eta <- rep(0,n)
          for (ii in 1:n){
            if (ii>=b){
              eta[ii] <- (1 - min.surv)
            }
          }
          eta.0 <- rep((1 - min.surv),n)
          
          
          #benefit, increasing function of trophic level
          B <- array(0,c(n,nR))
          for (i in 1:n){
            B[i,] <- 10*(beta.bene * yR) / (1 + beta.bene * yR) #
          }
          
          if (all.R.equal==T){
            B <- mean(B) 
          }
          
          #Survival
          H.s <- kappa.s * (1-eta) * delta
          H.s.0 <- kappa.s * (1-eta.0) * delta
          
          if(terms=='all'){
            u <-  beta.s + 
              H.s * B * mean(K.res) * rowSums(gamma[b,phi,ro,,]*theta[b,phi,ro,,]) - 
              (H.s * B * mean(K.res) ) / (exp(mean(r.res)) - 1) * 
              (((gamma[b,phi,ro,,]*theta[b,phi,ro,,]) %*% t( gamma[b,phi,ro,,]*theta[b,phi,ro,,])) %*% N[b,phi,ro,t,] )
            
          }
          if(terms=='di'){
            u <-  beta.s + 
              H.s * B * mean(K.res) * rowSums(gamma[b,phi,ro,,]*theta[b,phi,ro,,]) - 
              (H.s * B * mean(K.res) ) / (exp(mean(r.res)) - 1) * 
              (((gamma[b,1,1,,]*theta[b,1,1,,]) %*% t( gamma[b,1,1,,]*theta[b,1,1,,])) %*% N[b,phi,ro,t,] )
            
          }
          if(terms=='dd'){
            u <-  beta.s + 
              H.s * B * mean(K.res) * rowSums(gamma[b,1,1,,]*theta[b,1,1,,]) - 
              (H.s * B * mean(K.res) ) / (exp(mean(r.res)) - 1) * 
              (((gamma[b,phi,ro,,]*theta[b,phi,ro,,]) %*% t( gamma[b,phi,ro,,]*theta[b,phi,ro,,])) %*% N[b,phi,ro,t,] )
            
          }

          s <- invlogit(1.15 + 0.4*(a-1)) / (1 + exp(-u))
          
          
          S <- diag(as.vector(s))
          St <- t(array(0,c(n,n)))
          
          for(i in 1:(n-1)){
            St[i+1,i] <- 1
          }
          St[n,c((n-1),n)] <- 1
          
          
          #fecundity matrix
          M <- array(0,c(n,n))
          H.m <- kappa.m * eta * delta 
          H.m.0 <- kappa.m * eta.0 * delta 
          
          if(terms=='all'){
            M[1,] <- 0.5*(H.m * B * mean(K.res) * rowSums(gamma[b,phi,ro,,]*theta[b,phi,ro,,]) - 
                            (H.m * B * mean(K.res) ) / (exp(mean(r.res)) - 1) *  
                            (((gamma[b,phi,ro,,]*theta[b,phi,ro,,]) %*% t( gamma[b,phi,ro,,]*theta[b,phi,ro,,])) %*% N[b,phi,ro,t,] ))
            
          }
          if(terms=='di'){
            M[1,] <- 0.5*(H.m * B * mean(K.res) * rowSums(gamma[b,phi,ro,,]*theta[b,phi,ro,,]) - 
                            (H.m * B * mean(K.res) ) / (exp(mean(r.res)) - 1) *  
                            (((gamma[b,1,1,,]*theta[b,1,1,,]) %*% t( gamma[b,1,1,,]*theta[b,1,1,,])) %*% N[b,phi,ro,t,] ))
            
          }
          if(terms=='dd'){
            M[1,] <- 0.5*(H.m * B * mean(K.res) * rowSums(gamma[b,1,1,,]*theta[b,1,1,,]) - 
                            (H.m * B * mean(K.res) ) / (exp(mean(r.res)) - 1) *  
                            (((gamma[b,phi,ro,,]*theta[b,phi,ro,,]) %*% t( gamma[b,phi,ro,,]*theta[b,phi,ro,,])) %*% N[b,phi,ro,t,] ))
            
          }

          K <- St %*% S +  M %*% S
          
          N[b,phi,ro,t+1,] <- K %*% N[b,phi,ro,t,]
          if(terms=='all'){
            s.eq[1,b,phi,ro,] <- s
            N.eq[1,b,phi,ro,] <- N[b,phi,ro,t+1,]
          }
          if(terms=='dd'){
            s.eq[2,b,phi,ro,] <- s
            N.eq[2,b,phi,ro,] <- N[b,phi,ro,t+1,]
          }
          if(terms=='di'){
            s.eq[3,b,phi,ro,] <- s
            N.eq[3,b,phi,ro,] <- N[b,phi,ro,t+1,]
          }
          maxt <- t-1
          t <- t+1
        }
        print(maxt)
        
        # Invader
        for (b.inv in 1:length(eta.type)){
          
          #Survival Matrix
          eta <- rep(0,n)
          for (ii in 1:n){
            if (ii>=b.inv){
              eta[ii] <- (1 - min.surv)
            }
          }
          eta.0 <- rep((1 - min.surv),n)
 
          #Survival
          H.s <- kappa.s * (1-eta) * delta
          H.s.0 <- kappa.s * (1-eta.0) * delta
          
          if(terms=='all'){
            u <-  beta.s + 
              H.s * B * mean(K.res) * rowSums(gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) - 
              (H.s * B * mean(K.res) ) / (exp(mean(r.res)) - 1) * 
              (((gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) %*% t( gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,])) %*% N[b,phi,ro,t,] )
            
          }
          if(terms=='di'){
            u <-  beta.s + 
              H.s * B * mean(K.res) * rowSums(gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) - 
              (H.s * B * mean(K.res) ) / (exp(mean(r.res)) - 1) * 
              (((gamma[b.inv,1,1,,]*theta[b.inv,1,1,,]) %*% t( gamma[b.inv,1,1,,]*theta[b.inv,1,1,,])) %*% N[b,phi,ro,t,] )
            
          }
          if(terms=='dd'){
            u <-  beta.s + 
              H.s * B * mean(K.res) * rowSums(gamma[b.inv,1,1,,]*theta[b.inv,1,1,,]) - 
              (H.s * B * mean(K.res) ) / (exp(mean(r.res)) - 1) * 
              (((gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) %*% t( gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,])) %*% N[b,phi,ro,t,] )
            
          }
          
          s <- invlogit(1.15 + 0.4*(a-1)) / (1 + exp(-u))
          
          
          S <- diag(as.vector(s))
          St <- t(array(0,c(n,n)))
          
          for(i in 1:(n-1)){
            St[i+1,i] <- 1
          }
          St[n,c((n-1),n)] <- 1
          
          
          #fecundity matrix
          M <- array(0,c(n,n))
          H.m <- kappa.m * eta * delta 
          H.m.0 <- kappa.m * eta.0 * delta 
          if(terms=='all'){
            M[1,] <- 0.5*(H.m * B * mean(K.res) * rowSums(gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) - 
                            (H.m * B * mean(K.res) ) / (exp(mean(r.res)) - 1) *  
                            (((gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) %*% t( gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,])) %*% N[b,phi,ro,t,] ))
            
          }
          if(terms=='di'){
            M[1,] <- 0.5*(H.m * B * mean(K.res) * rowSums(gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) - 
                            (H.m * B * mean(K.res) ) / (exp(mean(r.res)) - 1) *  
                            (((gamma[b.inv,1,1,,]*theta[b.inv,1,1,,]) %*% t( gamma[b.inv,1,1,,]*theta[b.inv,1,1,,])) %*% N[b,phi,ro,t,] ))
            
          }
          if(terms=='dd'){
            M[1,] <- 0.5*(H.m * B * mean(K.res) * rowSums(gamma[b.inv,1,1,,]*theta[b.inv,1,1,,]) - 
                            (H.m * B * mean(K.res) ) / (exp(mean(r.res)) - 1) *  
                            (((gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) %*% t( gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,])) %*% N[b,phi,ro,t,] ))
            
          }
          K <- St %*% S +  M %*% S
          
          lam.inv[b,b.inv,phi,ro] <- Re(eigen(K)$values[1])
          
          if(maxt==years-3){
            
            #Survival
            H.s <- kappa.s * (1-eta) * delta
            H.s.0 <- kappa.s * (1-eta.0) * delta
            
            if(terms=='all'){
              u <-  beta.s + 
                H.s * B * mean(K.res) * rowSums(gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) - 
                (H.s * B * mean(K.res) ) / (exp(mean(r.res)) - 1) * 
                (((gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) %*% t( gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,])) %*% N[b,phi,ro,t-1,] )
              
            }
            if(terms=='di'){
              u <-  beta.s + 
                H.s * B * mean(K.res) * rowSums(gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) - 
                (H.s * B * mean(K.res) ) / (exp(mean(r.res)) - 1) * 
                (((gamma[b.inv,1,1,,]*theta[b.inv,1,1,,]) %*% t( gamma[b.inv,1,1,,]*theta[b.inv,1,1,,])) %*% N[b,phi,ro,t-1,] )
              
            }
            if(terms=='dd'){
              u <-  beta.s + 
                H.s * B * mean(K.res) * rowSums(gamma[b.inv,1,1,,]*theta[b.inv,1,1,,]) - 
                (H.s * B * mean(K.res) ) / (exp(mean(r.res)) - 1) * 
                (((gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) %*% t( gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,])) %*% N[b,phi,ro,t-1,] )
              
            }
            
            s <- invlogit(1.15 + 0.4*(a-1)) / (1 + exp(-u))
            
            
            S <- diag(as.vector(s))
            St <- t(array(0,c(n,n)))
            
            for(i in 1:(n-1)){
              St[i+1,i] <- 1
            }
            St[n,c((n-1),n)] <- 1
            
            
            #fecundity matrix
            M <- array(0,c(n,n))
            H.m <- kappa.m * eta * delta 
            H.m.0 <- kappa.m * eta.0 * delta 
            if(terms=='all'){
              M[1,] <- 0.5*(H.m * B * mean(K.res) * rowSums(gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) - 
                              (H.m * B * mean(K.res) ) / (exp(mean(r.res)) - 1) *  
                              (((gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) %*% t( gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,])) %*% N[b,phi,ro,t-1,] ))
              
            }
            if(terms=='di'){
              M[1,] <- 0.5*(H.m * B * mean(K.res) * rowSums(gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) - 
                              (H.m * B * mean(K.res) ) / (exp(mean(r.res)) - 1) *  
                              (((gamma[b.inv,1,1,,]*theta[b.inv,1,1,,]) %*% t( gamma[b.inv,1,1,,]*theta[b.inv,1,1,,])) %*% N[b,phi,ro,t-1,] ))
              
            }
            if(terms=='dd'){
              M[1,] <- 0.5*(H.m * B * mean(K.res) * rowSums(gamma[b.inv,1,1,,]*theta[b.inv,1,1,,]) - 
                              (H.m * B * mean(K.res) ) / (exp(mean(r.res)) - 1) *  
                              (((gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,]) %*% t( gamma[b.inv,phi,ro,,]*theta[b.inv,phi,ro,,])) %*% N[b,phi,ro,t-1,] ))
              
            }
            K2 <- St %*% S +  M %*% S
            
            lam.inv[b,b.inv,phi,ro] <- Re(eigen(K%*%K2)$values[1])
          }
        }
      }
    }
  }
        results <-  list(lam.inv,N,N.eq,s.eq)
        names(results) <- c('lam.inv','N','N.eq','s.eq')
        return(results)
        

}


#Run change all
results <- inter.coef(terms='all')
lam.inv <- results[['lam.inv']]


xlab <- list(label='Age at Maturity Resident',cex=0.5)
ylab <- list(label='Age at Maturity Invader',cex=0.5)

theme.novpadding.left <-
  list(layout.heights =
         list(top.padding = 0,
              main.key.padding = 0,
              key.axis.padding = 0,
              axis.xlab.padding = -0.5,
              xlab.key.padding = 0,
              key.sub.padding = 0,
              bottom.padding = 0),
       # axis.line = list(col = 0),
       clip =list(panel="off"),
       layout.widths =
         list(left.padding = 0,
              key.ylab.padding = 0,
              ylab.axis.padding = -0.5,
              axis.key.padding = 0,
              right.padding = 0))


theme.novpadding.right <-
  list(layout.heights =
         list(top.padding = 0,
              main.key.padding = 0,
              key.axis.padding = 0,
              axis.xlab.padding = -0.5,
              xlab.key.padding = 0,
              key.sub.padding = 0,
              bottom.padding = 0),
       # axis.line = list(col = 0),
       clip =list(panel="off"),
       layout.widths =
         list(left.padding = 0,
              key.ylab.padding = 0,
              ylab.axis.padding = -0.5,
              axis.key.padding = 0,
              right.padding = 0))

ages <- seq(1,length(a),length.out=length(eta.type))

dimnames(lam.inv) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(rho.type))

lamb.inv.df <- reshape2::melt(lam.inv)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi','ro','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)
lamb.inv.df$eq <- NA

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10



toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[1] & lamb.inv.df$ro==rho.type[1] ),]
plot1 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                   colorkey=FALSE,
                   contour=FALSE,
                   col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                   scales = list(tck = c(1,0),cex=0.5),
                   xlab="",
                   ylab=ylab,
                   par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum11.base ~ maximum11.base, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[2] & lamb.inv.df$ro==rho.type[1] ),]
plot2 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                   colorkey=FALSE,
                   contour=FALSE,
                   col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                   scales = list(tck = c(1,0),cex=0.5),
                   xlab="",
                   ylab="",
                   par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum21.base ~ maximum21.base, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[1] & lamb.inv.df$ro==rho.type[2] ),]
plot3 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                   colorkey=FALSE,
                   contour=FALSE,
                   col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                   scales = list(tck = c(1,0),cex=0.5),
                   xlab="",
                   ylab="",
                   par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum12.base ~ maximum12.base, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[2] & lamb.inv.df$ro==rho.type[2] ),]
plot4 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                   colorkey=FALSE,
                   contour=FALSE,
                   col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                   scales = list(tck = c(1,0),cex=0.5),
                   xlab="",
                   ylab="",
                   par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(6 ~ 6, pch=19,col = 'black',cex=2))+ 
  as.layer(xyplot(1 ~ 1, pch=19,col = 'black',cex=2))+ 
  as.layer(xyplot(4 ~ 4, pch=1,col = 'black',cex=2))






#Run change dd
results <- inter.coef(terms='dd')
lam.inv <- results[['lam.inv']]


ages <- seq(1,length(a),length.out=length(eta.type))

dimnames(lam.inv) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(rho.type))

lamb.inv.df <- reshape2::melt(lam.inv)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi','ro','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)
lamb.inv.df$eq <- NA

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10



toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[1] & lamb.inv.df$ro==rho.type[1] ),]
plot5 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                   colorkey=FALSE,
                   contour=FALSE,
                   col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                   scales = list(tck = c(1,0),cex=0.5),
                   xlab=xlab,
                   ylab=ylab,
                   par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(1 ~ 1, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[2] & lamb.inv.df$ro==rho.type[1] ),]
plot6 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                   colorkey=FALSE,
                   contour=FALSE,
                   col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                   scales = list(tck = c(1,0),cex=0.5),
                   xlab=xlab,
                   ylab="",
                   par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(1 ~ 1, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[1] & lamb.inv.df$ro==rho.type[2] ),]
plot7 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                   colorkey=FALSE,
                   contour=FALSE,
                   col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                   scales = list(tck = c(1,0),cex=0.5),
                   xlab=xlab,
                   ylab='',
                   par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(1 ~ 1, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[2] & lamb.inv.df$ro==rho.type[2] ),]
plot8 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                   colorkey=FALSE,
                   contour=FALSE,
                   col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                   scales = list(tck = c(1,0),cex=0.5),
                   xlab=xlab,
                   ylab="",
                   par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(1 ~ 1, pch=19,col = 'black',cex=2))











#Run change di
results <- inter.coef(terms='di')
lam.inv <- results[['lam.inv']]



ages <- seq(1,length(a),length.out=length(eta.type))

dimnames(lam.inv) <- list(round(c(ages),4),round(c((ages)),4),c(phi.type),c(rho.type))

lamb.inv.df <- reshape2::melt(lam.inv)
names(lamb.inv.df) <- c('eta.type.res','eta.type.inv','phi','ro','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)
lamb.inv.df$eq <- NA

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10



toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[1] & lamb.inv.df$ro==rho.type[1] ),]
plot9 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                   colorkey=FALSE,
                   contour=FALSE,
                   col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                   scales = list(tck = c(1,0),cex=0.5),
                   xlab="",
                   ylab=ylab,
                   par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(1 ~ 1, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[2] & lamb.inv.df$ro==rho.type[1] ),]
plot10 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab="",
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(3 ~ 3, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[1] & lamb.inv.df$ro==rho.type[2] ),]
plot11 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab="",
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(1 ~ 1, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df[which(lamb.inv.df$phi==phi.type[2] & lamb.inv.df$ro==rho.type[2] ),]
plot12 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab="",
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(3 ~ 3, pch=19,col = 'black',cex=2))



# #Some survival plots
# b=1
# plot(ages,s.eq[1,b,1,1,],ylim=c(0,1),bty='L',pch=16,col='black')
# points(ages,s.eq[2,b,1,1,],pch=16,col='red')
# points(ages,s.eq[3,b,1,1,],pch=16,col='yellow')
# 
# plot(ages,s.eq[1,b,2,1,],ylim=c(0,1),bty='L',pch=16,col='black')
# points(ages,s.eq[2,b,2,1,],pch=16,col='red')
# points(ages,s.eq[3,b,2,1,],pch=16,col='yellow')
# 
# plot(ages,s.eq[1,b,1,2,],ylim=c(0,1),bty='L',pch=16,col='black')
# points(ages,s.eq[2,b,1,2,],pch=16,col='red')
# points(ages,s.eq[3,b,1,2,],pch=16,col='yellow')
# 
# plot(ages,s.eq[1,b,2,2,],ylim=c(0,1),bty='L',pch=16,col='black')
# points(ages,s.eq[2,b,2,2,],pch=16,col='red')
# points(ages,s.eq[3,b,2,2,],pch=16,col='yellow')





