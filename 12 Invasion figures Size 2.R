
##############################################################################################################
#3. Calculate which size.mat is maximum
##############################################################################################################


##############################################################################################################
#Invasion plots, Column 1
##############################################################################################################
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



toplot <- lamb.inv.df1[which(lamb.inv.df1$phi.res==phi.type[1] & lamb.inv.df1$phi.inv==phi.type[1] & lamb.inv.df1$ro.res==rho.type[1] & lamb.inv.df1$ro.inv==rho.type[1] ),]
plot11 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab=ylab,
                    par.settings = theme.novpadding.left) + 
          as.layer(xyplot(psi.mat[maximum11.base] ~ psi.mat[maximum11.base], pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df1[which(lamb.inv.df1$phi.res==phi.type[2] & lamb.inv.df1$phi.inv==phi.type[2] & lamb.inv.df1$ro.res==rho.type[1] & lamb.inv.df1$ro.inv==rho.type[1] ),]
plot12 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum21.base] ~ psi.mat[maximum21.base], pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df1[which(lamb.inv.df1$phi.res==phi.type[1] & lamb.inv.df1$phi.inv==phi.type[1] & lamb.inv.df1$ro.res==rho.type[2] & lamb.inv.df1$ro.inv==rho.type[2] ),]
plot13 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(15.709459 ~ 15.709459, pch=1,col = 'black',cex=2))+ 
  as.layer(xyplot(min(psi.mat) ~ min(psi.mat), pch=16,col = 'black',cex=2))+ 
  as.layer(xyplot(max(psi.mat) ~ max(psi.mat), pch=16,col = 'black',cex=2))


toplot <- lamb.inv.df1[which(lamb.inv.df1$phi.res==phi.type[2] & lamb.inv.df1$phi.inv==phi.type[2] & lamb.inv.df1$ro.res==rho.type[2] & lamb.inv.df1$ro.inv==rho.type[2] ),]
plot14 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab='',
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum22.base] ~ psi.mat[maximum22.base], pch=19,col = 'black',cex=2))




toplot <- lamb.inv.df2[which(lamb.inv.df2$phi.res==phi.type[1] & lamb.inv.df2$phi.inv==phi.type[1] & lamb.inv.df2$ro.res==rho.type[1] & lamb.inv.df2$ro.inv==rho.type[1] ),]
plot21 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum11.2] ~ psi.mat[maximum11.2], pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df2[which(lamb.inv.df2$phi.res==phi.type[2] & lamb.inv.df2$phi.inv==phi.type[2] & lamb.inv.df2$ro.res==rho.type[1] & lamb.inv.df2$ro.inv==rho.type[1] ),]
plot22 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum21.2] ~ psi.mat[maximum21.2], pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df2[which(lamb.inv.df2$phi.res==phi.type[1] & lamb.inv.df2$phi.inv==phi.type[1] & lamb.inv.df2$ro.res==rho.type[2] & lamb.inv.df2$ro.inv==rho.type[2] ),]
plot23 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum12.2] ~ psi.mat[maximum12.2], pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df2[which(lamb.inv.df2$phi.res==phi.type[2] & lamb.inv.df2$phi.inv==phi.type[2] & lamb.inv.df2$ro.res==rho.type[2] & lamb.inv.df2$ro.inv==rho.type[2] ),]
plot24 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab='',
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum22.2] ~ psi.mat[maximum22.2], pch=19,col = 'black',cex=2))



toplot <- lamb.inv.df3[which(lamb.inv.df3$phi.res==phi.type[1] & lamb.inv.df3$phi.inv==phi.type[1] & lamb.inv.df3$ro.res==rho.type[1] & lamb.inv.df3$ro.inv==rho.type[1] ),]
plot31 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum11.3] ~ psi.mat[maximum11.3], pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df3[which(lamb.inv.df3$phi.res==phi.type[2] & lamb.inv.df3$phi.inv==phi.type[2] & lamb.inv.df3$ro.res==rho.type[1] & lamb.inv.df3$ro.inv==rho.type[1] ),]
plot32 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum21.3] ~ psi.mat[maximum21.3], pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df3[which(lamb.inv.df3$phi.res==phi.type[1] & lamb.inv.df3$phi.inv==phi.type[1] & lamb.inv.df3$ro.res==rho.type[2] & lamb.inv.df3$ro.inv==rho.type[2] ),]
plot33 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum12.3] ~ psi.mat[maximum12.3], pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df3[which(lamb.inv.df3$phi.res==phi.type[2] & lamb.inv.df3$phi.inv==phi.type[2] & lamb.inv.df3$ro.res==rho.type[2] & lamb.inv.df3$ro.inv==rho.type[2] ),]
plot34 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab='',
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(20.472973 ~ 20.472973, pch=1,col = 'black',cex=2))+ 
  as.layer(xyplot(min(psi.mat) ~ min(psi.mat), pch=16,col = 'black',cex=2))+ 
  as.layer(xyplot(max(psi.mat) ~ max(psi.mat), pch=16,col = 'black',cex=2))






toplot <- lamb.inv.df4[which(lamb.inv.df4$phi.res==phi.type[1] & lamb.inv.df4$phi.inv==phi.type[1] & lamb.inv.df4$ro.res==rho.type[1] & lamb.inv.df4$ro.inv==rho.type[1] ),]
plot41 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum11.4] ~ psi.mat[maximum11.4], pch=19,col = 'black',cex=2))



toplot <- lamb.inv.df4[which(lamb.inv.df4$phi.res==phi.type[2] & lamb.inv.df4$phi.inv==phi.type[2] & lamb.inv.df4$ro.res==rho.type[1] & lamb.inv.df4$ro.inv==rho.type[1] ),]
plot42 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum21.4] ~ psi.mat[maximum21.4], pch=19,col = 'black',cex=2))



toplot <- lamb.inv.df4[which(lamb.inv.df4$phi.res==phi.type[1] & lamb.inv.df4$phi.inv==phi.type[1] & lamb.inv.df4$ro.res==rho.type[2] & lamb.inv.df4$ro.inv==rho.type[2] ),]
plot43 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum12.4] ~ psi.mat[maximum12.4], pch=19,col = 'black',cex=2))



toplot <- lamb.inv.df4[which(lamb.inv.df4$phi.res==phi.type[2] & lamb.inv.df4$phi.inv==phi.type[2] & lamb.inv.df4$ro.res==rho.type[2] & lamb.inv.df4$ro.inv==rho.type[2] ),]
plot44 <- levelplot(toplot[,c('lambda')]~toplot[,c('size.mat.res')]*toplot[,c('size.mat.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(psi.mat[maximum22.4] ~ psi.mat[maximum22.4], pch=19,col = 'black',cex=2))







