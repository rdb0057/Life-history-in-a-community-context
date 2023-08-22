

##############################################################################################################
#Invasion plots
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
plot11 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab=ylab,
                    par.settings = theme.novpadding.left) + 
          as.layer(xyplot(maximum11.base ~ maximum11.base, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df1[which(lamb.inv.df1$phi.res==phi.type[2] & lamb.inv.df1$phi.inv==phi.type[2] & lamb.inv.df1$ro.res==rho.type[1] & lamb.inv.df1$ro.inv==rho.type[1] ),]
plot12 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum21.base ~ maximum21.base, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df1[which(lamb.inv.df1$phi.res==phi.type[1] & lamb.inv.df1$phi.inv==phi.type[1] & lamb.inv.df1$ro.res==rho.type[2] & lamb.inv.df1$ro.inv==rho.type[2] ),]
plot13 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum12.base ~ maximum12.base, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df1[which(lamb.inv.df1$phi.res==phi.type[2] & lamb.inv.df1$phi.inv==phi.type[2] & lamb.inv.df1$ro.res==rho.type[2] & lamb.inv.df1$ro.inv==rho.type[2] ),]
plot14 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab='',
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(6 ~ 6, pch=19,col = 'black',cex=2))+ 
  as.layer(xyplot(1 ~ 1, pch=19,col = 'black',cex=2))+ 
  as.layer(xyplot(3 ~ 3, pch=1,col = 'black',cex=2))




toplot <- lamb.inv.df2[which(lamb.inv.df2$phi.res==phi.type[1] & lamb.inv.df2$phi.inv==phi.type[1] & lamb.inv.df2$ro.res==rho.type[1] & lamb.inv.df2$ro.inv==rho.type[1] ),]
plot21 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum11.2 ~ maximum11.2, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df2[which(lamb.inv.df2$phi.res==phi.type[2] & lamb.inv.df2$phi.inv==phi.type[2] & lamb.inv.df2$ro.res==rho.type[1] & lamb.inv.df2$ro.inv==rho.type[1] ),]
plot22 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum21.2 ~ maximum21.2, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df2[which(lamb.inv.df2$phi.res==phi.type[1] & lamb.inv.df2$phi.inv==phi.type[1] & lamb.inv.df2$ro.res==rho.type[2] & lamb.inv.df2$ro.inv==rho.type[2] ),]
plot23 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum12.2 ~ maximum12.2, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df2[which(lamb.inv.df2$phi.res==phi.type[2] & lamb.inv.df2$phi.inv==phi.type[2] & lamb.inv.df2$ro.res==rho.type[2] & lamb.inv.df2$ro.inv==rho.type[2] ),]
plot24 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab='',
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum22.2 ~ maximum22.2, pch=19,col = 'black',cex=2))



toplot <- lamb.inv.df3[which(lamb.inv.df3$phi.res==phi.type[1] & lamb.inv.df3$phi.inv==phi.type[1] & lamb.inv.df3$ro.res==rho.type[1] & lamb.inv.df3$ro.inv==rho.type[1] ),]
plot31 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum11.3 ~ maximum11.3, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df3[which(lamb.inv.df3$phi.res==phi.type[2] & lamb.inv.df3$phi.inv==phi.type[2] & lamb.inv.df3$ro.res==rho.type[1] & lamb.inv.df3$ro.inv==rho.type[1] ),]
plot32 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum21.3 ~ maximum21.3, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df3[which(lamb.inv.df3$phi.res==phi.type[1] & lamb.inv.df3$phi.inv==phi.type[1] & lamb.inv.df3$ro.res==rho.type[2] & lamb.inv.df3$ro.inv==rho.type[2] ),]
plot33 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab="",
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum12.3 ~ maximum12.3, pch=19,col = 'black',cex=2))


toplot <- lamb.inv.df3[which(lamb.inv.df3$phi.res==phi.type[2] & lamb.inv.df3$phi.inv==phi.type[2] & lamb.inv.df3$ro.res==rho.type[2] & lamb.inv.df3$ro.inv==rho.type[2] ),]
plot34 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab='',
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum22.3 ~ maximum22.3, pch=19,col = 'black',cex=2))






toplot <- lamb.inv.df4[which(lamb.inv.df4$phi.res==phi.type[1] & lamb.inv.df4$phi.inv==phi.type[1] & lamb.inv.df4$ro.res==rho.type[1] & lamb.inv.df4$ro.inv==rho.type[1] ),]
plot41 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab=ylab,
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum11.4 ~ maximum11.4, pch=19,col = 'black',cex=2))



toplot <- lamb.inv.df4[which(lamb.inv.df4$phi.res==phi.type[2] & lamb.inv.df4$phi.inv==phi.type[2] & lamb.inv.df4$ro.res==rho.type[1] & lamb.inv.df4$ro.inv==rho.type[1] ),]
plot42 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum21.4 ~ maximum21.4, pch=19,col = 'black',cex=2))



toplot <- lamb.inv.df4[which(lamb.inv.df4$phi.res==phi.type[1] & lamb.inv.df4$phi.inv==phi.type[1] & lamb.inv.df4$ro.res==rho.type[2] & lamb.inv.df4$ro.inv==rho.type[2] ),]
plot43 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum12.4 ~ maximum12.4, pch=19,col = 'black',cex=2))



toplot <- lamb.inv.df4[which(lamb.inv.df4$phi.res==phi.type[2] & lamb.inv.df4$phi.inv==phi.type[2] & lamb.inv.df4$ro.res==rho.type[2] & lamb.inv.df4$ro.inv==rho.type[2] ),]
plot44 <- levelplot(toplot[,c('lambda')]~toplot[,c('eta.type.res')]*toplot[,c('eta.type.inv')],
                    colorkey=FALSE,
                    contour=FALSE,
                    col.regions=c('darkgrey','white'),cuts=1, #Need to adjust this based on the results.
                    scales = list(tck = c(1,0),cex=0.5),
                    xlab=xlab,
                    ylab='',
                    par.settings = theme.novpadding.left)+ 
  as.layer(xyplot(maximum22.4 ~ maximum22.4, pch=19,col = 'black',cex=2))+ 
  as.layer(xyplot(6 ~ 6, pch=19,col = 'black',cex=2))+ 
  as.layer(xyplot(4 ~ 4, pch=1,col = 'black',cex=2))







