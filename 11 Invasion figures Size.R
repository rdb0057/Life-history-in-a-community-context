


dimnames(invasion) <- list(round(c(psi.mat),4),round(c(psi.mat),4),c(phi.type),c(phi.type),c(rho.type),c(rho.type))
lamb.inv.df <- reshape2::melt(invasion)
names(lamb.inv.df) <- c('size.mat.res','size.mat.inv','phi.res','phi.inv','ro.res','ro.inv','value')

lamb.inv.df$value <- round(lamb.inv.df$value,5)
lamb.inv.df$eq <- NA

lamb.inv.df[which(lamb.inv.df$value>1),'lambda'] <- 10
lamb.inv.df[which(lamb.inv.df$value<=1),'lambda'] <- -10

toplot <- lamb.inv.df[which(lamb.inv.df$phi.res==phi.type[1] & lamb.inv.df$phi.inv==phi.type[1] & lamb.inv.df$ro.res==rho.type[1] & lamb.inv.df$ro.inv==rho.type[1] ),]
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('size.mat.res'),summarise,test=sum(test))
test[which(test$test==length(psi.mat)),]
test$max <- length(psi.mat) - test$test
maximum11 <- which.min(test$max)

toplot <- lamb.inv.df[which(lamb.inv.df$phi.res==phi.type[2] & lamb.inv.df$phi.inv==phi.type[2] & lamb.inv.df$ro.res==rho.type[1] & lamb.inv.df$ro.inv==rho.type[1] ),]
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('size.mat.res'),summarise,test=sum(test))
test[which(test$test==length(psi.mat)),]
test$max <- length(psi.mat) - test$test
maximum21 <- which.min(test$max)

toplot <- lamb.inv.df[which(lamb.inv.df$phi.res==phi.type[1] & lamb.inv.df$phi.inv==phi.type[1] & lamb.inv.df$ro.res==rho.type[2] & lamb.inv.df$ro.inv==rho.type[2] ),]
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('size.mat.res'),summarise,test=sum(test))
test[which(test$test==length(psi.mat)),]
test$max <- length(psi.mat) - test$test
maximum12 <- which.min(test$max)

toplot <- lamb.inv.df[which(lamb.inv.df$phi.res==phi.type[2] & lamb.inv.df$phi.inv==phi.type[2] & lamb.inv.df$ro.res==rho.type[2] & lamb.inv.df$ro.inv==rho.type[2] ),]
toplot$test <- 0
toplot[which(toplot$value<=1),'test'] <- 1
test <- ddply(toplot,c('size.mat.res'),summarise,test=sum(test))
test[which(test$test==length(psi.mat)),]
test$max <- length(psi.mat) - test$test
maximum22 <- which.min(test$max)


