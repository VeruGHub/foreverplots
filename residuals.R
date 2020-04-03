
myresplot <- function(x,df) { #Home-made residual plot :)
  e <- resid(x, type="pearson")
  f <- fitted(x)
  obs <- x$frame[,1]
  
  par(mfrow=c(1,2), pty="m",mar=c(3,3,1,1),mgp = c(2, 1, 0), ps=12, bty="l")
  plot(f,e, pch=1, cex=0.8, col=alpha(df$c,0.8),
       xlab="Fitted values", ylab="Pearson residuals")
  abline(h = 0, lty = 2, col="black", lw=1)
  plot(f ~ obs, xlab="Observed values", ylab="Fitted values", type="n")
  points(obs,f, pch=1, cex=0.8, col=alpha(df$c,0.8))
  abline(a = 0, b = 1, lty = 2, col="Black", lw=1)
}


#### EXAMPLE ####

library(glmmTMB)

plot_clima <- read.csv("Examples_data/plot_clima.csv")

#myresplot allows to color residuals as a funcion of any variable,
#but it is needed to create "c" variable
plot_clima$c <- ifelse(plot_clima$Cut23==1, "black", "chartreuse3")

m1 <- glmmTMB(ba_ha32nyear~spei23mean_12+(1|tipo), data=plot_clima, 
            family = "gaussian")
summary(m1)

myresplot(m1, plot_clima)

##To do:
#Maybe in the furute could be generalized for other model types 

