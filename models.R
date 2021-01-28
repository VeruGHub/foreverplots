
#For examples:
library(datasets)

#####HOME-MADE RESIDUAL PLOTS#####

myresplot <- function(model,df) { 
  e <- residuals(model, type="pearson")
  f <- fitted(model)
  obs <- model@frame[,1] #This could change according to model type
  
  par(mfrow=c(1,2), pty="m",mar=c(3,3,1,1),mgp = c(2, 1, 0), ps=12, bty="l")
  plot(f,e, pch=1, cex=0.8, col=alpha(df$c,0.8),
       xlab="Fitted values", ylab="Pearson residuals")
  abline(h = 0, lty = 2, col="black", lw=1)
  plot(f ~ obs, xlab="Observed values", ylab="Fitted values", type="n")
  points(obs,f, pch=1, cex=0.8, col=alpha(df$c,0.8))
  abline(a = 0, b = 1, lty = 2, col="Black", lw=1)
}

library(ggplot2)
library(patchwork)
library(performance)

myggresplot <- function(model,df) { 
  dfp <- data.frame(e = resid(model, type="pearson"),
                    f = fitted(model),
                    obs = model@frame[,1]) #This could change according to model type
  m <- lm(dfp$f~dfp$obs)
  r <- round(r2(m)[[1]],3)
  
  a <- ggplot(data= dfp, aes(x=f, y=e)) + 
    geom_point(color=alpha("darkslategrey",0.3)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(color="firebrick", se = FALSE) +
    xlab("Fitted values") + ylab("Pearson residuals") +
    theme_classic()
  b <- ggplot(data= dfp, aes(x=obs, y=f)) + 
    geom_point(color=alpha("darkslategrey",0.3)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_smooth(color="firebrick", se = FALSE) +
    xlab("Observed values") + ylab("Fitted values") +
    theme_classic() +
    annotate(geom="text", x=quantile(dfp$obs, 0.9), y=quantile(dfp$f, 0.2),
             label=bquote(R^{2}~"="~.(r)))
  return(a+b)
}

##To do:
#Maybe in the furute could be generalized for other model types 

#Example

library(lme4)

#myresplot allows to color residuals as a funcion of any variable,
#but it is needed to create "c" variable
iris$c <- ifelse(iris$Species=="setosa", "black", 
                 ifelse(iris$Species=="versicolor", "chartreuse3", 
                        "cornflowerblue"))

m1 <- glmer(Sepal.Length~Sepal.Width+Petal.Length+(1|Species), data=iris, 
            family = "gaussian")
summary(m1)

myresplot(m1, iris)
myggresplot(m1, plot_clima)


####SJPLOT####

library(sjPlot) 
library(sjmisc)

check_model(m1)

check_outliers(m1)
plot(check_outliers(m1))
plot_model(m1, type="pred", terms=c("Sepal.Width","Petal.Length"))


