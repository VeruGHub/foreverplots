
library(tidyverse)
library(datasets)

##### Paloma exploratory plot #####

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor*r) # * r) to have coefficients in the same size
}

df <- iris %>% 
  dplyr::select(Sepal.Length:Petal.Width)

pairs(df, diag.panel = panel.hist, 
      upper.panel = panel.smooth, lower.panel = panel.cor,
      na.omit=TRUE)


##### Sophie correlogram #####

library(corrplot)

c <- iris %>% 
  dplyr::select(Sepal.Length:Petal.Width)
  
c2 <- cor(c,method = "pearson")

corrplot(c2, method="circle",type="upper", tl.pos="td", tl.cex=0.8, 
         mar = c(0,1,1,5))


##### Ggpairs <3 #####

library(GGally)

iris %>% 
  ggpairs(aes(color = Species), 
          upper = list(continuous = wrap('cor', size = 3)),
          lower = list(combo = wrap("facethist", bins = 15), 
                       continuous = wrap("smooth_loess", alpha=0.3, size=0.1)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)))







