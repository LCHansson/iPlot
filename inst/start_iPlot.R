require(ggplot2)
require(tools)
require(sparkle)
require(ggthemes)
require(shiny)
require(xtable)

iPlot(diamonds)



# 
# testdata <- data.table(
#   categ0 = letters[1],
#   cont1 = runif(1e7,0,1e5),
#   categ1 = letters[1:20],
#   cont2 = rnorm(1e7,0,10),
#   categ2 = letters[runif(1e7,1,26)]
# )
# iPlot(testdata)


load("C:/Utveckling/GARP-analys/.RData")
rm(inkdata)
rm(allaink2)
gc()

inkdata <- webapp_ink[,names(webapp_ink)[sapply(names(webapp_ink), function(i) all(!is.na(webapp_ink[[i]])))],with=F]
iPlot(inkdata)
