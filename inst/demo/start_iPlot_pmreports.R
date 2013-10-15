library(pmreports)
library(extrafontdb)
loadfonts()

Sys.setenv(R_GSCMD = "C:\\Utveckling\\Verktyg\\gswin32.exe")
windowsFonts('Neo Sans Pro' = windowsFont("Neo Sans Pro"))

iPlot(diamonds)