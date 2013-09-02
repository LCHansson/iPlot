# iPlot

A visualization app using click events for static plots in [shiny](https://github.com/rstudio/shiny) (since 0.7.0).

    library(iPlot)
    iPlot("Pulse", "Age", MASS::survey, c("Pulse", "Age", "Height", "NW.Hnd", "Wr.Hnd"))

![iPlot](https://dl.dropboxusercontent.com/u/2904328/iPlot.png)

## Instructions

Click on the upper/lower part of the smaller plots to filter the data of the main plot.

## See also

- http://stackoverflow.com/questions/15875786 (thanks Rahul Savani!)
- [Shiny Chart Comments](https://gist.github.com/reinholdsson/6332998)
