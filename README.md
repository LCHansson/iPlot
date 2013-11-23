# iPlot

A visualization app using click events for static plots in [shiny](https://github.com/rstudio/shiny) (since 0.7.0).

    library(iPlot)
    iPlot(ggplot2::diamonds)

Example apps on glimmer:

- [iPlot with ggplot2::diamonds](http://glimmer.rstudio.com/reinholdsson/iplot/)
- [iPlot with MASS::survey](http://glimmer.rstudio.com/reinholdsson/iplot-survey/)

[![iPlot](https://dl.dropboxusercontent.com/u/2904328/iPlot.png)](http://glimmer.rstudio.com/reinholdsson/iplot-survey/)

## Installation

    # install.packages("devtools")
    library(devtools)
    install_github("sparkle", "metagraf")
    install_github("iPlot", "SwedishPensionsAgency")
    library(iPlot)

## Instructions

Click on the upper/lower part of the smaller plots to filter the data of the main plot.

## See also

- http://stackoverflow.com/questions/15875786 (thanks Rahul Savani!)
- [Shiny Chart Comments](https://gist.github.com/reinholdsson/6332998)
 
