\name{iPlot}
\alias{iPlot}
\title{Interactive Plot}
\usage{
  iPlot(data = ggplot2::diamonds, height = 600,
    width = 800, geom = NULL, liveSearchLimit = 7,
    options = list(), ...)
}
\arguments{
  \item{data}{dataset}

  \item{height}{height (app)}

  \item{width}{width (app)}

  \item{geom}{main plot geom function}

  \item{...}{see runApp()}
}
\description{
  ...
}
\examples{
\dontrun{
iPlot(MASS::survey)
iPlot(mtcars, geom = geom_bar())
iPlot(as.data.table(ggplot2::diamonds), geom = geom_density(aes(position="stack")))
}
}

