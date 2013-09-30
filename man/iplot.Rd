\name{iPlot}
\alias{iPlot}
\title{Interactive Plot}
\usage{
  iPlot(data, height = 600, width = 800,
    geom = geom_density(alpha = 0.3), ...)
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
}
}

