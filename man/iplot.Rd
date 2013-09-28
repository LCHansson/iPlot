\name{iPlot}
\alias{iPlot}
\title{Interactive Plot}
\usage{
  iPlot(x = names(data)[1], fill = NULL, data,
    height = 600, width = 800,
    geom = geom_density(alpha = 0.3), ...)
}
\arguments{
  \item{x}{variable}

  \item{data}{dataset}

  \item{vars}{filter variables}

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
iPlot(x = "Pulse", fill = "Exer", data = MASS::survey)
iPlot(x = "Pulse", fill = "Smoke", data = MASS::survey, vars = c("Height", "NW.Hnd", "Wr.Hnd"))
iPlot(x = "Height", fill = "Exer", data = MASS::survey, geom = geom_bar())
}
}

