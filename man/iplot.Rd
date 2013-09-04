\name{iPlot}
\alias{iPlot}
\title{Interactive Plot}
\usage{
  iPlot(x = names(data)[1], y = NULL, data,
    vars = names(data)[sapply(data, is.numeric)],
    height = 600, width = 800,
    geom = geom_hist(alpha = 0.3), ...)
}
\arguments{
  \item{x}{variable}

  \item{data}{dataset}

  \item{vars}{filter variables}

  \item{height}{height (app)}

  \item{width}{width (app)}

  \item{...}{see runApp()}
}
\description{
  ...
}
\examples{
\dontrun{
iPlot(x = "Pulse", y = "Exer", data = MASS::survey)
iPlot(x = "Pulse", y = "Smoke", data = MASS::survey, vars = c("Height", "NW.Hnd", "Wr.Hnd"))
}
}

