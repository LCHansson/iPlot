\name{iplot}
\alias{iplot}
\title{Interactive Plot}
\usage{
  iplot(x, y, data, vars, ...)
}
\arguments{
  \item{data}{dataset}

  \item{vars}{filter variables}

  \item{...}{see runApp()}
}
\description{
  ...
}
\examples{
iplot("Pulse", "Age", MASS::survey, c("Pulse", "Age"))
}

