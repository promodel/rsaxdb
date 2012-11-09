\name{parseTS}
\alias{parseTS}
\title{parseTS}
\description{Function creates time series object from PostgreSQL ts object}
\usage{parseTS(string, t_init = -540)}
\arguments{
  \item{string}{string representation returned by PostgreSQL}
  \item{t_init}{initial measurement time}
}

\value{time series of type ts}

\author{Anatoly Sorokin, Evgenia Temlyakova 2012}





