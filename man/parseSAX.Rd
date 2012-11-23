\name{parseSAX}
\alias{parseSAX}
\title{parseSAX}
\description{Function creates time series object from PostgreSQL ts object}
\usage{parseSAX(sstring, t_init = -540)}
\arguments{
  \item{sstring}{string representation of SAX returned by PostgreSQL}
  \item{t_init}{initial measurement time}
}

\value{time series of type ts}

\author{Anatoly Sorokin, Evgenia Temlyakova 2012}





