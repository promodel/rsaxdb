\name{plotTS}
\alias{plotTS}
\title{plotTS}
\description{Function to plot time-serieses from DB}
\usage{plotTS(ts, genes = 1:length(ts), light = c(), title = "Electrostatic potential", 
    norm = FALSE, shift = rep(0, length(ts)), first.is.query = FALSE)}
\arguments{
  \item{ts}{time serieses in database string form as character vector}
  \item{genes}{number of individual lines}
  \item{light}{decoration of graph}
  \item{title}{title of the plot}
  \item{norm}{logical to use normalisation or not}
  \item{shift}{offsets to be added to the time serieces}
  \item{first.is.query}{logical should first line be decorated with bolder line as original query}
}



\author{Anatoly Sorokin, Evgenia Temlyakova 2012}





