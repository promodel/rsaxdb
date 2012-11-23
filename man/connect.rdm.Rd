\name{connect.rdm}
\alias{connect.rdm}
\title{connect rdm}
\description{Open connection to RobustDM database}
\usage{connect.rdm(dbname = "kappaprom", port = "10000", host = if (!is.null(Sys.getenv("PGHOST"))) Sys.getenv("PGHOST") else "localhost", 
    ...)}
\arguments{
  \item{dbname}{name of the database! Should be changed before distribution!!!!}
  \item{port}{port at which database is listening! Should be changed before distribution, standard PostgreSQL port is 56!!!!}
  \item{host}{database server name, URL or IP}
  \item{\dots}{other authorization arguments needed by the DBMS instance; these typically include user, password, dbname, host, port, etc. For details see the PostgreSQL DBIDriver.}
}

\value{connection object see }

\author{Anatoly Sorokin, Evgenia Temlyakova 2012}





