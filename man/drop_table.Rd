% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drop_table.R
\name{drop_table}
\alias{drop_table}
\title{Drop a table from the database}
\usage{
drop_table(table_name, conn = con, purge = TRUE, silent = F)
}
\arguments{
\item{table_name}{name of the table in SQL database}

\item{conn}{connection object created using \code{nhsbsaR::con_nhsbsa()}}

\item{purge}{logical. If \code{TRUE}, releases space associated with the table back to the tablespace for use by other tables/objects. If \code{FALSE}, the table and its dependent objects are placed into recycle bin and continue to count toward the user's space quota. Tables in the recycle bin can be brought back using a \verb{FLASHBACK TABLE} statement.}

\item{silent}{If \code{TRUE}, does not output info messages \verb{Table dropped} or \verb{Table does not exist} to the console.}
}
\description{
Drop a database table in user's own schema.
}
\examples{
\dontrun{
# If connection object is named `con`
drop_table("INT646_TABLE_DB")
# If connection object is not named `con`
drop_table("INT646_TABLE_DB", conn = my_connection_name)
}
}
