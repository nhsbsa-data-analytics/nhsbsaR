#' Connect to NHSBSA DB
#'
#' Connect to NHSBSA DB from the Bastion Host. Hopefully you will already have
#' the ODBC driver installed, but if not you may have to raise a service
#' request with IT.
#'
#' @param driver String, ODBC driver to connect to the DB. Default is correct
#'   but an argument is provided in case this ever changes.
#' @param database String, name of the DB.
#' @param username String, username for DB. Default value is
#'   `Sys.getenv("DB_USERNAME")` and we recommend setting this in an
#'   `.Renviron` file.
#' @param password String, username for DB. Default value is
#'   `Sys.getenv("DB_PASSWORD")` and we recommend setting this in an
#'   `.Renviron` file.
#'
#' @return Connection to DB. Note: When finished close your connection using
#'   `DBI::dbDisconnect()`
#'
#' @examples
#' # Initialise connection assuming you have a `.Renviron` file with DB_USERNAME
#' # and DB_PASSWORD set (recommended)
#' con <- nhsbsR::con_nhsbsa()
#' con <- nhsbsR::con_nhsbsa(database = "DWCP")
#'
#' # Initialise connection without `.Renviron` file (Note: Never store passwords
#' # in your code)
#' con <- nhsbsR::con_nhsbsa(
#'   username = rstudioapi::showPrompt(),
#'   password = rstudioapi::askForPassword()
#' )
#'
#' # Boring DBI methods...
#' DBI::dbGetQuery(conn = con, statement = "SELECT * FROM dual")
#' DBI::dbReadTable(conn = con, name = "DUAL") # Case sensitive in ODBC
#'
#' # Cool dbplyr methods... (see https://dbplyr.tidyverse.org)
#' dplyr::tbl(src = con, from = "DUAL") # Case sensitive in ODBC
#' dplyr::tbl(src = con, from = dbplyr::in_schema(schema = "SYS", table = "DUAL") # Case sensitive in ODBC
#' dplyr::tbl(src = con, from = dbplyr::sql("SELECT * FROM dual"))
#'
#' # Remember to disconnect your session
#' DBI::dbDisconnect(con)
#'
#' @export
con_nhsbsa <- function(driver    = "Oracle in OraClient19Home1",
                       database,
                       username  = Sys.getenv("DB_USERNAME"),
                       password  = Sys.getenv("DB_PASSWORD")) {
  
  DBI::dbConnect(
    drv    = odbc::odbc(),
    Driver = driver,
    DBQ    = Sys.getenv(paste0("CONNECTION_STRING_", database)),
    UID    = username,
    PWD    = password
  )
  
}


#' Pool to NHSBSA DB
#'
#' Pool to NHSBSA DB from the Bastion Host. This is particularly useful in
#' shiny apps but can be used as a general alternative to `con_nhsbsa`.
#' Hopefully you will already have the ODBC driver installed, but if not you may
#' have to raise a service request with IT.
#'
#' @param driver String, ODBC driver to connect to the DB. Default is correct
#'   but an argument is provided in case this ever changes.
#' @param server String, server address of the DB. Default value is correct but
#'   an argument is provided in case this ever changes.
#' @param port Numeric, port of the DB. Default value is correct but an argument
#'   is provided in case this ever changes.
#' @param database String, name of the DB. Default value is "DALP.WORLD".
#' @param username String, username for DB. Default value is
#'   `Sys.getenv("DB_USERNAME")` and we recommend setting this in an
#'   `.Renviron` file.
#' @param password String, username for DB. Default value is
#'   `Sys.getenv("DB_PASSWORD")` and we recommend setting this in an
#'   `.Renviron` file.
#'
#' @return Pool to DB. Note: When finished close your connection using
#'   `pool::poolClose()`
#'
#' @examples
#' # Initialise pool assuming you have a `.Renviron` file with DB_USERNAME
#' # and DB_PASSWORD set (recommended)
#' pool <- nhsbsR::pool_nhsbsa()
#' pool <- nhsbsR::pool_nhsbsa(database = "DWCP")
#'
#' # Initialise pool without `.Renviron` file (Note: Never store passwords in
#' # your code)
#' pool <- nhsbsR::pool_nhsbsa(
#'   username = rstudioapi::showPrompt(),
#'   password = rstudioapi::askForPassword()
#' )
#'
#' # Boring DBI methods...
#' DBI::dbGetQuery(conn = pool, statement = "SELECT * FROM dual")
#' DBI::dbReadTable(conn = pool, name = "DUAL") # Case sensitive in ODBC
#'
#' # Cool dbplyr methods... (see https://dbplyr.tidyverse.org)
#' dplyr::tbl(src = pool, from = "DUAL") # Case sensitive in ODBC
#' dplyr::tbl(src = pool, from = dbplyr::in_schema(schema = "SYS", table = "DUAL") # Case sensitive in ODBC
#' dplyr::tbl(src = pool, from = dbplyr::sql("SELECT * FROM dual"))
#'
#' # Remember to disconnect your session
#' pool::poolClose(pool)
#'
#' @export
pool_nhsbsa <- function(driver    = "Oracle in OraClient19Home1",
                        database,
                        username  = Sys.getenv("DB_USERNAME"),
                        password  = Sys.getenv("DB_PASSWORD")) {
  
  pool::dbPool(
    drv    = odbc::odbc(),
    Driver = driver,
    DBQ    = Sys.getenv(paste0("CONNECTION_STRING_", database)),
    UID    = username,
    PWD    = password
  )
  
}
