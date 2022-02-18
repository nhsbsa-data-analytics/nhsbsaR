#' Connect to NHSBSA DB
#'
#' Connect to NHSBSA DB from the Bastion Host. Hopefully you will already have
#' the ODBC driver installed, but if not you may have to raise a service
#' request with IT.
#'
#' @param dsn Default is "FBS_8192k" with Fetch Buffer Size of 8192k created for us
#'   by Platform Services on Azure VDs. If, for some reason, this DSN is unavailable
#'   on your VD, set this to NULL in order to use the default "Oracle in OraClient19Home1"
#'   driver with default settings.
#' @param driver String, ODBC driver to connect to the DB. Default is correct
#'   but an argument is provided in case this ever changes.
#' @param database String, name of the DB - one of c("DALP", "DWCP")).
#' @param username String, username for DB. Default value is
#'   `Sys.getenv(paste0("DB_", database, "_USERNAME"))` and we recommend setting
#'   this in an `.Renviron` file.
#' @param password String, username for DB. Default value is
#'   `Sys.getenv(paste0("DB_", database, "_PASSWORD"))` and we recommend setting
#'   this in an `.Renviron` file.
#'
#' @return Connection to DB. Note: When finished close your connection using
#'   `DBI::dbDisconnect()`
#'
#' @examples
#' # Initialise connection assuming you have a `.Renviron` file with
#' # DB_DALP_USERNAME and DB_DALP_PASSWORD set (recommended)
#' \dontrun{
#'   con <- nhsbsaR::con_nhsbsa(database = "DALP")
#' }
#'
#' # Initialise connection without `.Renviron` file (Note: Never store passwords
#' # in your code)
#' \dontrun{
#'   con <- nhsbsaR::con_nhsbsa(
#'     database = "DWCP",
#'     username = rstudioapi::showPrompt(),
#'     password = rstudioapi::askForPassword()
#'   )
#' }
#'
#'
#' # Boring DBI methods...
#' \dontrun{
#'   DBI::dbGetQuery(conn = con, statement = "SELECT * FROM dual")
#'   DBI::dbReadTable(conn = con, name = "DUAL") # Case sensitive in ODBC
#' }
#'
#'
#' # Cool dbplyr methods... (see https://dbplyr.tidyverse.org)
#' \dontrun{
#'   dplyr::tbl(src = con, from = "DUAL") # Case sensitive in ODBC
#'   dplyr::tbl(src = con, from = dbplyr::in_schema(schema = "SYS", table = "DUAL") # Case sensitive in ODBC
#'   dplyr::tbl(src = con, from = dbplyr::sql("SELECT * FROM dual"))
#' }
#'
#'
#' # Remember to disconnect your session
#' \dontrun{
#'   DBI::dbDisconnect(con)
#' }
#'
#'
#' @export
con_nhsbsa <- function(

  dsn       = "FBS_8192k",
  driver    = "Oracle in OraClient19Home1",
  database,
  username  = Sys.getenv(paste0("DB_", database, "_USERNAME")),
  password  = Sys.getenv(paste0("DB_", database, "_PASSWORD"))
) {

  DBI::dbConnect(
    drv    = odbc::odbc(),
    dsn    = dsn,
    Driver = driver,
    DBQ    = Sys.getenv(paste0("DB_", database, "_CONNECTION_STRING")),
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
#' @param dsn Default is "FBS_8192k" with Fetch Buffer Size of 8192k created for us
#'   by Platform Services on Azure VDs. If, for some reason, this DSN is unavailable
#'   on your VD, set this to NULL in order to use the default "Oracle in OraClient19Home1"
#'   driver with default settings.
#' @param driver String, ODBC driver to connect to the DB. Default is correct
#'   but an argument is provided in case this ever changes.
#' @param database String, name of the DB - one of c("DALP", "DWCP")).
#' @param username String, username for DB. Default value is
#'   `Sys.getenv(paste0("DB_", database, "_USERNAME"))` and we recommend setting
#'   this in an `.Renviron` file.
#' @param password String, username for DB. Default value is
#'   `Sys.getenv(paste0("DB_", database, "_PASSWORD"))` and we recommend setting
#'   this in an `.Renviron` file.
#'
#' @return Pool to DB. Note: When finished close your connection using
#'   `pool::poolClose()`
#'
#' @examples
#' #Initialise pool assuming you have a `.Renviron` file with DB_DALP_USERNAME
#' #and DB_DALP_PASSWORD set (recommended)
#'
#' \dontrun{
#'   pool <- nhsbsaR::pool_nhsbsa(database = "DALP")
#' }
#'
#'
#' # Initialise pool without `.Renviron` file (Note: Never store passwords in
#' # your code)
#' \dontrun{
#'   pool <- nhsbsaR::pool_nhsbsa(
#'     database = "DWCP"
#'     username = rstudioapi::showPrompt(),
#'     password = rstudioapi::askForPassword()
#'   )
#' }
#'
#'
#' # Boring DBI methods...
#' \dontrun{
#'   DBI::dbGetQuery(conn = pool, statement = "SELECT * FROM dual")
#'   DBI::dbReadTable(conn = pool, name = "DUAL") # Case sensitive in ODBC
#' }
#'
#' # Cool dbplyr methods... (see https://dbplyr.tidyverse.org)
#' \dontrun{
#'   dplyr::tbl(src = pool, from = "DUAL") # Case sensitive in ODBC
#'   dplyr::tbl(src = pool, from = dbplyr::in_schema(schema = "SYS", table = "DUAL") # Case sensitive in ODBC
#'   dplyr::tbl(src = pool, from = dbplyr::sql("SELECT * FROM dual"))
#' }
#'
#'
#' # Remember to disconnect your session
#' \dontrun{
#'   pool::poolClose(pool)
#' }
#'
#' @export
pool_nhsbsa <- function(
  dsn       = "FBS_8192k",
  driver    = "Oracle in OraClient19Home1",
  database,
  username  = Sys.getenv(paste0("DB_", database, "_USERNAME")),
  password  = Sys.getenv(paste0("DB_", database, "_PASSWORD"))
) {

  pool::dbPool(
    drv    = odbc::odbc(),
    dsb    = dsn,
    Driver = driver,
    DBQ    = Sys.getenv(paste0("DB_", database, "_CONNECTION_STRING")),
    UID    = username,
    PWD    = password
  )

}
