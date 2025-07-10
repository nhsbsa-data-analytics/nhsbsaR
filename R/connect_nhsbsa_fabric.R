#' Connect to a NHSBSA Fabric Lakehouse
#'
#' Connect to a NHSBSA Fabric Lakehouse. Hopefully you will already have
#' the ODBC driver installed, but if not you may have to raise a service
#' request with IT.
#'
#' @param driver String, ODBC driver to connect to the DB. Default is correct
#'   but an argument is provided in case this ever changes.
#' @param database String, connection string to the Lakehouse SQL endpoint.
#' @param username String, username for DB. Default value is
#'   `Sys.getenv("FABRIC_USERNAME")` and we recommend setting
#'   this in an `.Renviron` file.
#' @param password String, username for DB. Default value is
#'   `Sys.getenv("FABRIC_PASSWORD")` and we recommend setting
#'   this in an `.Renviron` file.
#'
#' @return Connection to DB. Note: When finished close your connection using
#'   `DBI::dbDisconnect()`
#'
#' @examples
#' # Initialise connection assuming you have a `.Renviron` file with
#' # DB_DALP_USERNAME and DB_DALP_PASSWORD set (recommended)
#' \dontrun{
#'   con <- nhsbsaR::con_nhsbsa_fabric(database = "Example/Lakehouse/Connection")
#' }
#'
#' # Initialise connection without `.Renviron` file (Note: Never store passwords
#' # in your code)
#' \dontrun{
#'   con <- nhsbsaR::con_nhsbsa_fabric(
#'     database = "Example/Lakehouse/Connection",
#'     username = rstudioapi::showPrompt(),
#'     password = rstudioapi::askForPassword()
#'   )
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
con_nhsbsa_fabric <- function(

  driver    = "ODBC Driver 18 for SQL Server",
  database,
  username  = Sys.getenv("FABRIC_USERNAME"),
  password  = Sys.getenv("FABRIC_PASSWORD")
) {

  connection=DBI::dbConnect(
    drv    = odbc::odbc(),
    Driver = driver,
    Server = database,
    Authentication = "ActiveDirectoryPassword",
    UID    = username,
    PWD    = password
  )


}



