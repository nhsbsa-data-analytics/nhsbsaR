#' Connect to a NHSBSA Fabric Lakehouse
#'
#' Connect to a NHSBSA Fabric Lakehouse. Hopefully you will already have
#' the ODBC driver installed, but if not you may have to raise a service
#' request with IT.
#'
#' @param sql_analytics_endpoint String, connection string to the Lakehouse SQL
#'   endpoint.
#' @param lakehouse_name String, the name of the Fabric Lakehouse we are trying
#'   to connect to.
#' @param driver String, ODBC driver to connect to the DB. Default is correct
#'   but an argument is provided in case this ever changes.
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
#' # DB_DALP_USERNAME and DB_DALP_PASSWORD set (recommended).
#'
#' \dontrun{
#'   con <- nhsbsaR::con_nhsbsa_fabric(
#'      sql_analytics_endpoint = "Example/Lakehouse/Connection",
#'      lakehouse_name = 'lakehouse-name'
#'   )
#'   result <- DBI::dbGetQuery(conn, "SELECT * FROM table")
#' }
#'
#' # Initialise connection without `.Renviron` file (Note: Never store passwords
#' # in your code)
#' \dontrun{
#'   con <- nhsbsaR::con_nhsbsa_fabric(
#'     sql_analytics_endpoint = "Example/Lakehouse/Connection",
#'     lakehouse_name = 'lakehouse-name',
#'     username = rstudioapi::showPrompt(),
#'     password = rstudioapi::askForPassword()
#'   )
#' }
#'
#' # Remember to disconnect your session
#' \dontrun{
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
con_nhsbsa_fabric <- function(
    sql_analytics_endpoint,
    lakehouse_name,
    driver    = "ODBC Driver 18 for SQL Server",
    username  = Sys.getenv("FABRIC_USERNAME"),
    password  = Sys.getenv("FABRIC_PASSWORD")
) {
  connection <- DBI::dbConnect(
    drv    = odbc::odbc(),
    Driver = driver,
    Server = sql_analytics_endpoint,
    Database = lakehouse_name,  # Required to avoid DSN misinterpretation
    Authentication = "ActiveDirectoryPassword",
    UID    = username,
    PWD    = password
  )
}
