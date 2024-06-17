#' Execute SQL code on the database
#'
#' Execute native SQL code saved in the script on the database. Will execute one statement per script. If the script contains more than one statement, will execute the first statement (e.g. if the script contains `drop table xxx; create table xxx;`, only the DROP TABLE statement will be executed). Handles the presence of SQL variable declarations and comments (in `/* */` and `--`).
#'
#' @param script path to the SQL script
#' @param conn connection object created using `nhsbsaR::con_nhsbsa()`
#' @param return_output set to TRUE if the script is expected to return an output from a SELECT statement.
#'
#' @examples
#' \dontrun{
#' con <- nhsbsaR::con_nhsbsa(database = "DALP")
#'
#' # E.g. a create table statement
#' run_sql("INTXXX_BASE_TABLE.sql")
#'
#' # E.g. a select statement
#' df <- run_sql("INTXXX_SUMMARY_DATA.sql", return_output = T)
#' }
#' @export
run_sql = function(script, conn = con, return_output = F) {

  clean_script <- readr::read_lines(script) |>
    stringr::str_c(sep = " ", collapse = "\n") |>
    gsub(pattern = "/\\*.*?\\*/", replacement = " ") |> # remove /*  */ comments
    gsub(pattern = "--[^\r\n]*", replacement = " ") |> # remove -- comments
    gsub(pattern = "define[^\r\n]*", replacement = " ") |> # remove variable declarations
    gsub(pattern = ";.*", replacement = " ") |> # remove the query-end semicolon and everything after it
    gsub(pattern = "[\r\n\t\f\v]", replacement = " ") |> # remove line breaks, tabs, etc.
    gsub(pattern = " +", replacement = " ") # remove extra whitespaces

    if (return_output)
      DBI::dbGetQuery(conn, statement = clean_script)
    else {
      DBI::dbExecute(conn, statement = clean_script) |> capture.output(file = nullfile())
    }

}
