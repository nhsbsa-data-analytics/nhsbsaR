#' Read and write lakehouse tables via OneLake API / REST API
#'
#' @description
#' These functions utilise a combination of OneLake API and REST API to read lakehouse tables into local R data frames and write local R data frames into lakehouse tables.
#' A Spark session is NOT required.
#'
#' @details
#' You need the `FABRICQUERYR_TENANT_ID` environment variable set in `.Renviron`. This is an identifier of NHSBSA's corporate account with Fabric. Can be obtained from <https://app.fabric.microsoft.com> -> Profile icon -> Exclamation mark near Tenant Name.
#'
#' You also need `FABRIC_USERNAME` and `FABRIC_password` environment variables, which are likely already in place for use in `con_nhsbsa_fabric()`.
#'
#' `nhsbsa_fabric_write_table()` depends on package \pkg{arrow} and uploads Parquet files to a staging area under `Files/` via OneLake API, then converts them to a Delta table using REST API and cleans up the staging area.
#'
#' `nhsbsa_fabric_read_table()` depends on package \pkg{nanoarrow} and a local Python environment, and uses OneLake API only.
#' Ensure `reticulate` is pointed at the appropriate Python instance, e.g.
#'
#' `Sys.setenv(RETICULATE_PYTHON = "C:/ProgramData/Anaconda3/python.exe")`
#'
#' ...and that the Python environment has the required dependencies:
#'
#' `system2("C:/ProgramData/Anaconda3/python.exe", c("-m", "pip", "install", "deltalake==1.6.2", "nanoarrow==0.8.0"))`
#'
#' Verify the above has worked: `fabricQueryR::fabric_delta_config(initialize = TRUE)`
#'
#' @return
#' `nhsbsa_fabric_read_table()` returns a tibble.
#'
#' @param lakehouse Character. Lakehouse name.
#' @param schema Character. Lakehouse schema. Specify explicitly (best practice) or omit to use lakehouse's default schema (most commonly `dbo`).
#' @param table Character. Table name.
#'
#' @examples
#' \dontrun{
#' nhsbsa_fabric_write_table(
#'   lakehouse = "dsaas_int999_xxx",
#'   schema = "dbo",
#'   table = "mtcars",
#'   data = mtcars
#'   )
#'
#' df <- nhsbsa_fabric_read_table(
#'   lakehouse = "dsaas_int999_xxx",
#'   schema = "dbo",
#'   table = "mtcars"
#'   )
#' }
#'
#' @name nhsbsa_fabric_read_write_tables_via_OneLake_and_REST_API
#' @export
nhsbsa_fabric_read_table <- function(
    lakehouse,
    schema = NULL,
    table
    ) {

  lakehouse <- fabricQueryR::fabric_catalog_search(search = lakehouse, types = "Lakehouse")[[1]]

  fabricQueryR::fabric_lakehouse_read_table(
    lakehouse = lakehouse,
    schema = schema,
    table = table,
    auth_args = list(
      auth_type = "resource_owner",
      username  = Sys.getenv("FABRIC_USERNAME"),
      password  = Sys.getenv("FABRIC_PASSWORD")
    )
  )

}

#' @rdname nhsbsa_fabric_read_write_tables_via_OneLake_and_REST_API
#' @param data tibble or data.frame to be written to the lakehouse.
#' @param mode Character. `"Overwrite"` or `"Append"`.
#' @export
nhsbsa_fabric_write_table <- function(
    lakehouse,
    schema = NULL,
    table,
    data,
    mode = "Overwrite"
) {

  lakehouse <- fabricQueryR::fabric_catalog_search(search = lakehouse, types = "Lakehouse")[[1]]

  res <- fabricQueryR::fabric_lakehouse_write_table(
    lakehouse = lakehouse,
    schema = schema,
    table = table,
    data = data,
    mode = mode,
    auth_args = list(
      auth_type = "resource_owner",
      username  = Sys.getenv("FABRIC_USERNAME"),
      password  = Sys.getenv("FABRIC_PASSWORD")
    )
  )

  if (res$operation_status$status == "Succeeded") {

  target <- if (is.null(schema)) table else paste(schema, table, sep = ".")
  mode <- if (mode == "Overwrite") "(over)written" else if (mode == "Append") "appended"
  rows <- format(res$rows, big.mark = ",", scientific = FALSE)

  cli::cli_alert_success("{target} {mode} successfully: {rows} rows")

  } else res$operation_status$error

}
