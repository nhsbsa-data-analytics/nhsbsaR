#' Execute queries in a Fabric lakehouse via a Livy endpoint
#'
#' Execute queries in a Fabric lakehouse via a Livy endpoint.
#' Do not confuse with `con_nhsbsa_fabric()` which uses a SQL endpoint/ODBC, which is not suitable for large queries.
#' This function uses proper Spark sessions akin to notebooks, and can execute any code you would execute in a notebook.
#'
#' @details
#' Write lazy intermediate queries and pull the results into local R for checking or write final queries and save the results in the lakehouse as tables.
#'
#' On first use you will be prompted to authenticate via https://login.microsoft.com/device. This will save an authentication token on your device, which will be re-used for a while, but will expire after some time, at which point you will have to re-authenticate.
#'
#' Each function call starts a Spark session, executes the code block, and stops the session. This means shared statements have to be repeated in each query, e.g. imports of Python packages and functions, UDF definitions, variable declarations, etc.
#'
#' Code could be written as a string in the R script or read from a separate .py or .sql script using e.g. `readr::read_file()`. RStudio supports language-specific code highlighting in Python and SQL scripts.
#'
#' The function is a wrapper around the `fabricQueryR` package.
#'
#' You need to have the `FABRICQUERYR_TENANT_ID` environment variable set in `.Renviron`. This is an identifier of NHSBSA's corporate account with Fabric. Can be obtained from https://app.fabric.microsoft.com -> Profile icon -> Exclamation mark near Tenant Name.
#'
#' @param livy_url Character. Lakehouse-specific endpoint. Obtained by opening a lakehouse in web UI -> settings cog on the left -> Livy endpoint -> Session job connection string
#' @param kind Character. Only `"sql"` and `"pyspark"` are currently supported. In principle, `"sparkr"` and `"scala"` could be supported, but support is turned off, as it requires testing.
#' @param code Character. The code to run, which can consist of one or multiple statements.
#'
#' @return
#' For blocks of code that have a table to return, a tibble is returned.
#' For blocks of code that do not have a table to return (e.g. table creation statements), the code is executed in Spark and nothing is returned to R.
#' This is handled automatically.
#'
#' Be mindful when querying tables such as `dwcp_clone.px_form_item_elem_comb_fact`, as without some kind of limiter (e.g. `.limit(n)`), R will choke.
#'
#' @examples
#' \dontrun{
#' livy_url = "<livy connection string>"
#'
#' # PySpark: pull the result into local R
#' df <- nhsbsa_fabric_query(
#'   livy_url,
#'   kind     = "pyspark",
#'   code     = '
#' from pyspark.sql import functions as F
#'
#' (
#' spark.table("dbo.my_table")
#' .limit(10)
#' )
#' '
#' )
#'
#' # PySpark: save the result in a lakehouse
#' nhsbsa_fabric_query(
#'   livy_url,
#'   kind     = "pyspark",
#'   code     = '
#' from pyspark.sql import functions as F
#'
#' (
#' spark.table("dbo.my_table")
#' .limit(10)
#' .write.mode("overwrite").option("overwriteSchema", "true").saveAsTable("dbo.output_table")
#' )
#' '
#' )
#'
#' # PySpark: multiple intermediate lazy statements, of which only the last is evaluated and returned
#' nhsbsa_fabric_query(
#'   livy_url,
#'   kind     = "pyspark",
#'  code     = '
#'from pyspark.sql import functions as F
#'
#'df1 = spark.table("dbo.my_table")
#'
#'df2 = df1.limit(10)
#'
#'df3 = df2.select("col_A", "col_B")
#'
#'df3
#''
#')
#'
#' # SQL: pull the result into local R
#' df <- nhsbsa_fabric_query(
#'   livy_url,
#'   kind = "sql",
#'   code = "select * from dbo.my_table limit 10"
#' )
#'
#' # SQL: save the result in a lakehouse
#' nhsbsa_fabric_query(
#'   livy_url,
#'   kind = "sql",
#'   code = "
#'   create table dbo.output_table as
#'   select * from dbo.my_table limit 10
#'   "
#' )
#'
#' # SQL: multiple statements
#' nhsbsa_fabric_query(
#' livy_url,
#' kind = "sql",
#' code = "
#'   begin
#'     declare variable ym int;
#'     set ym = 202401;
#'     select * from dbo.my_table where year_month = ym limit 10;
#'   end
#'   "
#' )
#' }
#'
#' @export
nhsbsa_fabric_query <- function(livy_url, kind, code) {

  if (!kind %in% c("sql", "pyspark")) {
    stop("`kind` must be either 'sql' or 'pyspark'.")
  }

  aad_token <- AzureAuth::get_azure_token(
    resource  = "https://api.fabric.microsoft.com",
    tenant    = Sys.getenv("FABRICQUERYR_TENANT_ID"),
    app       = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
    auth_type = "device_code"
  )

  ## Wrap PySpark code in a harness that runs it and, if the last statement is a
  ## DataFrame, prints it as JSON between <<JSON>>/<<END>> markers for R to parse.
  if (kind == "pyspark") {

    harness <- r"---(
import ast, base64, json, textwrap
from decimal import Decimal
from pyspark.sql import DataFrame

def _pj_run(src):
    g = globals()
    tree = ast.parse(textwrap.dedent(src))
    last = None
    if tree.body and isinstance(tree.body[-1], ast.Expr):
        tail = ast.Expression(tree.body.pop().value)
        exec(compile(tree, "<user>", "exec"), g)
        last = eval(compile(tail, "<user>", "eval"), g)
    else:
        exec(compile(tree, "<user>", "exec"), g)
        t = tree.body[-1] if tree.body else None
        if isinstance(t, ast.Assign) and isinstance(t.targets[0], ast.Name):
            last = g.get(t.targets[0].id)
    if isinstance(last, DataFrame):
        rows = last.collect()
        conv = lambda o: float(o) if isinstance(o, Decimal) else str(o)
        print("<<JSON>>" + json.dumps([r.asDict(recursive=True) for r in rows], default=conv) + "<<END>>")

_pj_run(base64.b64decode("<<B64>>").decode("utf-8"))
)---"

    # Base64-encode the code so quotes, backslashes and newlines don't break the harness
    b64  <- gsub("\\s", "", jsonlite::base64_enc(charToRaw(enc2utf8(code))))

    # Insert the B64-encoded code into the harness
    code <- sub("<<B64>>", b64, harness, fixed = T)

  }


  res <- fabricQueryR::fabric_livy_query(
    livy_url = livy_url,
    access_token = aad_token$credentials$access_token,
    kind = kind,
    code = code
  )

  if (identical(res$output$status, "error")) {
    stop(paste(res$output$ename, res$output$evalue, sep = ": "), call. = F)
  }

  ## Parse PySpark output
  if (kind == "pyspark") {

    txt  <- res$output$data[["text/plain"]]
    json <- if (is.null(txt)) NA_character_ else
      stringr::str_match(txt, "(?s)<<JSON>>(.*)<<END>>")[, 2]

    if (is.na(json)) return(invisible(NULL))

    return(tibble::as_tibble(jsonlite::fromJSON(json)))

  }

  # SQL: return an output if there is one to return
  # (For PySpark, the harness already has equivalent behaviour: returns JSON only when the last statement is a DataFrame)

  no_output_sql <- "\\b(create|alter|drop|truncate|insert|update|delete|merge|cache|uncache|refresh|repair|analyze|use|add\\s+(?:jar|file))\\b"
  if ( kind == "sql" && !stringr::str_detect(code, stringr::regex(no_output_sql, ignore_case = T)) )
    return(res$output$parsed)


}
