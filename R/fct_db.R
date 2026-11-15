# Build a database connection from a credentials INI section.

db_connect <- function(section) {
  file <- path.expand(Sys.getenv("NBA_DB_CREDENTIALS", "~/.config/scs_hub_credentials.ini"))
  if (!file.exists(file)) {
    stop("Credentials file not found: ", file, call. = FALSE)
  }
  creds <- ini::read.ini(file)[[section]]
  if (is.null(creds)) {
    stop("Section '", section, "' not found in ", file, call. = FALSE)
  }
  DBI::dbConnect(
    RPostgres::Postgres(),
    user = creds$user,
    password = creds$password,
    host = creds$host,
    port = creds$port,
    dbname = creds$database
  )
}
