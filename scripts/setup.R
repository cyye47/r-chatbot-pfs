library(duckdb)
library(DBI)
library(here)

db_path <- here("pfs.duckdb")

# Delete if exists
if (file.exists(db_path)) {
  unlink(db_path)
}

# Load voyager_PFS.csv into a table named `pfs`
conn <- dbConnect(duckdb(), dbdir = db_path)
duckdb_read_csv(conn, "pfs", here("pfs.csv"))
dbDisconnect(conn)
