library("RSQLite")
library("DBI")
library("plyr")

con <- dbConnect(RSQLite::SQLite(), "Data/FTLsCMS2/FieldData_20220225.db")

tables <- dbListTables(con)
standInit <- dbGetQuery(con, "SELECT * FROM FVS_StandInit")
treeInit <- dbGetQuery(con, "SELECT * FROM FVS_TreeInit")
