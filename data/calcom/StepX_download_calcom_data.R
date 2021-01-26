

# Packages
# library(RODBC) 
library(odbc)

# Directories

# calcom <- "https://calcom.psmfc.org/master_document/SqlServer.PORICHTHYS.calcom/default.htm"

# Connect to CALCOM database
ch <- odbcConnect(dsn = "SqlServer.PORICHTHYS.calcom")

