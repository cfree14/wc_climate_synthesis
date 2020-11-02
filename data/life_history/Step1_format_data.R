
# Packages
library(Hmisc)
library(RODBC)

# Read data
db <- odbcConnect(dsn="data/life_history/LifeHistoryDB10/Life History Database Version 1.mdb")


db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=data/life_history/LifeHistoryDB10/Life History Database Version 1.mdb")


db <- mdb.get(file="data/life_history/LifeHistoryDB10/Life History Database Version 1.mdb")
