#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Make contingency tables of educational pairings
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
rm(list = ls())

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Set Parameters ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
SupTabsDir <- "CensusCSV/SupplementalTables/"
OutDir     <- "CensusCSV/CSV_EduWH/"

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
NofObs      <- as.matrix(read.csv(paste0(SupTabsDir, "NofObs.csv")))
TotProp1980 <- as.matrix(read.csv(paste0(SupTabsDir, "TotProp1980.csv"), header = FALSE))
TotProp1990 <- as.matrix(read.csv(paste0(SupTabsDir, "TotProp1990.csv"), header = FALSE))
TotProp2000 <- as.matrix(read.csv(paste0(SupTabsDir, "TotProp2000.csv"), header = FALSE))
TotProp2010 <- as.matrix(read.csv(paste0(SupTabsDir, "TotProp2010.csv"), header = FALSE))

EduWH1980 <- NofObs[1,2] * (TotProp1980 * (100/sum(TotProp1980)) / 100)
EduWH1990 <- NofObs[2,2] * (TotProp1990 / 100)
EduWH2000 <- NofObs[3,2] * (TotProp2000 / 100)
EduWH2010 <- NofObs[4,2] * (TotProp2010 / 100)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Save Data ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
write.table(EduWH1980, file = paste0(OutDir, "EduWH1980.csv"), sep = ",", row.names = FALSE, col.names = FALSE)
write.table(EduWH1990, file = paste0(OutDir, "EduWH1990.csv"), sep = ",", row.names = FALSE, col.names = FALSE)
write.table(EduWH2000, file = paste0(OutDir, "EduWH2000.csv"), sep = ",", row.names = FALSE, col.names = FALSE)
write.table(EduWH2010, file = paste0(OutDir, "EduWH2010.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

write.csv(EduWH2010, file = paste0(OutDir, "EduWH2010_writecsv.csv"), row.names = FALSE)
