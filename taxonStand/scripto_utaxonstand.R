library(U.Taxonstand)
library(openxlsx)
library(dplyr)


splist <- read.xlsx("taxonstand/spExample.xlsx")
databaseExample <- read.xlsx("taxonstand/databaseExample.xlsx")
#splist<- rename(splist, "SORTER"="X1")

# The input as a dataframe with the columns "SPECIES", "AUTHOR" and/or "RANK"

res <- nameMatch(spList=splist, spSource=databaseExample, author = TRUE, max.distance= 1)
??nameMatch

head(res)

write.csv(res,file="nameMatch_taxonstand.csv")


# The current default only keeps the first 'best' matching result for each taxon name. If you want to check all the matched results, please change the option 'matchFirst=FALSE'.
res <- nameMatch(spList=splist, spSource=databaseExample, author = TRUE, max.distance= 1, matchFirst=FALSE)
dim(res)

