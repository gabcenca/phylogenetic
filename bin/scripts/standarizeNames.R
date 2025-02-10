#Load libraries 
library(readr)
#to use u.taxonstand:
library(plyr)
library(magrittr)
library(U.Taxonstand) #devtools::install_github("ecoinfor/U.Taxonstand")
library(dplyr)
library(writexl)#install.packages("writexl")
library(openxlsx) #install.packages("openxlsx")

#Upload data frame 
records_df <- read_csv("data/temp/herbaGbifBIEN_df/herbGbifBien.csv")


#Upload the species list
splist <- read_delim("data/in/CatalogoAutoridadTaxomicaQuercus_csv/0010521-240906103802322.csv", 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)
names(splist)

#Extract name species, author and rank in order for the package to work
splist <- splist %>%
  select("species","verbatimScientificNameAuthorship",
         "gbifID","family") %>%
  rename("NAME"="species",
         "ID"="gbifID",
         "AUTHOR"="verbatimScientificNameAuthorship",
         "FAMILY"="family") %>%
  mutate("ACCEPTED_ID" = 0) %>%
  select("ID", "NAME", "AUTHOR", "ACCEPTED_ID", "FAMILY")

#Extract the df to excel format 
write_xlsx(splist,"data/temp/taxonomicNames_UTaxonStand/accepted_species.xlsx")

#Change the df of records to match with the package and select the columns of interest
names(records_df)
records_df <- records_df %>%
  rename("Sorter"="valueID",
         "Name"="scientificName",
         "Author" = "verbatimScientificNameAuthorship") %>%
  select("Sorter","Name","Author")

names(records_df)

#Write an excel sheet for this df
write_xlsx(records_df,"data/temp/records_UTaxonStand/records_dfUts.xlsx")

# ---- UTaxonStand ---

# load the database
database<- read.xlsx("data/temp/records_UTaxonStand/records_dfUts.xlsx")
str(database)
# load the species list to match with
splist <- readxl::read_xlsx("data/temp/taxonomicNames_UTaxonStand/accepted_species.xlsx")
str(splist)
# run the main function of name matching

res <- nameMatch(spList=splist, spSource=database, author=FALSE, max.distance=1, Append=TRUE)

# save the result in an xlsx file
write.xlsx(res,"Result_from_U.Taxonstand.xlsx", overwrite=TRUE)