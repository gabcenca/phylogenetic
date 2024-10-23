#Load libraries 
library(readr)
#to use u.taxonstand:
library(plyr)
library(magrittr)
library(U.Taxonstand) #devtools::install_github("ecoinfor/U.Taxonstand")
library(dplyr)
library(writexl)#install.packages("writexl")
library(openxlsx) #install.packages("openxlsx")
library(stringr)

#Upload data frame 

recordsHerbario_raw <- fread("data/in/herbariomex_dw/occurrences.csv")

recordsBIEN_raw <- fread("data/in/dataBIENmex_csv/dataBIENmex.csv")

files_list<- list.files("data/in/gbif_dwc/division_occurrence/", pattern=".csv", full.names=T)

files <- lapply(files_list,fread)

recordsGbif_raw <- do.call(rbind,files)

#Upload the species list
splist_raw <- read_delim("data/in/CatalogoAutoridadTaxomicaQuercus_csv/0010521-240906103802322.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)
names(splist_raw)



#Extract name species, author and rank in order for the package to work
splist <- splist_raw %>%
  select("scientificName", #coincide con el formato de acceptedScientificName
         "verbatimScientificNameAuthorship", # Para que haga match con el autor
         "family",
         "genus", 
         "speciesKey", #Codigo de la especie a la que corresponde el accepted_name(Ejemplo Q.
         #jonesii y Q. coccolobifolia tienen el mismo species_key porque son sinonimas)
         "taxonKey") %>% #codigo de cada taxon (los sinonimos y las variades tienen su propio taxonKey
  rename("NAME"="scientificName",
         "ID"="taxonKey",
         "AUTHOR"="verbatimScientificNameAuthorship",
         #"FAMILY"="family",
         #"GENUS"="genus",
         "ACCEPTED_ID" = "speciesKey") %>%
  mutate("RANK" = "") %>%
 #mutate(NAME = str_extract(NAME, "Quercus [a-zA-Z]{1,}|Quercus .{1}\\w{1,}|Quercus .{1} \\w{1,}")) %>%
  select("ID", "NAME", "AUTHOR", "RANK", "ACCEPTED_ID") 

head(splist)

names(splist) <- str_to_title(names(splist))

#Extract the df to excel format 
write_xlsx(splist,"data/temp/taxonomicNames_UTaxonStand/accepted_species.xlsx")

#Change the df of records to match with the package and select the columns of interest

records_df <- recordsGbif_raw %>%
  rename("NAME"="acceptedScientificName") %>%
         #"AUTHOR" = "scientificNameAuthorship") %>%
  mutate(RANK = "") %>% 
  select("NAME",
         #"AUTHOR", 
         "RANK") %>%
 # mutate(NAME = str_extract(NAME, "Quercus [a-zA-Z]{1,}|Quercus .{1}\\w{1,}|Quercus .{1} \\w{1,}")) %>% 
  filter(., !is.na(NAME) & !NAME %in% c("Quercus L.", "Quercus L")) 

names(records_df)

names(records_df) <- str_to_title(names(records_df))

#Write an excel sheet for this df
write_xlsx(records_df,"data/temp/records_UTaxonStand/records_dfUts.xlsx")

# ---- UTaxonStand ---

# load the database
records<- read.xlsx("data/temp/records_UTaxonStand/records_dfUts.xlsx")
#str(records)
#names(records)[1] <- "id_interno"

# load the species list to match with
splist <- readxl::read_xlsx("data/temp/taxonomicNames_UTaxonStand/accepted_species.xlsx")
str(splist)
names(splist)

test <- records[c(sample(1:nrow(records),10),27042),]
test2 <- data.frame(Name = "Quercus sapotifolia Liebm.", Author = "Liebm.", Rank=2)


# run the main function of name matching
res <- nameMatch(spList=test, spSource=splist, author=FALSE, max.distance=1, matchFirst = FALSE)

res$id_interno <- test$id_interno

# save the result in an xlsx file
write.xlsx(res,"Result_from_U.Taxonstand.xlsx", overwrite=TRUE)

names(splist_raw)


#Go to joinTable.R script and then select the columns of interest of herbGbif_raw
names(herbGbif_raw)
herbGbif_raw <- select(herbGbif_raw,
                       id_interno,)


#For records_df_raw values not in splist_raw
antijoin <- records_df_raw %>%
  anti_join(splist_raw, 
            # Define equivalence in column names in both df
            by = "scientificName")
unique(antijoin$scientificName)


#For splist_raw values not in records_df_raw
antijoin2 <- splist_raw %>%
  anti_join(records_df_raw, 
            # Define equivalence in column names in both df
            by = "scientificName")
unique(antijoin2$scientificName)


#For records_df_raw values in splist_raw
join <- records_df_raw %>%
          join(splist_raw, 
            # Define equivalence in column names in both df
            by = "scientificName")

unique(join$scientificName)

#For splist_raw values in records_df_raw
join2 <- splist_raw %>%
          join(records_df_raw, 
            # Define equivalence in column names in both df
            by = "scientificName")

unique(join2$scientificName)
