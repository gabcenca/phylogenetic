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
library(data.table)

#Upload data frame 

recordsHerbario_raw <- fread("data/in/herbariomex_dw/occurrences.csv")

#recordsBIEN_raw <- fread("data/in/dataBIENmex_csv/dataBIENmex.csv")

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

# --------------- GBIF BACKBONE -----------------


##Seleccionar columnas del herbario
herbario_revisar <- recordsHerbario_raw %>%
  mutate(nombre_autor= paste(scientificName, 
                             scientificNameAuthorship, 
                              sep = " ")) %>%
  select(nombre_autor,kingdom,id)

write.csv(herbario_revisar, "data/temp/herbario_checknames/herbario_revisar.csv")

library(rgbif)


test_herb <- herbario_revisar[c(sample(1:nrow(herbario_revisar), 10)), ]
test_herb <- herbario_revisar[c(sample(1:nrow(herbario_revisar), 100)), ]


# Crear una lista para almacenar los resultados
resultados <- apply(test_herb[], 1, function(fila) {
  name_backbone(
    name = fila["nombre_autor"],
    kingdom = fila["kingdom"]
  )
})

library(purrr)
resultados_df <- bind_rows(resultados)

str(test_herb)
test_herb <- as.data.frame(test_herb)
herb_final <- cbind(test_herb["id"], resultados_df)
View(herb_final)


##Seleccionar columnas de gbif
gbif_revisar <- recordsGbif_raw %>%
  select(acceptedScientificName,kingdom,gbifID)

test_gbif <- gbif_revisar[c(sample(1:nrow(gbif_revisar), 10)), ]

# Crear una lista para almacenar los resultados

resultados_gbif <- apply(test_gbif[], 1, function(fila) {
  name_backbone(
    name = fila["acceptedScientificName"],
    kingdom = fila["kingdom"]
  )
})


resultados_gbif <- bind_rows(resultados_gbif)

test_gbif <- as.data.frame(test_gbif)
gbif_final <- cbind(test_gbif["gbifID"], resultados_gbif)


#Probar con df de gbif completo, solo con una fila x especie

gbif_unicos<- gbif_revisar %>% distinct(acceptedScientificName, .keep_all = TRUE)

revision_gbif_unicos <- apply(gbif_unicos[], 1, function(fila) {
  name_backbone(
    name = fila["acceptedScientificName"],
    kingdom = fila["kingdom"]
  )
})

revision_gbif_unicos_df <- bind_rows(revision_gbif_unicos)

str(gbif_unicos)
gbif_unicos <- as.data.frame(gbif_unicos)
gbif_final <- cbind(gbif_unicos["gbifID"], revision_gbif_unicos_df)



# Contar el número de nombres aceptados y sinónimos
gbif_final %>% count(status)

# Extraer nombres aceptados
nombres_aceptados_gbif <- gbif_final %>% filter(status == "ACCEPTED")

# Extraer nombres con duda
nombres_duda_gbif <- gbif_final %>% filter(status == "DOUBTFUL")



#Probar con df de herbario completo, solo con una fila x especie

herbario_revisar <- recordsHerbario_raw %>%
  mutate(nombre_autor= paste(scientificName, 
                             scientificNameAuthorship, 
                             sep = " ")) %>%
  select(nombre_autor,kingdom,id)

herb_unicos<- herbario_revisar %>% distinct(nombre_autor, .keep_all = TRUE)

revision_herb_unicos <- apply(herb_unicos[], 1, function(fila) {
  name_backbone(
    name = fila["nombre_autor"],
    kingdom = fila["kingdom"]
  )
})

revision_herb_unicos_df <- bind_rows(revision_herb_unicos)

str(herb_unicos)
herb_unicos <- as.data.frame(herb_unicos)
herb_final_unicos <- cbind(herb_unicos["id"], revision_herb_unicos_df)



# Contar el número de nombres aceptados y sinónimos
herb_final_unicos %>% count(status)

# Extraer nombres aceptados
nombres_aceptados_herb <- herb_final_unicos %>% filter(status == "ACCEPTED")

# Extraer nombres con duda
nombres_duda_herb <- herb_final_unicos %>% filter(status == "DOUBTFUL")

# Extraer nombres sinonimos
nombres_sin_herb <- herb_final_unicos %>% filter(status == "SYNONYM")





