#Load libraries 
library(magrittr)
library(dplyr)
library(stringr)
library(data.table)
library(rgbif)
library(purrr)

setwd("~/phylogenetic/")

#Upload data frame 

recordsHerbario_raw <- fread("data/in/herbariomex_dw/occurrences.csv")

#recordsBIEN_raw <- fread("data/in/dataBIENmex_csv/dataBIENmex.csv")

files_list<- list.files("data/in/gbif_dwc/division_occurrence/", pattern=".csv", full.names=T)

files <- lapply(files_list,fread)

recordsGbif_raw <- do.call(rbind,files)


# --------------- GBIF BACKBONE -----------------


##Seleccionar columnas del herbario
herbario_revisar <- recordsHerbario_raw %>%
  mutate(nombre_autor= paste(scientificName, 
                             scientificNameAuthorship, 
                              sep = " ")) %>%
  select(nombre_autor,kingdom,id)

print("Inicia funcion backbone herbario")

#Correr para todo el df
resultados_herb <- apply(herbario_revisar, 1, function(fila) {
  name_backbone(
    name = fila["nombre_autor"],
    kingdom = fila["kingdom"]
  )
})


resultados_df <- bind_rows(resultados)
herbario_revisar <- as.data.frame(herbario_revisar)
herb_final <- cbind(herbario_revisar["id"], resultados_df)

print("Termina funcion backbone herbario")

##Seleccionar columnas de gbif
gbif_revisar <- recordsGbif_raw %>%
  select(acceptedScientificName,kingdom,gbifID)

print("Inicia funcion backbone gbif")
# Crear una lista para almacenar los resultados

resultados_gbif <- apply(gbif_revisar, 1, function(fila) {
  name_backbone(
    name = fila["acceptedScientificName"],
    kingdom = fila["kingdom"]
  )
})


resultados_gbif <- bind_rows(resultados_gbif)
gbif_revisar <- as.data.frame(gbif_revisar)
gbif_final <- cbind(gbif_revisar["gbifID"], resultados_gbif)

print("Termina funcion backbone gbif")


write.csv(gbif_final,file="data/temp/backbone_gbif/gbif_nombres_backbone.csv")
write.csv(herb_final,file="data/temp/backbone_gbif/herb_final_backbone.csv")

print ("Final")

# # Contar el número de nombres aceptados y sinónimos
# gbif_final %>% count(status)
# 
# # Extraer nombres aceptados
# nombres_aceptados_gbif <- gbif_final %>% filter(status == "ACCEPTED")
# 
# # Extraer nombres con duda
# nombres_duda_gbif <- gbif_final %>% filter(status == "DOUBTFUL")
# 
# 
# 
# #Probar con df de herbario completo, solo con una fila x especie
# 
# herbario_revisar <- recordsHerbario_raw %>%
#   mutate(nombre_autor= paste(scientificName, 
#                              scientificNameAuthorship, 
#                              sep = " ")) %>%
#   select(nombre_autor,kingdom,id)
# 
# herb_unicos<- herbario_revisar %>% distinct(nombre_autor, .keep_all = TRUE)
# 
# revision_herb_unicos <- apply(herb_unicos[], 1, function(fila) {
#   name_backbone(
#     name = fila["nombre_autor"],
#     kingdom = fila["kingdom"]
#   )
# })
# 
# revision_herb_unicos_df <- bind_rows(revision_herb_unicos)
# 
# str(herb_unicos)
# herb_unicos <- as.data.frame(herb_unicos)
# herb_final_unicos <- cbind(herb_unicos["id"], revision_herb_unicos_df)
# 
# 
# 
# # Contar el número de nombres aceptados y sinónimos
# herb_final_unicos %>% count(status)
# 
# # Extraer nombres aceptados
# nombres_aceptados_herb <- herb_final_unicos %>% filter(status == "ACCEPTED")
# 
# # Extraer nombres con duda
# nombres_duda_herb <- herb_final_unicos %>% filter(status == "DOUBTFUL")
# 
# # Extraer nombres sinonimos
# nombres_sin_herb <- herb_final_unicos %>% filter(status == "SYNONYM")
# 
# 



