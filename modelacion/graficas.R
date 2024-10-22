recordsHerbario_raw <- read_csv("data/in/herbariomex_dw/occurrences.csv")
names(recordsHerbario_raw)

recordsHerbario <- recordsHerbario_raw %>%
  mutate(id_interno = paste0("Herb_", row_number()))%>%
  filter(., !is.na(scientificName) & !scientificName %in% c("Quercus L.", "Quercus L", "Quercus"))

unique(recordsHerbario$scientificName)


recordsHerbario <- mutate(recordsHerbario, estado = str_extract(stateProvince, 
      "^(Aguascalientes|Baja California( Sur)?|Campeche|Chiapas|Chihuahua|Coahuila( de Zaragoza)?|Colima|Durango|Guanajuato|Guerrero|Hidalgo|Jalisco|M[eé]xico|Michoac[áa]n( de Ocampo)?|Morelos|Nayarit|Nuevo Le[óo]n|Oaxaca|Puebla|Quer[eé]taro|Quintana Roo|San Luis Potos[íi]|Sinaloa|Sonora|Tabasco|Tamaulipas|Tlaxcala|Veracruz|Yucat[aá]n|Zacatecas)$"
)) %>%
  filter(., !is.na(estado)|!is.na(minimumElevationInMeters)) %>%
  mutate(., elevacion = minimumElevationInMeters)


conteo_por_estado <- table(recordsHerbario$estado)

library(RColorBrewer) 
color <- brewer.pal(length(conteo_por_estado), "Set2") 

pie(conteo_por_estado, 
    labels = names(conteo_por_estado), 
    main = "Distribución de Registros por Estado",
    clockwise = TRUE,
    col = color)

unique(recordsHerbario$estado)

barplot(conteo_por_estado,
        main = "Conteo de Encinos por Estado",
        ylab = "Número de registro de Encinos",
        las = 2, # Orienta las etiquetas de los estados en el eje X
        col = "lightblue", # Color de las barras
        border = "blue")

lm(elevacion ~ height, data=women)

especie_especifica <- recordsHerbario %>%
  filter(scientificName == "Quercus viminea") # Cambia "Quercus robur" por la especie que deseas

# Ajustar el modelo lineal
modelo <- lm(elevacion ~ 1, data = especie_especifica)

# Resumen del modelo
summary(modelo)

plot(especie_especifica$elevacion, 
     main = "Regresión Lineal de Elevación para Quercus viminea",
     xlab = "Observaciones",
     ylab = "Elevación",
     pch = 19, 
     col = "blue")
abline(modelo, col = "red")


especie_especifica2 <- recordsHerbario %>%
  filter(scientificName == "Quercus insignis") # Cambia "Quercus robur" por la especie que deseas

# Ajustar el modelo lineal
modelo2 <- lm(elevacion ~ 1, data = especie_especifica2)

# Resumen del modelo
summary(modelo2)

plot(especie_especifica2$elevacion, 
     main = "Regresión Lineal de Elevación para Quercus viminea",
     xlab = "Observaciones",
     ylab = "Elevación",
     pch = 19, 
     col = "blue")
abline(modelo, col = "red")

legend("topright", # Posición de la leyenda
       legend = c("Datos de Elevación", "Línea de Regresión"),
       col = c("blue", "red"), # Colores de los puntos y la línea
       pch = c(19, NA), # Tipo de punto, NA para línea
       lty = c(NA, 1))

conteo_de_especies <- table(recordsHerbario$scientificName)

barplot(conteo_de_especies,
        main = "Conteo de especies de Encinos",
        ylab = "Número de registro de Encinos",
        las = 2, # Orienta las etiquetas de los estados en el eje X
        col = "lightblue", # Color de las barras
        border = "blue")

conteo_years <- table(recordsHerbario$year)

barplot(conteo_years,
        main = "Años de registros",
        ylab = "Número de registro de Encinos",
        las = 2, # Orienta las etiquetas de los estados en el eje X
        col = "lightblue", # Color de las barras
        border = "blue")



