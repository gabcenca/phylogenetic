## ---- LOAD DATA FRAMES ---- ###

recordsHerbario_raw <- read_csv("data/in/herbariomex_dw/occurrences.csv")
names(recordsHerbario_raw)

recordsHerbario <- recordsHerbario_raw %>%
  mutate(id_interno = paste0("Herb_", row_number()))%>%
  filter(., !is.na(scientificName) & !scientificName %in% c("Quercus L.", "Quercus L", "Quercus"))

unique(recordsHerbario$scientificName)

files_list<- list.files("data/in/gbif_dwc/division_occurrence/", pattern=".csv", full.names=T)

files <- lapply(files_list,fread)

recordsGbif_raw <- do.call(rbind,files)


## ---- EXTRAER ESTADOS Y ELEVACION DE LOS 2 DATAFRAME ---- ###

recordsHerbario <- mutate(recordsHerbario, estado = str_extract(stateProvince, 
      "^(Aguascalientes|Baja California( Sur)?|Campeche|Chiapas|Chihuahua|Coahuila( de Zaragoza)?|Colima|Durango|Guanajuato|Guerrero|Hidalgo|Jalisco|M[eé]xico|Michoac[áa]n( de Ocampo)?|Morelos|Nayarit|Nuevo Le[óo]n|Oaxaca|Puebla|Quer[eé]taro|Quintana Roo|San Luis Potos[íi]|Sinaloa|Sonora|Tabasco|Tamaulipas|Tlaxcala|Veracruz|Yucat[aá]n|Zacatecas)$"
)) %>%
  filter(., !is.na(estado)|!is.na(minimumElevationInMeters)) %>%
  mutate(., elevacion = minimumElevationInMeters)

recordsGbif <- mutate(recordsGbif_raw, estado = str_extract(stateProvince, 
               "^(Aguascalientes|Baja California( Sur)?|Campeche|Chiapas|Chihuahua|Coahuila( de Zaragoza)?|Colima|Durango|Guanajuato|Guerrero|Hidalgo|Jalisco|M[eé]xico|Michoac[áa]n( de Ocampo)?|Morelos|Nayarit|Nuevo Le[óo]n|Oaxaca|Puebla|Quer[eé]taro|Quintana Roo|San Luis Potos[íi]|Sinaloa|Sonora|Tabasco|Tamaulipas|Tlaxcala|Veracruz|Yucat[aá]n|Zacatecas)$"
)) %>%
  filter(., !is.na(estado)|!is.na(elevation)) %>%
  mutate(., elevacion = elevation)

unique(recordsGbif$estado)

## ---- CONTEOS POR ESTADO ---- ###


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


conteo_por_estado <- table(recordsGbif$estado)

barplot(conteo_por_estado,
        main = "Conteo de Encinos por Estado",
        ylab = "Número de registro de Encinos",
        las = 2, # Orienta las etiquetas de los estados en el eje X
        col = "pink", # Color de las barras
        border = "lightblue")


## ---- CONTEOS POR ESPECIE ---- ###

conteo_estado_especiesHerb <- table(recordsHerbario$scientificName)

barplot(conteo_estado_especiesHerb,
        main = "Conteo de registros por especie",
        ylab = "Número de registro de cada especie",
        las = 2, # Orienta las etiquetas de los estados en el eje X
        col = "pink", # Color de las barras
        border = "lightblue")

unique(recordsHerbario$scientificName)

## ---- REGRESION LINEAL JONESII ---- ###


jonesii <- recordsHerbario %>%
  filter(scientificName == "Quercus jonesii") # Cambia "Quercus robur" por la especie que deseas

names(jonesii)

# Ajustar el modelo lineal
modelo <- lm(elevacion ~ decimalLatitude, data = jonesii)

# Resumen del modelo
summary(modelo)

# Ajustar los márgenes para darle espacio a la derecha
par(mar = c(5, 4, 4, 8)) 

# Crear el gráfico
plot(jonesii$elevacion, 
     main = "Regresión Lineal de Elevación para Quercus Jonesii",
     xlab = "Observaciones",
     ylab = "Elevación",
     pch = 19, 
     col = "cornsilk4")

# Añadir la línea de la regresión
abline(modelo, col = "red")

# Determinar el rango de valores del eje X para ubicar la leyenda manualmente
x_range <- par("usr")[1:2]
y_range <- par("usr")[3:4]

# Añadir la leyenda fuera del área de la gráfica
legend(x = x_range[2] + 10, y = mean(y_range), legend = 
       "Registros de
       Quercus Jonesii", 
       fill = "cornsilk4", xpd = TRUE)


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


## ---- CONTEOS DE ESPECIES ---- ###

conteo_de_especies <- table(recordsHerbario$scientificName)

barplot(conteo_de_especies,
        main = "Conteo de especies de Encinos",
        ylab = "Número de registro de Encinos",
        las = 2, # Orienta las etiquetas de los estados en el eje X
        col = "lightblue", # Color de las barras
        border = "blue")

### ----------- CONTEO POR ANOS -------------- ####

conteo_years_herbario <- table(recordsHerbario$year)


par(mar = c(5, 4, 4, 2))
barplot(conteo_years_herbario,
        main = "Años de registros del Herbario",
        ylab = "Número de registro de Encinos",
        las = 2, # Orienta las etiquetas de los estados en el eje X
        col = "#6E8B3D", # Color de las barras
        border = "darkolivegreen")


conteo_years_gbif <- table(recordsGbif$year)


par(mar = c(5, 4, 4, 2))
barplot(conteo_years_gbif,
        main = "Años de registros de GBIF",
        ylab = "Número de registro de Encinos",
        las = 2, # Orienta las etiquetas de los estados en el eje X
        col = "#B9D3EE", # Color de las barras
        border = "slategray4")




# Calcula proporciones
species_counts <- table(recordsHerbario$scientificName)

# Convertir el vector species_counts a un data frame
species_df <- as.data.frame(species_counts)

# Renombrar las columnas
colnames(species_df) <- c("species", "species_count")

# Obtener las 20 especies con menos registros
least_records <- species_df[order(species_df$species_count), ][1:50, ]

# Obtener las 20 especies con más registros
most_records <- species_df[order(-species_df$species_count), ][1:20, ]

# Gráfico de barras con proporciones
par(mar = c(10, 4, 4, 2) + 0.1)  # Ajusta el margen inferior
barplot(most_records$species_count, 
        names.arg = most_records$species, 
        main = "Encinos con mayores registros en la Red de Herbarios Mexicana", 
        xlab = "Especies", 
        ylab = "Número de registros", 
        col = "lightgreen", 
        las = 3,  # Rota las etiquetas 90 grados
        cex.axis = 0.7)  # Ajusta el tamaño de la fuente


bp <- barplot(most_records$species_count, 
              main = "Encinos con mayores registros en Herbario", 
              ylab = "Número de registros", 
              col = "lightgreen", 
              names.arg = "",  # No colocar etiquetas aquí
              ylim = c(0, max(most_records$species_count) * 1.1))  # Ajusta el límite del eje y

# Añadir las etiquetas en diagonal
text(x = bp, 
     y = par("usr")[3] - 0.5,  # Ajusta la posición vertical
     labels = most_records$species, 
     srt = 45,  # Rota las etiquetas 45 grados
     adj = c(1, 0.5),  # Ajusta la posición
     xpd = TRUE)  # Permite que el texto se dibuje fuera de los límites del gráfico


#### RANGO DE ELEVACION ###
range(recordsGbif$elevation, na.rm = TRUE)
unique(recordsGbif$elevation)
range(recordsHerbario$elevacion, na.rm = TRUE)

mean(recordsGbif$elevation, na.rm = TRUE)
mean(recordsHerbario$elevacion, na.rm = TRUE)


## ver si existe relacion entre latitud y longitud
recordsGbif_clean <- recordsGbif[!is.na(recordsGbif$decimalLatitude) & !is.na(recordsGbif$elevation) & !is.na(recordsGbif$decimalLongitude), ]
length(recordsGbif_clean$decimalLatitude)
length(recordsGbif_clean$elevation)
length(recordsGbif_clean$decimalLongitude)
# Graficar los datos filtrados
plot(recordsGbif_clean$decimalLatitude[1:1000], recordsGbif_clean$elevation[1:1000],
     xlab = "Latitud Decimal", ylab = "Elevación",
     main = "Relación entre Latitud y Elevación")

plot(recordsGbif_clean$elevation[1:1000], recordsGbif_clean$decimalLatitude[1:1000],
     xlab = "Latitud Decimal", ylab = "Elevación",
     main = "Relación entre Latitud y Elevación")

plot(recordsGbif_clean$decimalLongitude[1:1000], recordsGbif_clean$elevation[1:1000],
     xlab = "Longitud Decimal", ylab = "Elevación",
     main = "Relación entre Longitud y Elevación")

