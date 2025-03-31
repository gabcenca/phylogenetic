
# Análisis de correlación climática para escoger las variables climáticas mas importantes


# Cargar las librerías necesarias
library(dplyr)
library(corrplot)
library(caret)
library(usdm)

# Cargar los datos climáticos (asegúrate de tener un archivo .csv o similar)
datos_climaticos <- read.csv("valor_clima_mne2025.csv") %>% select(-Latitude, -Longitude,-region, -wc2.1_30s_bio_9,-wc2.1_30s_bio_18, -wc2.1_30s_bio_19, -wc2.1_30s_bio_8,-registro, -species) 

# Calcular la matriz de correlación de Spearman entre las variables bioclimáticas
correlaciones <- cor(datos_climaticos, method = "spearman")

# Mostrar la matriz de correlación
print(correlaciones)


highly_correlated <- findCorrelation(correlaciones, cutoff = 0.7)


# Ver qué columnas están altamente correlacionadas
print(highly_correlated)


datos_reducido<- datos_climaticos[ , -highly_correlated]

# Visualizar la matriz de correlaciones
corrplot(correlaciones, method = "circle", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

vif <- vifcor(datos_climaticos, th=0.7, method = 'spearman')
print(vif)

subset <- vif@results$Variables

corselected <- cor(datos_climaticos[,subset])

corrplot(corselected, method = "number", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
