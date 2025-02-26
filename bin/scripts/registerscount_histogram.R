######
# Script : Make a df with the registers of each specie and a histogram.
# Author: Gabriela Centeno y Sofía Zorrilla
# Date: 20/06/2024
#######

#Load libraries
library(lubridate)
library(dplyr)

# -- Create a data frame with the sum of each species. -- #

species_count <- hgbif_completo %>%
  count(correctname, name = "total_registros")%>%
  arrange(total_registros)  %>%
  filter(!is.na(correctname))   #Hay 27 NAs



# -- Create and histogram so we can define the time frame to work with. -- #

# Extract the year from eventDate
hgbif_years <- hgbif_completo %>%
  mutate(year = year(ymd(eventDate)))

# Create the histogram
hist_conteos <- ggplot(hgbif_years, aes(x = year)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue", alpha = 0.7) +
  labs(title = "Histograma de Fechas de Colecta (Por Año)",
       x = "Año de Colecta",
       y = "Número de Registros") +
  theme_minimal() +
  scale_x_continuous(limits = c(1827, 2024),
                     breaks = seq(1827, 2024, by = 10))


