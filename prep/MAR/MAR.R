library(readxl)
library(dplyr)
library(readr)
library(sf)

y<- 2024

pol<- st_read("C:/R/OHI/datacenter2/rgn_12mn_1mn_merge.shp")
pol <- st_make_valid(pol)

c1<- st_read("C:/R/OHI/datacenter/polygon.shp") %>%
  dplyr::select(REP_SUBPES.10 ,Centro = REP_SUBPES.2)
c1 <- st_transform(c1, st_crs(pol))
c1<- st_centroid(c1)

sf_in <- st_intersection(c1, pol)

sf<- sf_in %>%
  as.data.frame() %>%
  select(Centro, rgn_id)

sf$Centro<- as.numeric(sf$Centro)

df <- read_csv("C:/github/chl2/prep/MAR/Produccion_centros_cultivo_2020_2024.csv")



df["Especie"][df["Especie"] == "SALMON PLATEADO O COHO"] <- "Salmones"
df["Especie"][df["Especie"] == "SALMON DEL ATLANTICO"] <- "Salmones"

df["Especie"][df["Especie"] == "OSTION DEL NORTE"] <- "Ostion del Norte"

df["Especie"][df["Especie"] == "ABALON JAPONES"] <- "Abalon"
df["Especie"][df["Especie"] == "ABALON ROJO"] <- "Abalon"


df["Especie"][df["Especie"] == "CHORO"] <- "Mitilidos"
df["Especie"][df["Especie"] == "CHORITO"] <- "Mitilidos"
df["Especie"][df["Especie"] == "CHOLGA"] <- "Mitilidos"


df["Especie"][df["Especie"] == "OSTRA DEL PACIFICO"] <- "Ostras"
df["Especie"][df["Especie"] == "OSTRA CHILENA"] <- "Ostras"

sp<- data.frame(table(df$Especie))

df<- df %>%
  select("Centro",  "Comuna", "Año", "Especie", "Etapa", "Mes", "EGRESOS K", "Tipo de Cultivo", "Sector", "Cuerpo de Agua") %>%
  filter(Año %in% c(2020:2024),
         Especie %in% c("Salmones", "Ostion del Norte", "Abalon", "Mitilidos", "Ostras"))

df1<- df %>%
  filter(`Cuerpo de Agua` == "Mar" |
           Centro %in% unique(sf_in$Centro))

df1<- df1%>%
  group_by(Comuna, Año, Especie) %>%
  dplyr::summarise(Catch = sum(`EGRESOS K`, na.rm = T)) %>%
  as.data.frame() %>%
  mutate(
    Comuna = paste0(
      toupper(substr(tolower(Comuna), 1, 1)),  # primera letra en mayúscula
      substr(tolower(Comuna), 2, nchar(Comuna))))
df1$Comuna <- iconv(df1$Comuna, from = "UTF-8", to = "ASCII//TRANSLIT")

df1$Comuna[which(df1$Comuna == "Cabo de hornos(navarino)")] <- "Cabo de hornos"
df1$Comuna[which(df1$Comuna == "Savedra")] <- "Saavedra"


library(readr)
rgn <- read_csv("C:/github/chl2/comunas/spatial/regions_list.csv") %>%
  select(Comuna = "rgn_name", rgn_id)

df1<- merge(df1, rgn, all = T) %>%
  select(rgn_id, year = Año, especie = Especie, tonnes = Catch) %>%
  filter(year %in% c((y-4):y))

write.csv(df1, "comunas/layers/mar_harvest_tonnes_chl2024.csv", row.names = F)

###
library(readr)
sus <- read_excel("C:/github/chl2/prep/MAR/mar_sustainability.xlsx", sheet = 2) %>%
  dplyr::select(especie, coeff)



write.csv(sus, "comunas/layers/mar_sustainability_chl2024.csv", row.names = F)

