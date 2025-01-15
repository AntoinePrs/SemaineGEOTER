
# Vider l'environnement et libérer la mémoire
rm(list=ls())
gc()

# Package
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(mapview)

# Chargement des données
dvf <- st_read("data/dvf13.gpkg")
iris <- st_read("data/CONTOURS-IRIS.shp")

# Préparation des données des IRIS 
irisMars <- iris %>% 
  filter(substr(INSEE_COM, 1, 3)=="132")

# Filtres du DVF pour avoir les apparts de Marseille en 22/23
dvfMars <- dvf %>% 
  filter(substr(code_insee, 1, 3)=="132", 
         maison == 0, 
         anneemut %in% c(2022, 2023))


# Cartographie
ggplot()+
  geom_sf(data = irisMars)+
  geom_sf(data = dvfMars %>% sample_n(2000), size=.2)

# Exploration interactive de 2000 transactions 
#sélectionnées aléatoirement
mapview(dvfMars %>% sample_n(2000))


# Géomatique --------------------------------------------------

# On extrait les codes et les géométries des IRIS
irisgeo <- irisMars %>% select(CODE_IRIS, geometry)

# Jointure spatiale entre les transactions et les IRIS
# (permet de rajouter CODE_IRIS aux données des données des 
# transactions)
transaction_iris <- st_join(dvfMars, 
                            irisgeo)

# On agrège sur le code IRIS en calculant la médiane et 
# le nombre de transactions qu'on agrège
iris_px <- transaction_iris %>% 
  as_tibble() %>% 
  group_by(CODE_IRIS) %>% 
  summarise(pxm2med=median(pxm2), 
            nbmut=n())

irisgeo <- left_join(irisgeo, iris_px, by="CODE_IRIS")

# 
ggplot()+
  geom_sf(data = irisgeo, aes(fill=pxm2med))+
  scale_fill_viridis_c()

# Discrétisation
ggplot()+
  geom_density(data = irisgeo, aes(x = pxm2med))

# Algorithme de discrétisation
bks <- mapsf::mf_get_breaks(irisgeo$pxm2med, 
                            nbreaks = 6, 
                            breaks = "msd", 
                            central = T)

# On supprime les classes 7 et 8
bks <- bks[c(1:6, 9)]

# Visualisation de la discrétisation
ggplot()+
  geom_density(data = irisgeo, aes(x = pxm2med))+
  geom_vline(xintercept = bks)

#
ggplot()+
  annotation_map_tile(zoom=11, type = "cartolight")+
  geom_sf(data = irisgeo, aes(fill=pxm2med), 
          #  alpha=.3
  )+
  scale_fill_fermenter(breaks = bks, 
                       direction=1, 
                       palette = "YlOrRd")+
  theme_bw()+
  coord_sf(datum = NA)+ # Supprime le graticule
  annotation_scale()+ # Ajoute une échelle
  labs(title = "Prix des IRIS marseillais", 
       subtitle = "Appartements en 2022 et 2023", 
       fill = "Prix (€/m²)", 
       caption = "Source : DGFiP-DGALN-Cerema, DVF+
       \nRéalisation : Master GEOTER")



irisctr <- st_point_on_surface(irisgeo)

ggplot()+
  geom_sf(data = irisgeo)+
  geom_sf(data = irisctr, aes(size=nbmut, fill=pxm2med), 
          shape=21, alpha=.5)+
  scale_size_area(max_size = 6)+
  scale_fill_viridis_c()