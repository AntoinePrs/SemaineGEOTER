rm(list=ls())

# Package
library(DBI) # connexion à une base de données
library(RPostgres) # Drivers PostgreSQL
library(sf)
library(dplyr)
library(tidyr) # idem
library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(mapview)


# Chargement des données -------------------------------------------------------

# Connexion au serveur
cred <- read.delim("cred.txt", sep = ',')

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "dvfplus", 
                 host = cred$host, 
                 port = cred$port, 
                 user = cred$user, 
                 password = cred$mdp)

# Requête d'un extrait de DVF sur le serveur
dvf <- st_read(con, query = "
SELECT idmutation, datemut, anneemut, moismut, 
coddep, libnatmut, vefa, valeurfonc, l_codinsee, sterr, sbati, nblocmai, 
nblocapt, nblocdep, nbapt1pp, nbapt2pp, nbapt3pp, nbapt4pp, nbapt5pp,
nbmai1pp, nbmai2pp, nbmai3pp, nbmai4pp, nbmai5pp, geomlocmut AS geom
FROM dvf.mutation
WHERE nbcomm = 1
AND coddep = '40'
AND anneemut IN ('2022', '2023')
AND libtypbien IN ('UNE MAISON', 'UN APPARTEMENT')
AND geomlocmut IS NOT NULL
AND valeurfonc IS NOT NULL")

# Chargement des IRIS
com <- st_read("data/COMMUNE.shp")
com <- com %>% filter(INSEE_DEP=="40")

# Préparation des données DVF --------------------------------------------------

# Filtrer
dvf <- dvf %>%
  mutate(code_insee = as.character(substr(l_codinsee, 2 , 6)),
         pxm2 = valeurfonc/sbati)

# Table pivot des pieces des logements
piece_long <- dvf %>% 
  as_tibble() %>% 
  select(idmutation, nbapt1pp:nbmai5pp) %>% 
  pivot_longer(nbapt1pp:nbmai5pp) %>% 
  filter(value==1) %>% 
  mutate(nbpiece=substr(name, 6, 6)) %>% 
  select(idmutation, nbpiece)

# Jointure entre la donnée des pièces et dvf
dvf <- left_join(dvf, piece_long, 
                 by="idmutation")

# Calcul des prix et du nombre de transaction ----------------------------------

# On limite ne garde que la geometrie, le nom et le code
com <- com %>% select(INSEE_COM, NOM, geometry)

# Jointure spatiale
transactions_com <- st_join(dvf, com, join = st_within)

# 
px_com <- transactions_com %>% 
  as_tibble() %>% 
  group_by(INSEE_COM) %>% 
  summarise(pxm2med = round(median(pxm2), 0), 
            pxm2moy = round(mean(pxm2), 0), 
            nbmut = n())

px_com <- left_join(com, px_com, by="INSEE_COM")

# Calcul des centroïdes
comCtr <- st_point_on_surface(px_com)

# Discrétisation 
var <- px_com$pxm2med
bks <- mapsf::mf_get_breaks(var, nbreaks = 6, breaks = "q6", central = T)

ggplot() +
  # Carte de base
  annotation_map_tile(type = "cartolight", 
                      #zoom = 11, 
                      alpha = .6) +
  # Choroplèthe des prix de l'immobilier
  geom_sf(data = px_com %>% filter(pxm2med != 0), 
          aes(fill = pxm2med)) +
  scale_fill_fermenter(
    breaks = bks, 
    palette = "YlOrRd", 
    direction = 1, 
    na.value = "lightgrey"
  ) +
  # Cercles proportionnels représentant le nombre de ventes
  geom_sf(data = comCtr, aes(size = nbmut),
          pch = 21, color = "black", stroke = 0.2) +
  # Ajustement de la taille des cercles en fonction du nombre de ventes
  scale_size_area(max_size = 2) +
  # Thème et autres annotations
  theme_bw() +
  coord_sf(datum = NA) +
  annotation_scale() +
  annotation_north_arrow(height = unit(0.5, "cm"), 
                         width = unit(0.5, "cm"), 
                         pad_y = unit(0.7, "cm")) +
  # Légendes et titres
  labs(
    size = "Transactions", 
    fill = "Prix (€/m²)", 
    title = "Prix de l'immobilier et nombre de ventes dans le 40", 
    subtitle = "Médiane des prix au m² et nombre de ventes par commune (2022-2023)", 
    caption = "Source : DGFiP-DGALN-Cerema DVF+, 2024\nRéalisation : Master GEOTER, 2025"
  )




