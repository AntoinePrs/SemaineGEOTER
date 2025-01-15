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
dvf <- dbGetQuery(con, "
SELECT idmutation, datemut, anneemut, moismut, 
coddep, libnatmut, vefa, valeurfonc, l_codinsee, sterr, sbati, nblocmai, 
nblocapt, nblocdep, nbapt1pp, nbapt2pp, nbapt3pp, nbapt4pp, nbapt5pp,
nbmai1pp, nbmai2pp, nbmai3pp, nbmai4pp, nbmai5pp
FROM dvf.mutation
WHERE nbcomm = 1
AND coddep = '84'
AND anneemut != '2024'
AND libtypbien IN ('UNE MAISON', 'UN APPARTEMENT')
AND geomlocmut IS NOT NULL
AND valeurfonc IS NOT NULL")

# Filtrer
dvf <- dvf %>%
  mutate(code_insee = as.character(substr(l_codinsee, 2 , 6)),
         pxm2 = valeurfonc/sbati, 
         maison = nblocmai)

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

# Calcul de l'évolution par type de bien
evol <- dvf %>% 
  mutate(type=ifelse(maison==1, "Maison", paste("Apt", nbpiece))) %>% 
  group_by(anneemut, type) %>% 
  summarise(pxm2med=median(pxm2), 
            nbmut=n())

# On extrait les données de 2014
evol14 <- evol %>% 
  ungroup() %>% 
  filter(anneemut==2014) %>% 
  select(type, pxm2med14=pxm2med)

# jointure de l'année 2014 sur toutes les années
evol <- left_join(evol, evol14, by = "type")

evol$base100 <- evol$pxm2med/evol$pxm2med14*100

# Visualisation 
ggplot(evol, aes(x=anneemut, y=base100, 
                 colour = type))+
  geom_line()+
  geom_point()+
  theme_bw()+
  labs(x="Année", 
       y="Evolution (base 100 en 2014)", 
       caption = "Source : DGFiP-DGALN-Cerema DVF+\nRéalisation : Master GEOTER")


