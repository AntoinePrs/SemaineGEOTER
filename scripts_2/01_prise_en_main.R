
# Prise en main des données DVF+

# Packages
library(DBI) # connexion à une base de données
library(RPostgres) # Drivers PostgreSQL
library(sf) # Données vectorielles en R
library(dplyr) # Manipulation de tableaux de données
library(tidyr) # idem


# Connexion au serveur----------------------------------------------
cred <- read.delim("cred.txt", sep = ',')

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "dvfplus", 
                 host = cred$host, 
                 port = cred$port, 
                 user = cred$user, 
                 password = cred$mdp)


# Requête de chargement des données
dvf <- st_read(con, query = "SELECT idmutation, datemut, anneemut, moismut, 
coddep, libnatmut, vefa, valeurfonc, l_codinsee, sterr, sbati, nblocmai, 
nblocapt, nblocdep, nbapt1pp, nbapt2pp, nbapt3pp, nbapt4pp, nbapt5pp,
nbmai1pp, nbmai2pp, nbmai3pp, nbmai4pp, nbmai5pp, geomlocmut AS geom
        FROM dvf.mutation
        WHERE coddep = '13'
        AND nbcomm = 1
        AND libtypbien IN ('UNE MAISON', 'UN APPARTEMENT')
        AND geomlocmut IS NOT NULL
        AND valeurfonc IS NOT NULL")

# Nettoyage des variables ------------------------------------------------------

# Code insee
dvf2 <- dvf
dvf2$l_codinsee <- as.character(substr(dvf$l_codinsee, 2, 6))

dvf2 <- mutate(dvf, code_insee = as.character(substr(l_codinsee, 2, 6)))

dvf2 <- dvf %>%
  mutate(code_insee = as.character(substr(l_codinsee, 2, 6)))


# Nettoyage du code INSEE et calcul du prix au m²
dvf <- dvf %>%
  mutate(code_insee = as.character(substr(l_codinsee, 2, 6)),
         pxm2 = valeurfonc/sbati)

# Extraction des colonnes sur le nombre de pièces
prov <- dvf %>% 
  as_tibble() %>% 
  select(idmutation, nbapt1pp:nbmai5pp)

# Table pivot des pieces des appt et maisons
piece_long <- prov %>% 
  pivot_longer(nbapt1pp:nbmai5pp) %>% 
  filter(value==1) %>% 
  mutate(nbpiece=substr(name, 6, 6)) %>% 
  select(idmutation, nbpiece)

# Jointure entre la donnée des pièces et dvf
dvf <- left_join(dvf, piece_long, 
                 by="idmutation")

# variable vrai/faux pour les maisons
dvf <- dvf %>% 
  mutate(maison = nblocmai)

# Ecriture des données des BdR dans un geopackage
st_write(dvf, "data/dvf13.gpkg")


# Statistiques descriptives

# Nombre de maisons et d'appartements 
dvf %>%  
  as_tibble() %>% 
  group_by(maison) %>% 
  summarise(volume=n()) 

# Nombre de transactions immobilières par an
dvf %>%  
  as_tibble() %>% 
  group_by(anneemut) %>% 
  summarise(volume=n()) 

# Communes dans lesquelles il y a eu le plus de ventes immobilières
dvf %>% 
  as_tibble() %>% 
  group_by(code_insee) %>% 
  summarise(nbmut=n()) %>% 
  arrange(desc(nbmut)) %>% 
  slice(1:10)

# Prix médian dans chaque commune
dvf %>% 
  as_tibble() %>% 
  group_by(code_insee) %>% 
  summarise(pxm2med = median(pxm2)) %>% 
  arrange(desc(pxm2med)) %>% 
  slice(1:15)

# Evolution des prix au cours du temps 
dvf %>% 
  as_tibble() %>% 
  filter(maison==0, 
         nbpiece==1, 
         code_insee=='13201', 
         anneemut != 2024) %>% 
  group_by(anneemut) %>% 
  summarise(pxm2med = median(pxm2), 
            pxmed = median(valeurfonc))





