

# Chargement des packages
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)

# Syntaxe d'une fonction
nom_de_la_fonction <- function(argument1, argument2){
  
  resultat <- argument1 + argument2

  return(resultat)  
}

# Exécution
nom_de_la_fonction(1, 7)


# Fonction qui affiche la carte d'un département ------------------------------

com <- st_read("data/COMMUNE.shp")

#==============================================================================
plot_departement <- function(code_departement){
  # Filtrer les communes avec l'argument (code département)
  comdep <- com %>% filter(INSEE_DEP == code_departement)
  
  comdeCtr <- st_point_on_surface(comdep)
  
  # Stocker la représentation dans un objet
  g <- ggplot()+
    geom_sf(data = comdep)+
    geom_sf(data = comdeCtr, aes(size=POPULATION), 
            alpha=.6)+
    scale_size_area()+
    theme_bw()+
    coord_sf(datum = NA)+
    annotation_scale()+
    labs(title = paste("Population du", code_departement))
  
  # Exporte l'objet
  ggsave(paste("outputs/cartodep/", 
               code_departement, ".png"), 
         width = 6, 
         height = 5,
         dpi = 300)
}
#==============================================================================

plot_departement("59")


# Boucles -----------------------------------------------

# La boucle for
for(i in 1:5){
  
  print(paste("Itération", i))
  
}

# La boucle while
i=1

while(i < 6){
  
  print(paste("Itération", i))
  
  i = i + 1
  
}



# Boucle qui exporte les cartes des départements 
for(i in c("04", "05", "06", "13", "83", "84")){
  
  plot_departement(i)
  
}







