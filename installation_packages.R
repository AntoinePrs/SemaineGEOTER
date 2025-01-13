# Liste des packages nécessaires
required_packages <- c(
  "DBI", "RPostgres", "sf", "dplyr", "tidyr", "knitr", 
  "cowplot", "mapview", "ggplot2", "ggspatial", "RColorBrewer", 
  "readr", "mapsf", "spdep", "rgeoda"
)

# Vérification et installation des packages manquants
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) { # Vérifie si le package est installé
    install.packages(pkg) # Installe le package s'il manque
  }
}

# Chargement des packages
lapply(required_packages, library, character.only = TRUE)
