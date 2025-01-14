
# Packages
library(sf)
library(dplyr)
library(ggplot2)

# Ouverture des données sur les BdR
dvf <- st_read("data/dvf13.gpkg")

# Bases de la syntaxes --------------------------------------------------------

# Extraction nombre mutation par année
mutan <- dvf %>% 
  as_tibble() %>% 
  group_by(anneemut) %>% 
  summarise(nbmut=n()) %>% 
  filter(anneemut !=2024)

# Création d'un diagramme en barre des transactions
(g <- ggplot()+
  geom_col(data = mutan , aes(x = anneemut, y=nbmut, 
                              fill=nbmut), 
           fill="steelblue")+
  theme_bw()+
  labs(x="Année", 
       y="Volume de mutation", 
       title = "Volume de transactions immobilières dans les\nBouches-du-Rhône", 
       subtitle = "Mutations dans l'immobilier résidentiel (maisons et appartements)", 
       caption = "Source : DGFiP-DGALN-Cerema DVF+\nRéalisation : Master GEOTER"))

ggsave("output/fig_volume_bdr.png", g, height = 5, width = 6.5, dpi = 300)


#mutan$anneemut <- factor(mutan$anneemut, 
#                         levels = mutan %>% arrange(desc(nbmut)) %>% pull(anneemut))

#

# Sauvegarde d'un graphique

dvf23 <- dvf %>% filter(anneemut==2023)

g2 <- ggplot()+
  geom_histogram(data= dvf23, aes(x=pxm2), fill="steelblue", 
                 binwidth = 300)+
  scale_x_continuous(limits = c(0,15000))+
  theme_bw()+
  labs(x="Prix (€/m²)", 
       y="Fréquence", 
       title = "Distribution des prix dans les Bouches-du-Rhône", 
       subtitle = "Immobilier résidentiel en 2023", 
       caption = "Source : DGFiP-DGALN-Cerema DVF+\nRéalisation : Master GEOTER")
g2

ggsave("output/fig_hist_bdr.png", g2, height = 5, width = 6.5, dpi = 300)


#
dvfMars <- dvf %>% 
  filter(substr(code_insee, 1, 3) == "132", 
         maison == 0, 
         anneemut == 2023)

ggplot()+
  geom_boxplot(data =dvfMars, aes(x=code_insee, y=pxm2))+
  scale_y_continuous(limits=c(0,15000))+
  theme(axis.text.x = element_text(angle = 90))


# Evolution des prix par type de bien
evol <- dvf %>% 
  mutate(type=ifelse(maison==1, "Maison", paste("Apt", nbpiece))) %>% 
  filter(substr(code_insee, 1, 3) == "132", 
         anneemut != 2024) %>% 
  group_by(anneemut, type) %>% 
  summarise(pxm2med=median(pxm2), 
            nbmut=n())

ggplot(evol, aes(x=anneemut, y=pxm2med, 
                 colour = type))+
  geom_line()+
  geom_point()





