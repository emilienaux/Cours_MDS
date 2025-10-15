library(tidyverse)
library(ggrepel)
library(patchwork)


fraise <- read.csv2("~/Documents/analyse fact/fraise.csv", sep = ";", header = TRUE)
fraise$CJ = as.factor(fraise$CJ)
summary(fraise)
head(fraise)

# Moyenne des notes pour chaque type de fraise et chaque descripteur sensoriel
fraise_wide <- fraise %>%
  group_by(ProductName, AttributeName) %>%
  summarise(Note_moy = mean(Note, na.rm = TRUE)) %>%
  pivot_wider(names_from = AttributeName, values_from = Note_moy)

## Calcule des distance puis MDS
distances_fraise <- dist(fraise_wide[, -1])  # exclut la colonne ProductName
mds_fraise <- cmdscale(distances_fraise)

## Mise en forme data frame pour la visualisation
mds_fraise_df <- as.data.frame(mds_fraise)
colnames(mds_fraise_df) <- c("Dim1", "Dim2")
mds_fraise_df$ProductName <- fraise_wide$ProductName

##Corrélations entre descripteurs sensoriels et axes MDS 
correlations <- cor(fraise_wide[, -1], mds_fraise_df[, 1:2])

df_cor <- as.data.frame(correlations)
df_cor$Attribut <- rownames(df_cor)

##Création du graphique (types de fraises + descripteurs senso) 
ggplot() +
  geom_point(data = mds_fraise_df,
             aes(x = Dim1, y = Dim2, color = ProductName),
             size = 4) + ## indique le type de fraise 
  geom_text_repel(data = mds_fraise_df,
                  aes(x = Dim1, y = Dim2, label = ProductName, color = ProductName),
                  size = 4, fontface = "bold") + ##légende 
  
  geom_segment(data = df_cor, ## Flèches = descripteurs sensoriels
               aes(x = 0, y = 0, xend = Dim1 * 2, yend = Dim2 * 2),  # *2 projette plus loins donc meilleur visibilité 
               arrow = arrow(length = unit(0.5, "cm")), color = "coral") +
  geom_text_repel(data = df_cor,
                  aes(x = Dim1 * 2, y = Dim2 * 2, label = Attribut),
                  color = "blue", size = 4) + ## nom des descripteurs 
  
  # Axes x et y du graphique + légende et titre
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  coord_equal() +
  labs(
    title = "Carte sensorielle MDS des fraises",
    subtitle = "Représentation des types de fraises et des descripteurs sensoriels",
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14)
  )


##ACP POUR COMPARER 

library(FactoMineR)
library(factoextra)

donnees_acp <- as.data.frame(fraise_wide[, -1]) ## on enlève la colone des types de fraises pour l'analyse
rownames(donnees_acp) <- fraise_wide$ProductName

#ACP
res.acp <- PCA(donnees_acp, scale.unit = TRUE, graph = FALSE)

# Visualisation : individus qui correspondent aux types de fraises) 
fviz_pca_ind(res.acp,
             geom.ind = "point",
             label = fraise_wide$ProductName, 
             col.ind = fraise_wide$ProductName, 
             palette = "Dark2",         # palette pour les couleurs
             repel = TRUE,
             title = "ACP - Types de fraises (individus)")

# Visualisation : variables qui sont les descripteurs sensoriels
fviz_pca_var(res.acp,
             repel = TRUE,
             title = "ACP - Descripteurs sensoriels")

# Individus et variables sur le meme graph 
fviz_pca_biplot(res.acp,
                geom.ind = "point",
                label = fraise_wide$ProductName,
                col.ind = fraise_wide$ProductName,
                col.var = "darkblue",
                palette = "Dark2",
                repel = TRUE,
                title = "Biplot ACP - Fraises et descripteurs sensoriels")
