

library(FactoMineR)
library(factoextra)
library(MASS)
library(ggplot2)
data <- read.csv("./data/bank-additional.csv")

acm_result <- MCA(data, quali.sup = 13, graph = FALSE)  # Typeclient est la 1ère colonne

fviz_mca_ind(acm_result, 
             col.ind = "gray", 
             habillage = 1,  # Coloration selon "Typeclient"
             label = "none",  # Supprime les labels des individus
             repel = TRUE)    # Évite le chevauchement des labels

fviz_mca_var(acm_result, 
             col.var = "blue", 
             repel = TRUE)  # Pour éviter le chevauchement des étiquettes


fviz_mca_var(acm_result, 
             col.var = "blue", 
             repel = TRUE)  # Pour éviter le chevauchement des étiquettes


############################  C  A  H  ####################################
ind_coords <- acm_result$ind$coord

# Calculer une matrice de distance (euclidienne par défaut)
dist_matrix <- dist(ind_coords)

# Réaliser la classification ascendante hiérarchique (CAH) avec la méthode de Ward
cah_result <- hclust(dist_matrix, method = "ward.D2")

# Visualiser le dendrogramme
plot(cah_result, main = "Dendrogramme de la CAH sur les composantes de l'ACM")


nb_clusters <- 2
clusters <- cutree(cah_result, k = nb_clusters)

# Ajouter les groupes au dataset
data$cluster <- as.factor(clusters)

# Visualisation des clusters sur le plan factoriel
fviz_cluster(list(data = ind_coords, cluster = clusters))


##########################  A. DISCRI  ####################################

ind_coords <- acm_result$ind$coord

typeclient <- as.factor(data[, 1])  # La première colonne est "Typeclient"

# Réaliser l'Analyse Discriminante Linéaire avec Typeclient comme variable cible
lda_result <- lda(typeclient ~ ., data = as.data.frame(ind_coords))

# Afficher les résultats
print(lda_result)

# Prédictions des groupes
lda_pred <- predict(lda_result)

# Ajouter les scores discriminants aux données
data_lda <- data.frame(lda1 = lda_pred$x[,1], cluster = typeclient)

# Visualisation des résultats avec un graphique de densité
library(ggplot2)
ggplot(data_lda, aes(x = lda1, fill = cluster)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution des scores LDA (LD1)") +
  theme_minimal()

table(lda_pred$class, typeclient)