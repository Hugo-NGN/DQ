library(FactoMineR)
library(factoextra)
library(vcd)
library(MASS)
library(ggrepel)
library(rcompanion)

#data_path <- paste(getwd(), "/data/bank-prepocessed-cleaned.csv",sep='')
data_path <- "./data/bank-preprocessed3-withoutUnknown.csv"
#data_path <- "./data/bank-preprocessed3.csv"

data <- read.csv(file = data_path, sep = ',')
data['X'] <- NULL
# Check the structure of the data
str(data)

for (col in names(data)) {
  if (is.character(data[[col]])) {
    data[[col]] <- as.factor(data[[col]])
  }
}

################################  CRAMER  ####################################
cat_cols <- names(data)[sapply(data, is.factor)]

# Calculer la V de Cramer pour toutes les paires de colonnes
cramer_matrix <- matrix(NA, length(cat_cols), length(cat_cols), dimnames = list(cat_cols, cat_cols))

for (i in 1:length(cat_cols)) {
  for (j in 1:length(cat_cols)) {
    if (i != j) {
      cramer_matrix[i, j] <- cramerV(table(data[[cat_cols[i]]], data[[cat_cols[j]]]))
    } else {
      cramer_matrix[i, j] <- NA  # Diagonale vide
    }
  }
}

# Afficher la matrice des coefficients de V de Cramer
print(cramer_matrix)


################################################################################




# Ensure 'y' is a factor
data$y <- as.factor(data$y)

# Find the index of the supplementary qualitative variable
quali_sup_index <- which(colnames(data) == "y")

# Convert character columns to factors
for (col in 1:ncol(data)) {
    data[, col] <- as.factor(data[, col])
}

# Check for missing values
if (any(sapply(data, function(x) any(is.na(x))))) {
  stop("There are missing values in the dataset. Please handle them before proceeding.")
}

# Perform MCA
acm_result <- MCA(data, quali.sup = quali_sup_index, graph = TRUE)



#### Projection des individus
ind_coords <- data.frame(acm_result$ind$coord)
ind_coords$y <- as.factor(data$y)  # Convertir la variable en facteur si ce n'est pas déjà le cas

# Visualisation avec ggplot2
ggplot(ind_coords, aes(x = Dim.1, y = Dim.2, color = data$y)) +
  geom_point(alpha = 0.5) +  # Ajouter des points colorés par y
  theme_minimal() +
  labs(title = "acm Factor Map",
       x = paste0("Dim 1 (", round(acm_result$eig[1,2], 2), "%)"),
       y = paste0("Dim 2 (", round(acm_result$eig[2,2], 2), "%)")) +
  scale_color_manual(values = c("blue", "red")) 


#### Projection des variables (modalitées)
var_coords <- data.frame(acm_result$var$coord)
var_coords$Type <- "Active"
var_coords$variable <- rownames(var_coords)

sup_coords <- data.frame(acm_result$quali.sup$coord)
sup_coords$Type <- "Supplémentaire"
sup_coords$variable <- rownames(sup_coords)

# Fusionner les deux jeux de données
all_coords <- rbind(var_coords, sup_coords)

# Visualisation améliorée
ggplot(all_coords, aes(x = Dim.1, y = Dim.2, label = variable, color = Type)) +
  geom_point(size = 3, alpha = 0.7) +  # Augmenter la taille des points et ajouter de la transparence
  geom_text_repel(size = 5, max.overlaps = 20) +  # Éviter le chevauchement des labels
  theme_minimal(base_size = 14) +  # Augmenter la taille de police générale
  scale_color_manual(values = c("Active" = "red", "Supplémentaire" = "blue")) +
  labs(title = "Projection des variables dans le plan principal",
       x = paste0("Dim 1 (", round(acm_result$eig[1,2], 2), "%)"),
       y = paste0("Dim 2 (", round(acm_result$eig[2,2], 2), "%)")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  theme(legend.position = "bottom") 


############################  C  A  H  ####################################

# Get individual coordinates
ind_coords <- acm_result$ind$coord

# Calculate the distance matrix
dist_matrix <- dist(ind_coords)

# Hierarchical clustering with Ward's method
cah_result <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(cah_result, main = "Dendrogramme de la CAH sur les composantes de l'ACM")

# Cut into 2 clusters
nb_clusters <- 2
clusters <- cutree(cah_result, k = nb_clusters)

# Add clusters to the dataset
data$cluster <- as.factor(clusters)


#fviz_cluster(list(data = ind_coords, cluster = clusters))

y <- data$y

data_projection <- data.frame(ind_coords, cluster = as.factor(clusters), y = as.factor(y))


colnames(data_projection) <- c("Dim 1", "Dim 2", "Dim 3", "Dim 4", "Dim 5", "cluster")

ggplot(data_projection, aes(x = `Dim 1`, y = `Dim 2`, color = cluster, shape = y)) +
  geom_point(size = 3) +  
  labs(title = "Visualisation des clusters (et des modalités)",
       x = "Composante 1",
       y = "Composante 2",
       color = "Cluster",
       shape = "Y") +
  theme_minimal() +
  scale_shape_manual(values = c(16, 17, 15, 18))  


table(data$y, clusters)


##########################  A. DISCRI  ####################################

# Ensure 'y' is a factor
data$y <- as.factor(data$y)

# Perform Linear Discriminant Analysis
lda_result <- lda(data$y ~ ., data = as.data.frame(ind_coords))

# Print LDA results
print(lda_result)

# Predict groups
lda_pred <- predict(lda_result)

# Add discriminant scores to the data
data_lda <- data.frame(lda1 = lda_pred$x[,1], cluster = data$y)

# Visualization with ggplot2
ggplot(data_lda, aes(x = lda1, fill = cluster)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution des scores LDA (LD1)") +
  theme_minimal()

# Confusion matrix
table(data$y, lda_pred$class)


############################### Q D A  #########################################


qda_result <- qda(data$y ~ ., data = as.data.frame(ind_coords))

# Afficher les résultats de la QDA
print(qda_result)

# Prédiction des groupes
qda_pred <- predict(qda_result)

# Ajouter les scores discriminants à la base de données
data_qda <- data.frame(qda1 = qda_pred$posterior[,1], cluster = data$y)

# Visualisation de la distribution des scores QDA
ggplot(data_qda, aes(x = qda1, fill = cluster)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution des scores QDA (Posteriors de la classe 1)") +
  theme_minimal()

# Matrice de confusion
table(data$y, qda_pred$class)
