library(FactoMineR)
library(factoextra)
library(vcd)
library(rcompanion)
data_path <- paste(getwd(), "/data/bank-prepocessed-cleaned.csv",sep='')
data <- read.csv(file = data_path, sep = ',')
data['X'] <- NULL
# Check the structure of the data
str(data)

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

# Visualization of clusters
fviz_cluster(list(data = ind_coords, cluster = clusters))

table(clusters, data$y)


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
table(lda_pred$class, data$y)


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
table(qda_pred$class, data$y)
