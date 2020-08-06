library(OpenML)
library(dplyr)
library(ggplot2)
library(ggpubr)


all_datasets <- listOMLDataSets()
#Filter to binary target variable
selected_datasets <- all_datasets[all_datasets$number.of.classes==2, ]
#Checking duplicates from versioning
duplicate <- group_by(selected_datasets, name)%>%count()%>%filter(n>1)
duplicate_id <- selected_datasets$data.id[selected_datasets$name %in% duplicate$name]
#Leaving only the oldest version of data set (versions follow the order of uploads according to OML)
selected_versions <- group_by(selected_datasets[selected_datasets$name %in% duplicate$name,], name)%>%
  slice(which.min(version))
#Update selected data sets
to_remove <- duplicate_id[!duplicate_id %in% selected_versions$data.id]
selected_datasets <- selected_datasets[!selected_datasets$data.id %in% to_remove, ]
datasets <- selected_datasets[, c("data.id", "name", 
                                  "number.of.features", "number.of.instances", 
                                  "number.of.numeric.features", "number.of.symbolic.features")]
datasets <- datasets[complete.cases(datasets), ]
#IDs datasets with missings
miss_ids <- read.csv(file = "datasets_store/datasets_selection/selected_datasets.csv")
miss_ids <- miss_ids$ID


#Frame with numeric columns only
to_cluster <- datasets[, -c(1,2)]
to_cluster <- scale(to_cluster)

kmean_wcss <- function(k) {
  cluster <- kmeans(to_cluster, k)
  return (cluster$tot.withinss)
}

#Choice of clusters number for experiment
min_k <- 2
max_k <- 50
wcss <- sapply(min_k:max_k, kmean_wcss)

elbow <- data.frame("k" = min_k:max_k, "wcss" = wcss)

#"Elbow" plot for k selection 
ggplot(elbow, aes(k, wcss))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = seq(min_k, max_k, by = 1))

#Choice of cluster numbers (according to plot k=13)

result <- kmeans(to_cluster, 13)

#Adding info about clusters to frame
datasets$clusters <- result$cluster
datasets$miss <- rep(FALSE, nrow(datasets))
datasets$miss[datasets$data.id %in% miss_ids] <- TRUE


#Distribution of datasets with missings on clusters
ggplot(datasets, aes(clusters, fill = miss))+
  geom_bar(stat = 'count')

no_of_miss <- group_by(datasets, clusters)%>%
  filter(miss==TRUE)%>%
  count()

ggplot(no_of_miss, aes(clusters, n))+
  geom_bar(stat = "identity")


#Visualization with PCA, only clusters with missings
to_cluster <- as.data.frame(to_cluster)
to_cluster$clusters <- result$cluster
to_cluster$miss <- rep(FALSE, nrow(datasets))
to_cluster$miss[datasets$data.id %in% miss_ids] <- TRUE
only_clust_w_miss <- to_cluster[to_cluster$clusters %in% no_of_miss$clusters, ]

pca <- prcomp(only_clust_w_miss, scale. = TRUE)
coord <- as.data.frame(pca$x)
coord$clusters <- factor(only_clust_w_miss$clusters)
coord$miss <- only_clust_w_miss$miss

ggscatter(coord, x = "PC1", y = "PC2", color = "clusters", shape = "miss")


###Quick function for generating datasets summary with clusters of chosen number###
###Requires evoking whole script###
cluster_datasets <- function(k){
  result <- kmeans(to_cluster, k)
  datasets$clusters <- result$cluster
  datasets$miss <- rep(FALSE, nrow(datasets))
  datasets$miss[datasets$data.id %in% miss_ids] <- TRUE
  return(datasets)
}


#clustering_13 <- cluster_datasets(13)
#clustering_30 <- cluster_datasets(30)
# write.csv(clustering_13, file = "datasets_store/clustering_experiment/clustering_13.csv")
# write.csv(clustering_30, file = "datasets_store/clustering_experiment/clustering_30.csv")

