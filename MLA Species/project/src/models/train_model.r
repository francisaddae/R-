library(data.table)
library(Rtsne)
library(ClusterR)
library(ggplot2)
library(caret)

rm(list = ls())

##### Data Imports/Wrangling ####
traindata <- fread('project/volume/data/raw/Gene_data.csv')
sampleSub <- fread('project/volume/data/raw/example_sub.csv')

traindata[, id :=NULL]
traindata

#factoring data 
traindata$locus_1 <- as.factor(traindata$locus_1)
traindata$locus_2 <- as.factor(traindata$locus_2)
traindata$locus_3 <- as.factor(traindata$locus_3)
traindata$locus_4 <- as.factor(traindata$locus_4)
traindata$locus_5 <- as.factor(traindata$locus_5)
traindata$locus_6 <- as.factor(traindata$locus_6)
traindata$locus_7 <- as.factor(traindata$locus_7)
traindata$locus_8 <- as.factor(traindata$locus_8)
traindata$locus_9 <- as.factor(traindata$locus_9)
traindata$locus_10 <- as.factor(traindata$locus_10)
traindata$locus_11 <- as.factor(traindata$locus_11)
traindata$locus_12 <- as.factor(traindata$locus_12)
traindata$locus_13 <- as.factor(traindata$locus_13)
traindata$locus_14 <- as.factor(traindata$locus_14)
traindata$locus_15 <- as.factor(traindata$locus_15)
traindata

#creating dummy
dummy <- dummyVars(~.,data = traindata)
dummy

#predict 
traindata <- data.table(predict(dummy,newdata = traindata))
traindata


train <- data.frame(lapply(traindata, jitter, factor= 0.00001))
train

##### PCA #### 
bst_mod <- prcomp(train, scale. = TRUE, center = TRUE)
bst_mod


#plots 
screeplot(bst_mod)
plot(bst_mod, type = "l", main = "Varaince of PCA") #--> importance of 3 

#obtaining PCA data points  
pca_dt <- data.table(unclass(bst_mod)$x)
pca_dt

##### TSNE ####  <- hyperparammeter tuning
# https://rpubs.com/marwahsi/tnse <- important stuff
#datacamps 
tsne_pca <- Rtsne(pca_dt,pca = F, dims =2, perplexity = 250, 
                  eta = 500, verbose = TRUE, max_iter = 5000) #tuned Hyperparameters
tsne_pca 

#obtain tsne values 
tsne_dt_pca <- data.table(tsne_pca$Y)
tsne_dt_pca

#plotting 
ggplot(tsne_dt_pca, aes(x= V1, y = V2,color = V1,V2 )) + geom_point()
ggplot(tsne_dt_pca, aes(x= V2, y = V1,color = V2,V1 )) + geom_point()

##### GMM (Guassian Mixture Model) ##### 
#using the pca_dt with 3 dimensions 
pca3D <- Optimal_Clusters_GMM(tsne_dt_pca[,.(V1,V2)],
                              max_clusters = 10,
                              criterion = 'BIC')
pca3D

#model fitting 
k_vals <- c(NA, pca3D[-1] - pca3D[-length(pca3D)]) 
k_vals

#making datatabkes out of it 
delta_K <- data.table(delK = k_vals, K = 1:length(k_vals))
delta_K

#ploting the K
ggplot(delta_K, aes(x = K, y = -delK)) + geom_point() + geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_text(aes(label = K), hjust = 0, vjust = -1)


finK <- 3


##### Modeling #### 
gmm <- GMM(tsne_dt_pca[,.(V1,V2)], finK)
gmm
#using the train data (with jitter)
l_clust_data <- data.table(gmm$Log_likelihood^10)
net_lh_data <- apply(l_clust_data, 1, FUN = function(x){sum (1/x)}) 
cluster_proba<- 1 /( net_lh_data * l_clust_data)
cluster_prob_data <- predict_GMM(tsne_dt_pca,
                                 gmm$centroids,
                                 gmm$covariance_matrices,
                                 gmm$weights)$cluster_proba
cluster_prob_data <- data.table(cluster_prob_data)
cluster_prob_data

#plot 
ggplot(tsne_dt_pca, aes(x = V1, y = V2, col = cluster_prob_data$V2)) + 
  geom_point() + ggtitle("   Species1   ")
ggplot(tsne_dt_pca, aes(x = V1, y = V2, col = cluster_prob_data$V3)) + 
  geom_point() + ggtitle("   Species2   ")
ggplot(tsne_dt_pca, aes(x = V1, y = V2, col = cluster_prob_data$V1)) + 
  geom_point() + ggtitle("   Species3   ")


fin <- data.table(id <- sampleSub$id,
                  species1 <- cluster_prob_data$V2,
                  species2 <- cluster_prob_data$V3,
                  species3 <- cluster_prob_data$V1)
setnames(fin, c(names(fin)), c(names(sampleSub)))
fin
fwrite(fin, 'project/volume/data/processed/submission.csv')

