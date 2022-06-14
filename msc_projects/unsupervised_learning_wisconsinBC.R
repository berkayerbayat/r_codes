# DSM 5008 FINAL

library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("pastecs")
library(pastecs)
library(corrplot)
library(magrittr)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library(stats)
library(corrplot)
library(factoextra)
library(stats)
library(rstatix)

df <- read.csv("/Users/berkayerbayat/Desktop/GIT/GitforR/msc_projects/datasets/wdbc.data",header=F)

head(df)
colnames(df)[1:2] <- c("ID", "diagnosis")

new_df <- df %>% 
  select(diagnosis, V3:V12)
head(new_df)

numeric_summary <- function(data){
  #requires pastect package
  data %>% 
    select_if(is.numeric) %>% 
    stat.desc() %>% 
    t()
}

numeric_summary(new_df)

new_df %<>% 
  na.omit()

# Değişkenler için uzaklık matrisi elde ettik

num_data <- select_if(new_df,is.numeric)

res <- cor(num_data)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

chart.Correlation(res, histogram=TRUE, pch=19)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

# kutu grafiği çizdireceğiz
par(mfrow=(c(2,5)))

for (i in num_data) {
  boxplot(i)
}
dev.off()
boxplot(num_data)


#Temel bileşen analizi uygulayalım

df_pca <- prcomp(num_data, center = TRUE, scale. = TRUE)
summary(df_pca)
#PC2 seçilebilir

# 3 BOYUTLU PCA GÖRSELLEŞTİRMESİ

summary(df_pca)

prin_comp <- prcomp(num_data, center = TRUE, scale. = TRUE)

summary(prin_comp)

axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
            ticklen=4,
            titlefont=list(size=13))


fig <- new_df %>%
  plot_ly()
fig <- fig %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label='V3', values=~V3),
      list(label='V4', values=~V4),
      list(label='V5', values=~V5),
      list(label='V6', values=~V6),
      list(label='V7', values=~V7),
      list(label='V8', values=~V8),
      list(label='V9', values=~V9),
      list(label='V10', values=~V10),
      list(label='V11', values=~V11),
      list(label='V12', values=~V12)
    ),
    color = ~diagnosis,
    marker = list(
      size = 7,
      line = list(
        width = 1,
        color = 'rgb(230,230,230)'
      )
    )
  )
fig <-  fig %>% style(diagonal = list(visible = FALSE))
fig <- fig %>%
  layout(
    hovermode='closest',
    dragmode= 'select',
    plot_bgcolor='rgba(240,240,240, 0.95)',
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    xaxis2=axis,
    xaxis3=axis,
    xaxis4=axis,
    yaxis2=axis,
    yaxis3=axis,
    yaxis4=axis
  )

fig

explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
explained_variance_ratio <- 100 * explained_variance_ratio
components <- prin_comp[["x"]]
components <- data.frame(components)
components <- cbind(components, new_df$diagnosis)
components$PC3 <- -components$PC3
components$PC2 <- -components$PC2

axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
            ticklen=4,
            titlefont=list(size=13))

fig <- components %>%
  plot_ly()  %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label=paste('PC 1 (',toString(round(explained_variance_ratio[1],1)),'%)',sep = ''), values=~PC1),
      list(label=paste('PC 2 (',toString(round(explained_variance_ratio[2],1)),'%)',sep = ''), values=~PC2),
      list(label=paste('PC 3 (',toString(round(explained_variance_ratio[3],1)),'%)',sep = ''), values=~PC3),
      list(label=paste('PC 4 (',toString(round(explained_variance_ratio[4],1)),'%)',sep = ''), values=~PC4)
    ),
    color = ~new_df$diagnosis, colors = c('#636EFA','#EF553B')
  ) %>%
  style(diagonal = list(visible = FALSE)) %>%
  layout(
    legend=list(title=list(text='color')),
    hovermode='closest',
    dragmode= 'select',
    plot_bgcolor='rgba(240,240,240, 0.95)',
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    xaxis2=axis,
    xaxis3=axis,
    xaxis4=axis,
    yaxis2=axis,
    yaxis3=axis,
    yaxis4=axis
  )

fig

df_pca$rotation


fviz_contrib(df_pca, choice = "var", axes = 1, fill="lightblue" ,top = 6, ggtheme = theme_gray())

fviz_contrib(df_pca, choice = "var", axes = 2, top = 4,fill="lightblue" , ggtheme = theme_gray())

fviz_contrib(df_pca, choice = "var", axes = 3, top = 4,fill="lightblue" , ggtheme = theme_gray())


b <- df_pca$x

fviz_eig(df_pca)
screeplot(df_pca, type='lines')


summary(df)
fviz_pca_ind(df_pca,
             col.ind = "cos2",
             gradient.cols = c("Darkblue", "blue", "lightblue"),
             repel = TRUE     
)
fviz_pca_biplot(df_pca, repel = TRUE,
                col.var = "darkblue", # Variables color
                col.ind = "gray"  # Individuals color
)

fviz_pca_ind(df_pca, axes = c(1, 3),
             col.ind = "cos2",
             gradient.cols = c("Darkblue", "blue", "lightblue"),
             repel = TRUE     
)
fviz_pca_biplot(df_pca,axes = c(1, 3), repel = TRUE,
                col.var = "darkblue", # Variables color
                col.ind = "gray"  # Individuals color
)
fviz_pca_ind(df_pca, axes = c(2, 3),
             col.ind = "cos2",
             gradient.cols = c("Darkblue", "blue", "lightblue"),
             repel = TRUE     
)
fviz_pca_biplot(df_pca,axes = c(2, 3), repel = TRUE,
                col.var = "darkblue", # Variables color
                col.ind = "gray"  # Individuals color
)


# K MEANS UYGULAMASI

# bileşenlere karşılık yeni gözlem değerlerini elde ediniz

pca_df <- as.data.frame(-df_pca$x[,c(1,2,3)])
head(pca_df)

fviz_nbclust(pca_df, kmeans, method = "wss") # 5-6 kümeler denilebilir
fviz_nbclust(pca_df, kmeans, method = "silhouette") #silhoutte 2
fviz_nbclust(pca_df, kmeans, method = "gap_stat") # 2 veriyor

set.seed(123)
km_3 <- kmeans(pca_df, 3, nstart = 50)
print(km_3)

km_4 <- kmeans(pca_df, 4, nstart = 25)
print(km_4)

km_5 <- kmeans(pca_df, 5, nstart = 25)
print(km_5)

km_6 <- kmeans(pca_df, 6, nstart = 25)
print(km_6)

km_7 <- kmeans(pca_df, 7, nstart = 25)
print(km_7)

km_8 <- kmeans(pca_df, 8, nstart = 25)
print(km_8)


summary(num_data)
aggregate(num_data, by=list(cluster=km_3$cluster), mean)
aggregate(num_data, by=list(cluster=km_4$cluster), mean)
aggregate(num_data, by=list(cluster=km_5$cluster), mean)
aggregate(num_data, by=list(cluster=km_6$cluster), mean)
aggregate(num_data, by=list(cluster=km_7$cluster), mean)
aggregate(num_data, by=list(cluster=km_8$cluster), mean)

fviz_cluster(km_3, data = num_data,
             ellipse.type = "convex",
             star.plot = TRUE, 
             repel = TRUE,
             ggtheme = theme_gray()
)

fviz_cluster(km_4, data = num_data,
             ellipse.type = "convex",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_gray()
)

fviz_cluster(km_5, data = num_data,
             ellipse.type = "convex",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_gray()
)
# 5 den sonra 4. kümeyi 3. PCA boyutunda bölüyor ancak iki boyutta bu ayrımı biz farkedemiyoruz

tit = 'Total Explained Variance = 99.99'
km_6


fig_6 <- plot_ly(km_6, x = ~PC1, y = ~PC2, z = ~PC3, color=cluster  ) %>%
  add_markers(size = 12)


fig <- fig %>%
  layout(
    title = tit,
    scene = list(bgcolor = "#e5ecf6")
  )

fig

fviz_cluster(km_6, data = num_data,
             ellipse.type = "convex",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_gray()
)

fviz_cluster(km_7, data = num_data,
             ellipse.type = "convex",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_gray()
)

fviz_cluster(km_8, data = num_data,
             ellipse.type = "convex",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_gray()
)

# K medoids

fviz_nbclust(pca_df, pam, method= "wss") 
pam_data_3 <- pam(pca_df,3)
print(pam_data_3)
fviz_cluster(pam_data_3,
             ellipse.type = "convex",
             repel = TRUE,
             ggtheme = theme_classic()
)
fviz_nbclust(pca_df, pam, method= "wss") 
fviz_nbclust(pca_df, pam, method= "silhouette")
fviz_nbclust(pca_df, pam, method= "gap")

pam_data_3 <- pam(data,3)
print(pam_data_3)
fviz_cluster(pam_data_3,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

b <- pam_data_3$cluster
bad <- as.data.frame(as.factor(b)[as.factor(b)==1])
help <- df[row.names(bad),]
help

fviz_nbclust(help, pam, method= "wss") 
fviz_nbclust(help, pam, method= "silhouette")
fviz_nbclust(help, pam, method= "gap")

pam_data_3 <- pam(help,3)
print(pam_data_3)
fviz_cluster(pam_data_3,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
pam_data_3$clusinfo

# Hierarchical clustering

dist_euc <- dist(pca_df, method="euclidean")
dist_man <- dist(pca_df, method="manhattan")
as.matrix(dist_euc)[1:6,1:6]
as.matrix(dist_man)[1:6,1:6]

##ward.D2 ba?lant? fonksiyonu ile
hc_e <- hclust(d=dist_euc, method="ward.D2")
plot(hc_e)

hc_m <- hclust(d=dist_man, method="ward.D2")
plot(hc_m)

library("factoextra")
fviz_dend(hc_e,cex=.5) #cex yaz? b?y?kl??? i?indir

fviz_dend(hc_m,cex=.5) #cex yaz? b?y?kl??? i?indir

coph_e <- cophenetic(hc_e)
as.matrix(coph_e)[1:6,1:6]
as.matrix(dist_euc)[1:6,1:6]
cor(dist_euc,coph_e)

coph_m <- cophenetic(hc_m)
as.matrix(coph_m)[1:6,1:6]
as.matrix(dist_man)[1:6,1:6]
cor(dist_man,coph_m)

###avarage ba?lant? fonksiyonu ile
hc_e2 <- hclust(d=dist_euc, method="average")
plot(hc_e2)
hc_m2 <- hclust(d=dist_man, method="average")
plot(hc_m2)


fviz_dend(hc_e2,cex=.5) #cex yaz? b?y?kl??? i?indir

fviz_dend(hc_m2,cex=.5) #cex yaz? b?y?kl??? i?indir

coph_e2 <- cophenetic(hc_e2)
as.matrix(coph_e2)[1:6,1:6]
as.matrix(dist_euc)[1:6,1:6]
cor(dist_euc,coph_e2)

coph_m2 <- cophenetic(hc_m2)
as.matrix(coph_m2)[1:6,1:6]
as.matrix(dist_man)[1:6,1:6]
cor(dist_man,coph_m2)

#### cut tree in 4 groups
grup <- cutree(hc_e, k=4)
grup
table(pca_df)
rownames(pca_df)[grup==1]  
rownames(pca_df)[grup==2]  
rownames(pca_df)[grup==3] 
rownames(pca_df)[grup==4]  

fviz_dend(hc_e, k = 4, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


fviz_cluster(list(data = pca_df, cluster = grup),
             palette = c("#2E9FDF", "#00FF00", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())


#### cut tree in 3 groups
grup <- cutree(hc_e, h=7)
grup
table(grup)
rownames(pca_df)[grup==1]  
rownames(pca_df)[grup==2]  
rownames(pca_df)[grup==3] 



fviz_dend(hc_e, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


fviz_cluster(list(data = data, cluster = grup),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())


#####AGNES-DIANA####
# Agglomerative Nesting (Hierarchical Clustering)
res.agnes <- agnes(x = scale(USArrests), # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward" # Linkage method
)

fviz_dend(res.agnes, cex = 0.6, k = 4)


# DIvisive ANAlysis Clustering
res.diana <- diana(x = scale(USArrests), # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean" # metric for distance matrix
                   
)

fviz_dend(res.diana, cex = 0.6, k = 4)


# MODEL BASED CLUSTER
library(MASS)
data <- geyser
dim(data)
summary(data)
help(geyser)
head(data)
plot(data$duration,data$waiting)
boxplot(data)

# Scatter plot
library("ggpubr")
ggscatter(pca_df, x = "PC1", y = "PC2")+
  geom_density2d() # Add 2D density

library(mclust)
mc <- Mclust(pca_df)
summary(mc)
View(mc)
head(mc$z)
head(mc$classification,10)


library(factoextra)
# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)


#G=3
mc <- Mclust(data, G=3)
summary(mc)
head(mc$classification,10)
mc$z

# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)

###################################
#### Density-Based Clustering
###################################
library(factoextra)
set.seed(123)
fviz_nbclust(pca_df, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
km.res <- kmeans(pca_df, 5, nstart = 25)
km.res
fviz_cluster(km.res, num_data, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

#install.packages("fpc")
library(fpc)
#install.packages("dbscan")
library(dbscan)

# Compute DBSCAN using fpc package
set.seed(123)

kNNdistplot(num_data, k = 5)
abline(h = 0.15, lty = 2)

db <- dbscan(num_data, eps = 0.15, MinPts = 5)
print(db)

# Plot DBSCAN results
fviz_cluster(db, data = num_data, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())


#  Küme geçerliliği istatistiklerini de dikkate alarak seçtiğiniz en uygun kümeleme analizi
# yöntemini gerekçelerinizle belirtiniz. (Diğer şıklar içinde değerlendirildiyse burada özet bilgi
#                                           şeklinde verilebilir.)





# 11. Finalde elde etmiş olduğunuz kümelerin tanımlayıcı istatistiklerini elde ederek yorumlayınız.

t(t(df_pca$x %*% t(df_pca$rotation)) * df_pca$scale + df_pca$center)
t(t(df_pca$x %*% t(df_pca$rotation)) + df_pca$center)


