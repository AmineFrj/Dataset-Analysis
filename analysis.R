library(FactoMineR)
library(ggplot2)    # besoins de factoextra
library(factoextra) # pour afficher les groups des variables

espvie = read.table("../Binome8/espvie.txt",check.names=T)
#espvie_sc=scale(espvie)
espvie
summary(espvie)
cor(espvie)
var(espvie)
plot(espvie,pch=19)

# ======= afficher les boxplots ========
layout(matrix(c(1,1,2,3),ncol = 2))
#layout.show(3)
boxplot(espvie[,c(1,4,5,6)])
boxplot(espvie[,c(3,7)])
boxplot(espvie[,2],xlab="PNB.h")
layout(1)

#par(mfrow = c(2,2))
#p=ggplot(espvie)
#p + geom_boxplot() + facet_grid(. ~ espvie)
#par(mar=c(1,1,1,1))


#qplot(espvie$espvieH,x=1,geom = "boxplot")

#install.packages("devtools")
#library(devtools)
#library(easyGgplot2)
#ggplot2.boxplot(data=espvie)


#library(cluster)
#library(fpc)
#install.packages(fpc)
#local({r <- getOption("repos"); 
#r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})
#k=kmeans(espvie,4)
#clusplot(espvie, k$cluster)

# === distance de mahalanobis pour la detection d'outliers ====
summary(mahalanobis(espvie,center = colMeans(espvie),cov = cov(espvie)))
boxplot(mahalanobis(espvie,center = colMeans(espvie),
                    cov = cov(espvie)),xlab="Figure 1",
                      main="Mahalanobis Distance")

# ======== kmeans variables =========
pca_espv = PCA(X = espvie,scale.unit = T)
plot(pca_espv$var$contrib)
fviz_contrib(pca_espv, choice = "var", axes = 1, top = 10)
res.kmeans=kmeans(pca_espv$var$coord,centers = 4)
grp <- as.factor(res.k$cluster)
fviz_pca_var(pca_espv, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF","#00992244"),
             legend.title = "Cluster")
# ======== kmeans individuals =========
k=kmeans(pca_espv$ind$coord,3)
plot.PCA(pca_espv,col.ind =k$cluster,choix = "ind")
text(x = espvie)
