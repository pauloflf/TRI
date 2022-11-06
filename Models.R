#algoritmos
#1- fanny
#2- hierarchical
#3- pam
#4- agnes
#5- clara
#6- sota
#7- kmeans
#8- diana

# Metrics of clusters
# https://mhahsler.github.io/Introduction_to_Data_Mining_R_Examples/book/clustering-analysis.html
# http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization
NumeroClusters<-numeroClasses;
mtxmse<-c();allClassicifation<-c()
mtxTimes<-c()

#Algoritmos
# Fanny - metric = c("euclidean", "manhattan", "SqEuclidean")
time.start<-Sys.time();
fanny.fit<-fanny(as.data.frame(data), NumeroClusters, memb.exp = (NumeroClusters),metric = "euclidean",maxit = 500)
time.stop<-Sys.time();
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes,totalTime)

g1<-clusplot(data, fanny.fit$clust, main='Gráfico dos grupos de Fanny.euclidean', color=TRUE, shade=FALSE)#, labels=2, lines=0)
TempLevel<-as.factor(fanny.fit$clustering)
source("Adjustlevels.R")
outCluster.fanny.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.fanny.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.fanny.fit.euclidean <- roc(dataAnaliseClass, outCluster.fanny.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.fanny.fit.euclidean ",ROC.outCluster.fanny.fit.euclidean$auc ))
source("Evaluation.R")
stepmodel<-cbind("fanny.fit.euclidean",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.fanny.fit,"outCluster.fanny.fit.euclidean.xlsx")


# Fanny - metric = c("euclidean", "manhattan", "SqEuclidean")
time.start<-Sys.time()
fanny.fit<-fanny(as.data.frame(data), NumeroClusters, memb.exp = NumeroClusters,metric = "manhattan",maxit = 500)
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g2<-clusplot(data, fanny.fit$clust, main='Gráfico dos grupos de Fanny.manhattan', color=TRUE, shade=FALSE)
TempLevel<-as.factor(fanny.fit$clustering)
source("Adjustlevels.R")
outCluster.fanny.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.fanny.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.fanny.fit.manhattan <- roc(dataAnaliseClass, outCluster.fanny.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.fanny.fit.manhattan ",ROC.outCluster.fanny.fit.manhattan$auc ))
source("Evaluation.R")
stepmodel<-cbind("fanny.fit.manhattan",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.fanny.fit,"outCluster.fanny.fit.manhattan.xlsx")


# Fanny - metric = c("euclidean", "manhattan", "SqEuclidean")
time.start<-Sys.time()
fanny.fit<-fanny(as.data.frame(data), NumeroClusters, memb.exp = NumeroClusters,metric = "SqEuclidean",maxit = 500)
g3<-clusplot(data, fanny.fit$clust, main='Gráfico dos grupos de Fanny.SqEuclidean', color=TRUE, shade=FALSE)
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

TempLevel<-as.factor(fanny.fit$clustering)
#if(numeroClasses<3){
#print("Binary")
source("Adjustlevels.R")
outCluster.fanny.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.fanny.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.fanny.fit.SqEuclidean <- roc(dataAnaliseClass, outCluster.fanny.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.fanny.fit.SqEuclidean ",ROC.outCluster.fanny.fit.SqEuclidean$auc ))
source("Evaluation.R")
stepmodel<-cbind("fanny.fit.SqEuclidean",stepEvaluation)
#} else {
#print("Multclass")
#source("AdjustlevelsMulticlass.R")
#}

mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.fanny.fit,"outCluster.fanny.fit.SqEuclidean .xlsx")

########################################

#pam - metric = c("euclidean", "manhattan")
time.start<-Sys.time()
pam.fit<-pam(as.data.frame(data), NumeroClusters, diss = inherits(data, "dist"),metric = "euclidean")
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g4<-clusplot(data, pam.fit$clust, main='Gráfico dos grupos de Pam.euclidean', color=TRUE, shade=FALSE)
TempLevel<-as.factor(pam.fit$clustering)
source("Adjustlevels.R")
outCluster.pam.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.pam.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.pam.fit.euclidean <- roc(dataAnaliseClass, outCluster.pam.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.pam.fit.euclidean ",ROC.outCluster.pam.fit.euclidean$auc ))
source("Evaluation.R")
stepmodel<-cbind("pam.fit.euclidean",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.pam.fit,"outCluster.pam.fit.euclidean.xlsx")

time.start<-Sys.time()
pam.fit<-pam(as.data.frame(data), NumeroClusters, diss = inherits(data, "dist"),metric = "manhattan")
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g5<-clusplot(data, pam.fit$clust, main='Gráfico dos grupos de Pam.manhattan', color=TRUE, shade=FALSE)
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

TempLevel<-pam.fit$clustering
source("Adjustlevels.R")
outCluster.pam.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.pam.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.pam.fit.manhattan <- roc(dataAnaliseClass, outCluster.pam.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.pam.fit.manhattan ",ROC.outCluster.pam.fit.manhattan$auc ))
source("Evaluation.R")
stepmodel<-cbind("pam.fit.manhattan",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.pam.fit,"outCluster.pam.fit.manhattan.xlsx")

#######################################

#clara - metric = c("euclidean", "manhattan", "jaccard")
time.start<-Sys.time()
clara.fit<-clara(as.data.frame(data), NumeroClusters, metric ="euclidean")
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g6<-clusplot(data, clara.fit$clust, main='Gráfico dos grupos de Clara.euclidean', color=TRUE, shade=FALSE)
TempLevel<-clara.fit$clustering
source("Adjustlevels.R")
outCluster.clara.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.clara.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.clara.fit.euclidean <- roc(dataAnaliseClass, outCluster.clara.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.clara.fit.euclidean ",ROC.outCluster.clara.fit.euclidean$auc ))
source("Evaluation.R")
stepmodel<-cbind("clara.fit.euclidean",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.clara.fit,"outCluster.clara.fit.euclidean.xlsx")

time.start<-Sys.time()
clara.fit<-clara(as.data.frame(data), NumeroClusters, metric ="manhattan")
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g7<-clusplot(data, clara.fit$clust, main='Gráfico dos grupos de Clara.manhattan', color=TRUE, shade=FALSE)
TempLevel<-clara.fit$clustering
source("Adjustlevels.R")
outCluster.clara.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.clara.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.clara.fit.manhattan <- roc(dataAnaliseClass, outCluster.clara.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.clara.fit.manhattan ",ROC.outCluster.clara.fit.manhattan$auc ))
source("Evaluation.R")
stepmodel<-cbind("clara.fit.manhattan",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.clara.fit,"outCluster.clara.fit.manhattan.xlsx")

time.start<-Sys.time()
clara.fit<-clara(as.data.frame(data), NumeroClusters, metric ="jaccard")
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)


g8<-clusplot(data, clara.fit$clust, main='Gráfico dos grupos de Clara.jaccard', color=TRUE, shade=FALSE)
TempLevel<-clara.fit$clustering
source("Adjustlevels.R")
outCluster.clara.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.clara.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.clara.fit.jaccard <- roc(dataAnaliseClass, outCluster.clara.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.clara.fit.jaccard ",ROC.outCluster.clara.fit.jaccard$auc ))
source("Evaluation.R")
stepmodel<-cbind("clara.fit.jaccard",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.clara.fit,"outCluster.clara.fit.jaccard.xlsx")

#######################################

#sota
time.start<-Sys.time()
sota.fit<-sota(as.matrix(data),NumeroClusters-1)
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g9<-clusplot(data, sota.fit$clust, main='Gráfico dos grupos de sota', color=TRUE, shade=FALSE)
TempLevel<-sota.fit$clust
source("Adjustlevels.R")
outCluster.sota.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.sota.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.sota.fit<- roc(dataAnaliseClass, outCluster.sota.fit, algorithm = 0);# Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.sota.fit ",ROC.outCluster.sota.fit$auc ))
source("Evaluation.R")
stepmodel<-cbind("sota.fit",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.sota.fit,"outCluster.sota.fit.xlsx")

#clusplot(data, sota.fit$clust, main='Gráfico dos grupos de sota',
#         color=TRUE, shade=FALSE,
#         labels=2, lines=0)

########################################

# k-means - algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"
time.start<-Sys.time()
k.means.fit <- kmeans(as.data.frame(data), NumeroClusters, algorithm = "Hartigan-Wong")
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g10<-clusplot(data, k.means.fit$clust, main='Gráfico dos grupos de K-means.Hartigan-Wong', color=TRUE, shade=FALSE)
TempLevel<-k.means.fit$cluster
source("Adjustlevels.R")
outCluster.k.means.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.k.means.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.k.means.fit.HartiganWong <- roc(dataAnaliseClass, outCluster.k.means.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.k.means.fit.HartiganWong ",ROC.outCluster.k.means.fit.HartiganWong$auc ))
source("Evaluation.R")
stepmodel<-cbind("k.means.fit.HartiganWong",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.k.means.fit,"outCluster.k.means.fit.HartiganWong.xlsx")


#clusplot(data, k.means.fit$cluster, main='Gráfico dos grupos de kmeans',
#         color=TRUE, shade=FALSE,
#       labels=2, lines=0)

time.start<-Sys.time()
k.means.fit <- kmeans(as.data.frame(data), NumeroClusters, algorithm = "Lloyd")
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g11<-clusplot(data, k.means.fit$clust, main='Gráfico dos grupos de K-means.Lloyd', color=TRUE, shade=FALSE)
TempLevel<-k.means.fit$cluster
source("Adjustlevels.R")
outCluster.k.means.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.k.means.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.k.means.fit.Lloyd <- roc(dataAnaliseClass, outCluster.k.means.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.k.means.fit.Lloyd ",ROC.outCluster.k.means.fit.Lloyd$auc ))
source("Evaluation.R")
stepmodel<-cbind("k.means.fit.Lloyd",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.k.means.fit,"outCluster.k.means.fit.Lloyd.xlsx")

#clusplot(data, k.means.fit$cluster, main='Gráfico dos grupos de kmeans', color=TRUE, shade=FALSE,labels=2, lines=0)

time.start<-Sys.time()
k.means.fit <- kmeans(as.data.frame(data), NumeroClusters, algorithm = "Forgy")
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g12<-clusplot(data, k.means.fit$clust, main='Gráfico dos grupos de K-means.Forgy', color=TRUE, shade=FALSE)
TempLevel<-k.means.fit$cluster
source("Adjustlevels.R")
outCluster.k.means.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.k.means.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.k.means.fit.Forgy <- roc(dataAnaliseClass, outCluster.k.means.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.k.means.fit.Forgy ",ROC.outCluster.k.means.fit.Forgy$auc ))
source("Evaluation.R")
stepmodel<-cbind("k.means.fit.Forgy",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.k.means.fit,"outCluster.k.means.fit.Forgy.xlsx")

#clusplot(data, k.means.fit$cluster, main='Gráfico dos grupos de kmeans', color=TRUE, shade=FALSE,labels=2, lines=0)

time.start<-Sys.time()
k.means.fit <- kmeans(as.data.frame(data), NumeroClusters, algorithm = "MacQueen")
time.stop<-Sys.time()
totalTime<-time.stop - time.start
mtxTimes<-rbind(mtxTimes, totalTime)

g13<-clusplot(data, k.means.fit$clust, main='Gráfico dos grupos de K-means.MacQueen', color=TRUE, shade=FALSE)
TempLevel<-k.means.fit$cluster
source("Adjustlevels.R")
outCluster.k.means.fit<-TempLevel
allClassicifation<-cbind(allClassicifation,TempLevel)

msevalue<-mse(as.numeric(dataAnaliseClass), outCluster.k.means.fit)
mtxmse<-rbind(mtxmse,msevalue)

ROC.outCluster.k.means.fit.MacQueen <- roc(dataAnaliseClass, outCluster.k.means.fit, algorithm = 0); # Curva ROC
mxtROC<-rbind(mxtROC,c("ROC.outCluster.k.means.fit.MacQueen ",ROC.outCluster.k.means.fit.MacQueen$auc ));
source("Evaluation.R")
stepmodel<-cbind("k.means.fit.MacQueen",stepEvaluation)
mtxEvaluation<-rbind(mtxEvaluation,stepmodel)
write.xlsx(outCluster.k.means.fit,"outCluster.k.means.fit.MacQueen.xlsx")
write.xlsx(mtxmse,"mtxmse.xlsx")

#clusplot(data, k.means.fit$cluster, main='Gráfico dos grupos de kmeans',color=TRUE, shade=FALSE,labels=2, lines=0)

############################################

print(mxtROC)

write.xlsx(allClassicifation,"allClassicifation.xlsx")

mtxTimeAll<-cbind(mtxTimeAll,mtxTimes)

mtxallmse<-cbind(mtxallmse,mtxmse)






