rm(list=ls())

LoadLibrary<-function(){
  
  library(ClusterR)
  library(smotefamily)
  library(RSNNS) ;
  library(clusterSim,cluster);
  library(MASS);
  library(caret);
  library(kohonen); 
  library(clValid);
  library(mclust);
  library(ggplot2);
  library(beepr);
  library(PMCMR)
  library(pROC)
  library(ROCR)
  library(mirt)
  library(xlsx)
  library(readxl)
  library(EvaluationMeasures)
  library(ltm, mvtnorm)
  library(pcIRT)
  library(eRm,latticeExtra)
  library(nortest)
  library(pspearman)
  library(Kendall)
  library(gridExtra)
  library(Metrics)
  library(plot3D)
  library(corrplot)
  library(Hmisc)
  library(PerformanceAnalytics)
  library(pacman)
  library(rstatix)
  library(DescTools)
  library(irr)
  library(tictoc)
  library(rcompanion)
  library(PMCMRplus)
  library(dunn.test)
  library(FSA)
  library(parallel)
  
  print("------------------");
  print("Libraries OK");
  print("------------------");
}

SlicesStartMin<-function(DatasetSelected){
  dataSplit1<-c();dataSplit2<-c();
  
  rawdata<-DatasetSelected
  
  # data
  # dataAnaliseClass
  
  #-------------------------------------------
  cls<-summary(dataAnaliseClass)
  if(cls[1]<cls[2]){mini<-"0"}
  if(cls[1]>=cls[2]){mini<-"1"}
  
  library(dplyr)
  
  if(mini=="0"){
    mirority<-min(cls);
    dataAnaliseOrder<-arrange(dataAnalise, dataAnalise[,dataAnaliseCol],);
    splitMinority<-dataAnaliseOrder[1:mirority,];
    splitMajority<-dataAnaliseOrder[(mirority+1):(dim(dataAnaliseOrder)[1]),]
  }
  
  if(mini=="1"){
    mirority<-min(cls);
    dataAnaliseOrder<-arrange(dataAnalise, desc(dataAnalise[,dataAnaliseCol]),)
    splitMinority<-dataAnaliseOrder[1:mirority,];
    splitMajority<-dataAnaliseOrder[(mirority+1):(dim(dataAnaliseOrder)[1]),]
  }
  
  szLineMinority<-floor(dim(splitMinority)[1])
  szColMinority<-floor(dim(splitMinority)[2])
  
  szLineMajority<-floor(dim(splitMajority)[1])
  szColMajority<-floor(dim(splitMajority)[2])
  
  return(splitMinority)
}

SlicesStartMaj<-function(DatasetSelected) {
  
  dataSplit1<-c();dataSplit2<-c();
  
  rawdata<-DatasetSelected
  
  # data
  # dataAnaliseClass
  
  #-------------------------------------------
  cls<-summary(dataAnaliseClass)
  if(cls[1]<cls[2]){mini<-"0"}
  if(cls[1]>=cls[2]){mini<-"1"}
  
  library(dplyr)
  
  if(mini=="0"){
    mirority<-min(cls);
    dataAnaliseOrder<-arrange(dataAnalise, dataAnalise[,dataAnaliseCol],);
    splitMinority<-dataAnaliseOrder[1:mirority,];
    splitMajority<-dataAnaliseOrder[(mirority+1):(dim(dataAnaliseOrder)[1]),]
  }
  
  if(mini=="1"){
    mirority<-min(cls);
    dataAnaliseOrder<-arrange(dataAnalise, desc(dataAnalise[,dataAnaliseCol]),)
    splitMinority<-dataAnaliseOrder[1:mirority,];
    splitMajority<-dataAnaliseOrder[(mirority+1):(dim(dataAnaliseOrder)[1]),]
  }
  
  szLineMinority<-floor(dim(splitMinority)[1])
  szColMinority<-floor(dim(splitMinority)[2])
  
  szLineMajority<-floor(dim(splitMajority)[1])
  szColMajority<-floor(dim(splitMajority)[2])
  
  return(splitMajority)
  
}

SlicesJoin<-function(splitMinority,splitMajority){
  
  splitMinority<-splitMinority[1:nrow(splitMinority),]
  splitMajority<-splitMajority[1:nrow(splitMajority),]
  szLineMinority<-floor(dim(splitMinority)[1])
  
  int0<-1; int1<-(floor(szLineMinority/2));
  int2<-(int1+1); int3<-nrow(splitMinority)
  
  dataSplit1<-rbind(splitMinority[int0:int3,])
  dataSplit2temp<-splitMajority[sample(nrow(splitMajority),replace=F),]
  dataSplit2<-dataSplit2temp[1:int3,]
  
  dataSplit1<-dataSplit1[sample(nrow(dataSplit1),replace=F),]
  dataSplit2<-dataSplit2[sample(nrow(dataSplit2),replace=F),]
  
  joinSplits<-rbind(dataSplit1,dataSplit2)
  
  joinSplits<-joinSplits[sample(nrow(joinSplits),replace=F),]
  
  return(joinSplits)
  
  
}

OutEasyData<-function(dataTemp,vetDificultXPL){
  mtxOutPutData<-cbind(dataTemp,vetDificultXPL)
  zsmtxOutPutData<-ncol(mtxOutPutData)
  dataOrder<-arrange(mtxOutPutData,mtxOutPutData[,zsmtxOutPutData],);
  return(dataOrder)
}

SlicesChange<-function(classOuputdata, dataOrder,rowsOff,mini){
  myxInputNew<-c()
  
  if(classOuputdata != mini){
    splitMajorityMix<-splitMajority[sample(nrow(splitMajority),replace=F),]
    myxInputNew<-splitMajorityMix[1:rowsOff,]
  }
  
  if(classOuputdata == mini){
    splitMinorityMix<-splitMinority[sample(nrow(splitMinority),replace=F),]
    myxInputNew<-splitMinorityMix[1:rowsOff,]
  }
  
  
  return(myxInputNew)
  
  
}

amostragem<-function(){
  classtarget<-c()
  vetPos<-sample(1:dataAnaliseRow, 1, replace=T)
  classtarget<-dataAnalise[vetPos,dataAnaliseCol]
  amostra<-dataAnalise[vetPos,]
  return(amostra)
}

AdjustClass<-function(TempLevel){
  empLevel<-as.matrix(TempLevel)
  #TempLevel<-as.matrix.data.frame(TempLevel)
  
  for (i in 1:length(TempLevel)){
    #print(TempLevel[i])
    if(TempLevel[i]=="1"){TempLevel[i]<-"0"}
    if(TempLevel[i]=="2"){TempLevel[i]<-"1"}  
    if(TempLevel[i]=="3"){TempLevel[i]<-"2"}
  }
  
  TempLevel<-as.numeric(TempLevel)
  
  for (i in 1:length(TempLevel)){
    #print(TempLevel[i])
    if(TempLevel[i]=="tested_positive"){TempLevel[i]<-0}
    if(TempLevel[i]=="tested_negative"){TempLevel[i]<-1}  
    
  }
  
  TempLevel<-as.numeric(TempLevel)
  return(TempLevel)
  
}

classifiers<-function(dataSplitx,algorithm){
  
  #dataSplitx<-dataTemp
  data<-scale(dataSplitx[1:ncol(dataSplitx)-1])
  
  szdata<-dim(data)[2]
  vetclasse<-dataSplitx[,ncol(dataSplitx)]
  
  NumeroClusters<-2;
  mtxmse<-c();TempLevel<-c();
  
  if (algorithm==1){
    fanny.fit<-fanny(as.data.frame(data), NumeroClusters, memb.exp = (NumeroClusters),metric = "euclidean",maxit = 500)
    TempLevel<-(fanny.fit$clustering)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==2){
    # Fanny - metric = c("euclidean", "manhattan", "SqEuclidean")
    fanny.fit<-fanny(as.data.frame(data), NumeroClusters, memb.exp = NumeroClusters,metric = "manhattan",maxit = 500)
    TempLevel<-(fanny.fit$clustering)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==3){
    # Fanny - metric = c("euclidean", "manhattan", "SqEuclidean")
    fanny.fit<-fanny(as.data.frame(data), NumeroClusters, memb.exp = NumeroClusters,metric = "SqEuclidean",maxit = 500)
    TempLevel<-(fanny.fit$clustering)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==4){
    #pam - metric = c("euclidean", "manhattan")
    pam.fit<-pam(as.data.frame(data), NumeroClusters, diss = inherits(data, "dist"),metric = "euclidean")
    TempLevel<-(pam.fit$clustering)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==5){
    pam.fit<-pam(as.data.frame(data), NumeroClusters, diss = inherits(data, "dist"),metric = "manhattan")
    TempLevel<-(pam.fit$clustering)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==6){
    #clara - metric = c("euclidean", "manhattan", "jaccard")
    clara.fit<-clara(as.data.frame(data), NumeroClusters, metric ="euclidean")
    TempLevel<-(clara.fit$clustering)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==7){
    clara.fit<-clara(as.data.frame(data), NumeroClusters, metric ="manhattan")
    TempLevel<-(clara.fit$clustering)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==8){
    clara.fit<-clara(as.data.frame(data), NumeroClusters, metric ="jaccard")
    TempLevel<-(clara.fit$clustering)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==9){
    #sota
    sota.fit<-sota(as.matrix(data),NumeroClusters-1)
    TempLevel<-(sota.fit$clust)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==10){
    k.means.fit <- kmeans(as.data.frame(data), NumeroClusters, algorithm = "Hartigan-Wong")
    TempLevel<-(k.means.fit$cluster)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==11){
    k.means.fit <- kmeans(as.data.frame(data), NumeroClusters, algorithm = "Lloyd")
    TempLevel<-(k.means.fit$cluster)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==12){
    #clusplot(data, k.means.fit$cluster, main='Gráfico dos grupos de kmeans', color=TRUE, shade=FALSE,labels=2, lines=0)
    k.means.fit <- kmeans(as.data.frame(data), NumeroClusters, algorithm = "Forgy")
    TempLevel<-(k.means.fit$cluster)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==13){
    k.means.fit <- kmeans(as.data.frame(data), NumeroClusters, algorithm = "MacQueen")
    TempLevel<-(k.means.fit$cluster)
    outCluster.fanny.fit<-AdjustClass(TempLevel)
  }
  
  if (algorithm==14){
    #Diana
    g14<-diana(data,metric = "euclidean", stand = TRUE) # default: Euclidean
    TempLevel<-cutree(as.hclust(g14), k = 2)
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==15){
    # KMeans_arma
    library(ClusterR)
    g15<-KMeans_arma(data, clusters = 2, n_iter = 10, seed_mode = "random_subset", verbose = T, CENTROIDS = NULL)
    TempLevel<-predict_KMeans(data , g15)
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==16){
    # KMeans_rcpp
    g16<-KMeans_rcpp(data, clusters = 2)#, num_init = 5, max_iters = 10, initializer = 'optimal_init', threads = 1, verbose = F)
    TempLevel<-g16$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==17){
    # MiniBatchKmeans
    g17<-MiniBatchKmeans(data, clusters = 2, batch_size = 10, num_init = 5, max_iters = 100, init_fraction = 0.2, initializer = 'kmeans++', early_stop_iter = 10, verbose = F)
    TempLevel<-predict_MBatchKMeans(data, g17$centroids)
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==18){
    g18<-MiniBatchKmeans(data, clusters = 2, batch_size = 10, num_init = 5, max_iters = 100, init_fraction = 0.2, initializer = 'random', early_stop_iter = 10, verbose = F)
    TempLevel<-predict_MBatchKMeans(data, g18$centroids)
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==19){
    g19<-MiniBatchKmeans(data, clusters = 2, batch_size = 10, num_init = 5, max_iters = 100, init_fraction = 0.2, initializer = 'optimal_init', early_stop_iter = 10, verbose = F)
    TempLevel<-predict_MBatchKMeans(data, g19$centroids)
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==20){
    g20<-MiniBatchKmeans(data, clusters = 2, batch_size = 10, num_init = 5, max_iters = 100, init_fraction = 0.2, initializer = 'quantile_init', early_stop_iter = 10, verbose = F)
    TempLevel<-predict_MBatchKMeans(data, g20$centroids)
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==21){
    # Fuzzy C-means
    library("e1071")
    g21<- cmeans(data, centers = 2,dist='euclidean')
    TempLevel<-g21$cluster
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==22){
    # Fuzzy C-means
    library("e1071")
    g22<- cmeans(data, centers = 2,dist='manhattan')
    TempLevel<-g22$cluster
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==23){
    # Cluster_Medoids
    g23<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='euclidean')
    TempLevel<-g23$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==24){
    # Cluster_Medoids
    g24<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='manhattan')
    TempLevel<-g24$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==25){
    # Cluster_Medoids
    g25<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='chebyshev')
    TempLevel<-g25$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==26){
    # Cluster_Medoids
    g26<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='canberra')
    TempLevel<-g26$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==27){
    g27<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='braycurtis')
    TempLevel<-g27$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==28){
    g28<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='pearson_correlation')
    TempLevel<-g28$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==29){
    g29<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='simple_matching_coefficient')
    TempLevel<-g29$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==30){
    g30<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='minkowski')
    TempLevel<-g30$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }

  if (algorithm==31){
    g31<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='hamming')
    TempLevel<-g31$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==32){
    g32<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='jaccard_coefficient')
    TempLevel<-g32$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==33){
    g33<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='Rao_coefficient')
    TempLevel<-g33$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==34){
    g34<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='mahalanobis')
    TempLevel<-g34$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
  
  if (algorithm==35){
    g35<-Cluster_Medoids(data, clusters = 2, swap_phase = TRUE, verbose = F,distance_metric='cosine')
    TempLevel<-g35$clusters
    #source("Adjustlevels.R")
    outCluster.fanny.fit<-AdjustClass(TempLevel)
    
  }
 
  return(outCluster.fanny.fit)
  
}

CalcMSE<-function(vetTarget, vetPredict,metric){
  
  # Calculate metrics
  if(metric==1){msevalue<-mse(vetTarget, vetPredict)}
  if(metric==2){msevalue<-EvaluationMeasures.Accuracy(vetTarget, vetPredict)}
  if(metric==3){msevalue<-EvaluationMeasures.Sensitivity(vetTarget, vetPredict)}
  if(metric==4){msevalue<-EvaluationMeasures.Specificity(vetTarget, vetPredict)}
  if(metric==5){msevalue<-recall(vetTarget, vetPredict)}
  if(metric==6){msevalue<-roc(as.vector(vetTarget), as.vector(vetPredict))}
  return(msevalue)
}

CalcMASE<-function(vetTarget2, vetPredict){
  #vetTargetMAPE<-AdjustClass(vetTarget2)
  #vetTargetMAPE2<-as.integer(vetTargetMAPE)
 mapevalue<-mase(vetTarget2, vetPredict)
  return(mapevalue)
}

funcTime<-function(time.start,data){
  
  szdataLine<-dim(data)[1]
  
  time.stop<-Sys.time();
  totalTime<-(time.stop - time.start)/szdataLine
  return(totalTime)
}

compareAll<-function(data001){
  
  countRight<-0;vetRight<-0;
  szdata001L<-dim(data001)[1]
  szdata001C<-dim(data001)[2]
  
  for(stepdata001L in 1:szdata001L){
    countRight<-0;
    for(stepdata001C in 2:szdata001C){
      classCurrent<-data001[stepdata001L,1]
      if((classCurrent)==data001[stepdata001L,stepdata001C]){countRight<-countRight+1}
    }
    vetRight<-rbind(vetRight,countRight)
    
    
  }
  vetRight<-rbind(vetRight,"end")
  return(vetRight)
  
  
}

funcEstatistic<-function(mtxTimes){
  mx<-max(mtxTimes)
  mi<-min(mtxTimes)
  md<-mean(mtxTimes)
  dp<-sd(mtxTimes)
  cv<-(dp/md)*100
  vettime1<-cbind(mx,mi,dp,md,cv)
  return(vettime1)
}

calcDificult1PL<-function(dataDif){
  
  vetdata1<-as.data.frame(dataTemp[,ncol(dataTemp)])
  sizevet<-dim(vetdata1)[1]
  dataDif<-vetdata1
  dataset1 <- simdata(dataDif, vetPredict, sizevet, itemtype = 'dich') # change numeric to categorical
  
  mod1 <- mirt(dataset1, 1, method = 'EM',itemtype='Rasch',technical = list(NCYCLES = sizevet), GenRandomPars = TRUE)
  coefModel<-coef(mod1,simplify=TRUE, IRTpars=TRUE)
  coefModel1<-as.data.frame(coefModel[1])
  dificultModel1<-coefModel1[,2]
  
  TethaResult(mod1,SelectDifficult,algorithm,stepMoment)
  
  return(dificultModel1)
  
}

calcDificult2PL<-function(dataDif){
  
  vetdata1<-as.data.frame(dataTemp[,ncol(dataTemp)])
  sizevet<-dim(vetdata1)[1]
  dataDif<-vetdata1
  dataset1 <- simdata(dataDif, vetPredict, sizevet, itemtype = 'dich') # change numeric to categorical
  
  mod2 <- mirt(dataset1, 1, method = 'EM',itemtype='2PL',technical = list(NCYCLES = sizevet), GenRandomPars = TRUE)
  coefModel<-coef(mod2,simplify=TRUE, IRTpars=TRUE)
  coefModel2<-as.data.frame(coefModel[1])
  dificultModel2<-coefModel2[,2]
  
  TethaResult(mod2,SelectDifficult,algorithm,stepMoment)
  
  return(dificultModel2)
  
}

calcDificult3PL<-function(dataDif){
  
  vetdata1<-as.data.frame(dataTemp[,ncol(dataTemp)])
  sizevet<-dim(vetdata1)[1]
  dataDif<-vetdata1
  dataset1 <- simdata(dataDif, vetPredict, sizevet, itemtype = 'dich') # change numeric to categorical
  
  mod3 <- mirt(dataset1, 1, method = 'EM',itemtype='3PL',technical = list(NCYCLES = sizevet))#, GenRandomPars = TRUE)
  coefModel<-coef(mod3,simplify=TRUE, IRTpars=TRUE)
  coefModel3<-as.data.frame(coefModel[1])
  dificultModel3<-coefModel3[,2]
  BCdificultModel3<-coefModel3[,2:3]
  
  TethaResult(mod3,SelectDifficult,algorithm,stepMoment)
  
  return(coefModel3)
  
}

funcTime<-function(time.start,data){
  
  szdataLine<-dim(data)[1]
  
  time.stop<-Sys.time();
  totalTime<-(time.stop - time.start)/szdataLine
  return(totalTime)
}

plotG1<-function(vetDifficult,metric){
  
  legend_title <- "PL´s Models"
  legendsModels<-c("1PL", "2PL", "3PL")
  gg<-(vetDifficult)
  gg<-as.data.frame(t(gg))
  turns<-dim(gg)[1]
  mtrik<-c()
  if(metric==1){
    df<-ggplot(gg,aes(x = 1:turns,y = MSEs),group=legendsModels)+
      geom_line(aes(y = gg[,1]), color = "red")+
      geom_line(aes(y = gg[,2]), color = "green")+
      geom_line(aes(y = gg[,3]), color = "blue")+ scale_x_continuous(limits = c(2, turns))#+ scale_y_continuous(limits = c(0, 0.15))
    
    df1<-df + theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )
  }
  if(metric==2){df<-ggplot(gg,aes(x = 1:turns,y = Accuracy),group=legendsModels)+
    geom_line(aes(y = gg[,1]), color = "red")+
    geom_line(aes(y = gg[,2]), color = "green")+
    geom_line(aes(y = gg[,3]), color = "blue")+ scale_x_continuous(limits = c(2, turns))#+ scale_y_continuous(limits = c(0, 0.15))
  
  df1<-df + theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
  }
  if(metric==3){df<-ggplot(gg,aes(x = 1:turns,y = Sensitivity),group=legendsModels)+
    geom_line(aes(y = gg[,1]), color = "red")+
    geom_line(aes(y = gg[,2]), color = "green")+
    geom_line(aes(y = gg[,3]), color = "blue")+ scale_x_continuous(limits = c(2, turns))#+ scale_y_continuous(limits = c(0, 0.15))
  
  df1<-df + theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
  
    
    
    
    
    mtrik<-""}
  if(metric==4){
  df<-ggplot(gg,aes(x = 1:turns,y = Specificity),group=legendsModels)+
    geom_line(aes(y = gg[,1]), color = "red")+
    geom_line(aes(y = gg[,2]), color = "green")+
    geom_line(aes(y = gg[,3]), color = "blue")+ scale_x_continuous(limits = c(2, turns))#+ scale_y_continuous(limits = c(0, 0.15))
  
  df1<-df + theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
  }
  if(metric==5){
  df<-ggplot(gg,aes(x = 1:turns,y = RECALL),group=legendsModels)+
    geom_line(aes(y = gg[,1]), color = "red")+
    geom_line(aes(y = gg[,2]), color = "green")+
    geom_line(aes(y = gg[,3]), color = "blue")+ scale_x_continuous(limits = c(2, turns))#+ scale_y_continuous(limits = c(0, 0.15))
  
  df1<-df + theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
  }
  
  return(print(df1))
  
  #scale_fill_discrete(name = "PLs", labels = c("1PL", "2PL", "3PL"))
}

funcGraphic<-function(vetDifficult){
  nameGrafico<-paste0('Dataset',DatasetCorrente,"Alg",algorithm,".tiff");
  tituloGrafico<-paste0('Dataset',DatasetCorrente,"Alg",algorithm,".tiff");
  
  setwd(path); getwd();
  
  tiff(nameGrafico, units="in", width=5, height=5, res=300)
  setwd(paste0(path,"/Splits/Graficos")); getwd();
  plotG1(vetDifficult,metric)
  dev.off()
  
  setwd("C:/TRICluster-out/"); getwd();
}

reduceData<-function(DatasetSelected){
  
  newExemplesByClass<-24 # 24 exemples by dataset of original class balanced
  
  #lmitClass<-dim(DatasetSelected)[1]/2
  lmitClass<-newExemplesByClass
  
  fistSplitTemp<-SlicesStartMin(DatasetSelected)
  fistSplit<-fistSplitTemp[1:lmitClass,]
  secondSplitTemp<-SlicesStartMaj(DatasetSelected)
  secondSplit<-secondSplitTemp[1:lmitClass,]
  
  dim(fistSplit)
  dim(secondSplit)
  
  DatasetSelected<-rbind(fistSplit,secondSplit)
  
  DatasetSelected<-DatasetSelected[sample(nrow(DatasetSelected),replace=F),]
  
  return(DatasetSelected)
  
}

balancedDataX<-function(DatasetSelected,DatasetCorrente){
  
  if(DatasetCorrente=="1"){multiplo<-5}
  if(DatasetCorrente=="3"){multiplo<-0.5}
  if(DatasetCorrente=="4"){multiplo<-2}
  if(DatasetCorrente=="5"){multiplo<-5}
  if(DatasetCorrente=="13"){multiplo<-4}
  if(DatasetCorrente=="14"){multiplo<-2}
  if(DatasetCorrente=="16"){multiplo<-0.5}
  if(DatasetCorrente=="17"){multiplo<-0.5}
  if(DatasetCorrente=="19"){multiplo<-0.5}
  if(DatasetCorrente=="20"){multiplo<-2}
  if(DatasetCorrente=="21"){multiplo<-0.5}
  if(DatasetCorrente=="24"){multiplo<-0.5}
  
  dataX<-DatasetSelected[,1:(ncol(DatasetSelected)-1)]
  dataClassX<-as.factor(DatasetSelected[,(ncol(DatasetSelected))])
  Balanceded<-SMOTE(dataX, dataClassX, K=2, dup_size = multiplo)
  cls1<-summary(dataAnaliseClass);cls1
  cls2<-summary(as.factor(Balanceded$syn_data[,ncol(Balanceded$syn_data)]));cls2
  
  dim(DatasetSelected)
  dim(Balanceded$data)
  
  oversampled<-Balanceded$data
  dataY<-oversampled[,1:(ncol(oversampled)-1)]
  #datajoin<-rbind(dataX[1:nrow(dataX)-1,],dataY[1:nrow(dataY)-1,])
  datajoin<-rbind(dataX,dataY)
  
  dataClassY<-as.factor(oversampled[,(ncol(oversampled))])
  dataClassJoin<-c(dataClassX,dataClassY)
  
  mode(dataClassY)
  
  mode(dataClassX)
  
  newDataBalanced<-cbind(datajoin,dataClassJoin)
  
  return(newDataBalanced)
  
}

PercentData<-function(DatasetSelected,Percent){
  
  # By percentage
  
  szsplitMinority<-dim(splitMinority)[1]
  szsplitMajority<-dim(splitMajority)[1]
  
  newszsplitMinority<-floor(szsplitMinority*(Percent/100))
  newszsplitMajority<-floor(szsplitMinority*(Percent/100))
  
  #fistSplit<-splitMinority[1:newszsplitMinority,1:(ncol(splitMinority)-1)]
  #secondSplit<-splitMajority[1:newszsplitMinority,1:(ncol(splitMajority)-1)]
  
  fistSplit<-splitMinority[1:newszsplitMinority,1:(ncol(splitMajority))]
  secondSplit<-splitMajority[1:newszsplitMinority,1:(ncol(splitMajority))]
  
  
  dim(fistSplit)
  dim(secondSplit)
  
  DatasetSelected<-rbind(fistSplit,secondSplit)
  
  DatasetSelected<-DatasetSelected[sample(nrow(DatasetSelected),replace=F),]
  
  return(DatasetSelected)
  
}

SaveParameters<-function(dataTemp,meandificultBest,vetDificult1PL){
  
  setwd(path); getwd();
  
  namevetDificult1PL<-paste0(path,"/Splits/Mensure/Best/","vetDificult1PL",'Dataset',DatasetCorrente,"algorithm",algorithm,SelectDifficult,"PL",".csv")
  write.csv(vetDificult1PL,namevetDificult1PL)
  
  namemeandificultBest<-paste0(path,"/Splits/Mensure/Best/","meandificultBest",'Dataset',DatasetCorrente,"algorithm",algorithm,SelectDifficult,"PL",".xlsx")
  vetDificultX<-vetDificult1PL
  vetDificult1PLOrd<-sort(as.matrix(vetDificultX));
  
  vetOut<-c("min",min(vetDificult1PLOrd),"media",meandificultBest,"maximo",max(vetDificult1PLOrd),
            "Desvio-padrão",sd(vetDificult1PLOrd),"Coeficiente de variacao",(sd(vetDificult1PLOrd)/meandificultBest)*100)
  
  write.xlsx(vetOut,namemeandificultBest)
  
  nameBestSet<-paste0(path,"/Splits/Mensure/Best/","MtxBestSet",'Dataset',DatasetCorrente,"algorithm",algorithm,SelectDifficult,"PL",".csv")
  write.csv(dataTemp,nameBestSet)
  
  setwd("C:/TRICluster-out/"); getwd();
}

metricsByTRI<-function(vetTarget, vetPredict){
  
  vetTarget<-AdjustClass(vetTarget)
  
  p1<-as.numeric(vetPredict) 
  p2<-as.numeric(vetTarget)  
  
  vetMeansure<-c()
  prec<- EvaluationMeasures.Precision(p1,p2)
  acc <- EvaluationMeasures.Accuracy(p1,p2)
  sen <- EvaluationMeasures.Sensitivity(p1,p2)
  spe <- EvaluationMeasures.Specificity(p1,p2)
  rec <- recall(p1,p2)
  roc1 <- roc(p1,p2)
  roc2 <-as.list.data.frame(roc1$auc)
  roc<-roc2[1]
  #if((metric==5)&&(roc==0){}
  
  titlevetMeansure<-c("prec","acc","sen","spe","rec")
  vetMeansure<-c(prec,acc,sen,spe,rec)
  
  return(vetMeansure)
  
}

saveVetDown<-function(vetDown,SelectDifficult,vetDifficult){
  
  nameVetDown<-paste0(path,'/Splits/Mensure/PointsToPlot/','vetDownDataset',DatasetCorrente,"Alg",algorithm,
                      'SelectDifficult',SelectDifficult,".csv");  
  write.csv(vetDown,nameVetDown)
  
}

savevetDifficult<-function(vetDown,SelectDifficult,vetDifficult,stepMoment,vetMSEAll,vetMASEAll,vetMetricsStepAll,mtxPredict,mtxvetDificult1PL,vetDificult1PL){
  
  setwd(path); getwd();
  
  
  namevetDifficult<-paste0(path,'/Splits/vetDifficult/','vetDifficult',DatasetCorrente,"D",DatasetCorrente,"Alg",algorithm,
                           'All3Difficult',stepMoment,".csv");  
  write.csv(vetDifficult,namevetDifficult)
  
  
  namevetMSEAll<-paste0(path,'/Splits/vetMSEAll/','vetMSEAll',"D",DatasetCorrente,"Alg",algorithm,
                        'Difficult',SelectDifficult,"_",stepMoment,".csv");  
  write.csv(vetMSEAll,namevetMSEAll)
  
  
  namevetMASEAll<-paste0(path,'/Splits/vetMASEAll/','vetMASEAll',"D",DatasetCorrente,"Alg",algorithm,
                        'Difficult',SelectDifficult,"_",stepMoment,".csv");  
  write.csv(vetMASEAll,namevetMASEAll)
  
  
  namevetMetricsStepAll<-paste0(path,'/Splits/vetMetricsStepAll/','vetMetricsStepAll',"D",DatasetCorrente,"Alg",algorithm,
                                'Difficult',SelectDifficult,"_",stepMoment,".csv");  
  write.csv(vetMetricsStepAll,namevetMetricsStepAll)
  
  
  namemtxPredict<-paste0(path,'/Splits/Classifications/','Classifications',"D",DatasetCorrente,"Alg",algorithm,
                                'Difficult',SelectDifficult,"_",stepMoment,".csv");  
  write.csv(mtxPredict,namemtxPredict)
  
  
  namemtxvetDificult1PL<-paste0(path,'/Splits/mtxvetDificult1PL/','mtxvetDificult1PL',"D",DatasetCorrente,"Alg",algorithm,
                         'Difficult',SelectDifficult,"_",stepMoment,".csv");  
  write.csv(mtxvetDificult1PL,namemtxvetDificult1PL)
 
  
  namevetDificult1PL<-paste0(path,'/Splits/vetDificultXPL/','mtxvetDificultPL',"D",DatasetCorrente,"Alg",algorithm,
                                'Difficult',SelectDifficult,"_",stepMoment,".csv");  
  write.csv(vetDificult1PL,namevetDificult1PL)
  
  setwd("C:/TRICluster-out/"); getwd();
  
}

TethaResult<-function(modX,SelectDifficult,algorithm,stepMoment){
  
  expected<-modX@Model[7]
  nameexpected<-paste0(path,'/Splits/Thetas/','vetThetas',"D",DatasetCorrente,"Alg",algorithm,
                       'SelectDifficult',SelectDifficult,"Turn",stepMoment,".csv");  
  write.csv(expected,nameexpected)
  
  # Return vector with tethas in each search
  #return(expected)
  
}

balancedDataOutput<-function(dataTemp,SelectDifficult,flag1){
  
  dataOrderNew1<-OutEasyData(dataTemp,SelectDifficult)
  classOuputdata<-dataOrderNew1[1,ncol(dataOrderNew1)-1]
  myxInputNew<-SlicesChange(classOuputdata, dataOrderNew1,rowsOff,mini)
  myxInputNew1<-as.list(myxInputNew)
  szdataOrderNew1<-dim(dataOrderNew1)[1]  
  
  while (flag1!=1){
    for (s1 in 1:szdataOrderNew1){
      currentData<-dataOrderNew1[s1,1:ncol(dataOrderNew1)-1]
      currentData<-as.list(currentData)
      qdeIntersection<-length(intersect(myxInputNew1,currentData))
      
      #if(myxInputNew1 != currentData){flag1<-"1";break}else{myxInputNew<-SlicesChange(classOuputdata, dataOrderNew1,rowsOff,mini)}
      if(qdeIntersection>0){flag1<-"1";break}else{myxInputNew<-SlicesChange(classOuputdata, dataOrderNew1,rowsOff,mini)}
      
      
      
    }
  }
  return(myxInputNew)
}

ET_test<-function(){
  
  library(stats)
  
  setwd("I:/backup_Projeto/24/Splits/vetDifficult"); 
  getwd();
  
  Y<-1;W<-1;X<-1
  
  nameMTXDifficults<-paste0("vetDifficult",Y,"D",W,"Alg",X,"All3Difficult1000.csv")  
  MTXsetSelected_raw<-read.csv(file = nameMTXDifficults,header = TRUE, sep = ",")
  MTXsetSelected<-as.data.frame(MTXsetSelected_raw[,2:ncol(MTXsetSelected_raw)])
  
  dim(MTXsetSelected)
  
  zz<-pairwise.wilcox.test(MTXsetSelected_raw[1,],MTXsetSelected_raw[3,],p.adjust.method = "bonferroni")
  zz<-pairwise.wilcox.test(MTXsetSelected[1,],MTXsetSelected[3,],p.adjust.method = "bonferroni")
  
  
  KK_output<-kruskal.test(MTXsetSelected[1:3,])
  
  
  
  
  
}

#  Main program ------------------------------

set.seed(1)
LoadLibrary()

DatasetCorrenteVet<-c(1,3,4,5,13,14,17,19,20,21,24);#(1,3,4,5,13,14,16,17,19,20,21,24);
algorithmList<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35)#c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)
metric<-5#c(1,2,3,4,5)#(mse,Accuracy,Sensitivity,Specificity,recall)
turns<-1000 #(1,n) # random rounds 
rowsOff<-1#(1,nrow) # New samples in exchange
SelectDifficult<-c(1,2,3) # Molde PL
szDatasetCorrente<-(length(DatasetCorrenteVet))
szalgorithm<-length(algorithmList)
szSelectDifficult<-length(SelectDifficult)
Percent<-24 # Percent of minirity class: 50% and 75% of classes
switchType<-3 # if switchType == 1: size of class is 24 or switchType==2: size of class is iqual ro percent

# Mode selection to archive output data
if(switchType=="1"){path<-"I:/backup_Projeto/24"}
if((switchType=="2") && (Percent<-50)){path<-'I:/backup_Projeto/50'}
if((switchType=="2")&&(Percent<-75)){path<-'I:/backup_Projeto/75'}
if(switchType=="3"){path<-'I:/backup_Projeto/SMOTE'}

setwd(path); getwd();

for(stepDatasetCorrente in 4:9){ # all Datasets
  DatasetCorrente<-DatasetCorrenteVet[stepDatasetCorrente]
  
  for(stepalgorithm in 1:34){ # all algorithm
    algorithm<-algorithmList[stepalgorithm]
    vetDifficult<-c()
    
    for(stepDifficult in 1:3){ # all Difficults
      SelectDifficult<-stepDifficult
      mtxMetrics<-c();vetMSEAll<-c();vetMASEAll<-c();vetMetricsStepAll<-c();mtxPredict<-c();mtxvetDificult1PL<-c();vetMetrics2<-c()
      mtxMetrics2<-c()
     # }}}          
      
      setwd("c:/TRICluster-out"); getwd();
      source("LoadDatasets.R")
      cls<-summary(dataAnaliseClass)
      mini<-"0"
      mirority<-mini
      numeroClasses<-length(levels(dataAnaliseClass))
      bestSlice<-c();vetMSE<-c();vetBest<-c();meandificultBest<-(-12);vetDown<-c()
      
      splitMinority<-SlicesStartMin(DatasetSelected)
      splitMajority<-SlicesStartMaj(DatasetSelected)
      dataTemp<-SlicesJoin(splitMinority,splitMajority)
      mtxReference<-c()
      
      # Redulce exemples in all dataset to 25 exemples balanced - set by smalless class in datasets
      #if(DatasetCorrente=="16"){}
      
      if(switchType=="1"){dataTemp<-reduceData(DatasetSelected);
      if(metric==1){dataBest<-3.5};if(metric==2){dataBest<-0};if(metric==3){dataBest<-0};if(metric==4){dataBest<-0};if(metric==5){dataBest<-0}} #size class = 24
      if(switchType=="2"){dataTemp<-PercentData(DatasetSelected,Percent);
      if(metric==1){dataBest<-3.5};if(metric==2){dataBest<-0};if(metric==3){dataBest<-0};if(metric==4){dataBest<-0};if(metric==5){dataBest<-0}} #size class = 80% of minority class
      if(switchType=="3"){dataTemp<-balancedDataX(DatasetSelected,DatasetCorrente);
      if(metric==1){dataBest<-3.0};if(metric==2){dataBest<-0};if(metric==3){dataBest<-0};if(metric==4){dataBest<-0};if(metric==5){dataBest<-0}}#size class = minority class original
      # ************
    
      for (stepMoment in 1:turns){
        
        mtxReference<-dataTemp
        vetPredict<-classifiers(dataTemp,algorithm)
        
        # CPU core
        
        if(SelectDifficult==1){vetDificult1PL<-pvec(1:30, function(calcDificult1PL) dataTemp,mc.cores = 1);meanDificult1PL<-median(as.matrix(vetDificult1PL[1:nrow(vetDificult1PL),1:ncol(vetDificult1PL)-1]))}
        if(SelectDifficult==2){vetDificult2PL<-pvec(1:30, function(calcDificult2PL) dataTemp,mc.cores = 1);meanDificult1PL<-median(as.matrix(vetDificult2PL[1:nrow(vetDificult2PL),1:ncol(vetDificult2PL)-1]))}
        if(SelectDifficult==3){vetDificult3PL<-pvec(1:30, function(calcDificult3PL) dataTemp,mc.cores = 1);meanDificult1PL<-median(as.matrix(vetDificult3PL[1:nrow(vetDificult3PL),1:ncol(vetDificult3PL)-1]))}
        
        #meanDificult1PL<-mean(as.matrix(vetDificult1PL[1:nrow(vetDificult1PL),1:ncol(vetDificult1PL)-1]))
        vetTarget<-dataTemp[,ncol(dataTemp)]
        vetTarget2<-as.numeric(vetTarget)
        
        if(switchType=="3"){vetTarget2<-AdjustClass(vetTarget2)}
        
        tempMSE<-CalcMSE(vetTarget2, vetPredict,metric)
        tempMASE<-CalcMASE(vetTarget2, vetPredict)
        
        vetMSEAll<-cbind(vetMSEAll,tempMSE)
        vetMASEAll<-cbind(vetMASEAll,tempMASE)
        
        vetMs<-c()
        #update dataBest and update meandificultBest
        if((metric==1)&&(tempMSE <= dataBest)&&(meanDificult1PL>=meandificultBest)){
          dataBest<-tempMSE;
          meandificultBest<-meanDificult1PL;
          bestSlice<-dataTemp;
          vetDown<-cbind(vetDown,stepMoment); #check-out
          vetMetrics<-c();
          vetMetrics<-metricsByTRI(vetTarget, vetPredict)
          mtxMetrics<-cbind(mtxMetrics,vetMetrics)
          m1<-vetMetrics[1]
          m2<-vetMetrics[2]
          m3<-vetMetrics[3]
          m4<-vetMetrics[4]
          m5<-vetMetrics[5]
          vetMs<-c(m1,m2,m3,m4,m5)
          mtxMetrics2<-cbind(mtxMetrics2,vetMs)  
        }
        
        if((metric==2)&&(tempMSE >= dataBest)&&(meanDificult1PL>=meandificultBest)){
          dataBest<-tempMSE;
          meandificultBest<-meanDificult1PL;
          bestSlice<-dataTemp;
          vetDown<-cbind(vetDown,stepMoment); #check-out
          vetMetrics<-c();
          vetMetrics<-metricsByTRI(vetTarget, vetPredict)
          mtxMetrics<-cbind(mtxMetrics,vetMetrics)
          m1<-vetMetrics[1]
          m2<-vetMetrics[2]
          m3<-vetMetrics[3]
          m4<-vetMetrics[4]
          m5<-vetMetrics[5]
          vetMs<-c(m1,m2,m3,m4,m5)
          mtxMetrics2<-cbind(mtxMetrics2,vetMs)  
        }
        
        if((metric==3)&&(tempMSE >= dataBest)&&(meanDificult1PL>=meandificultBest)){
          dataBest<-tempMSE;
          meandificultBest<-meanDificult1PL;
          bestSlice<-dataTemp;
          vetDown<-cbind(vetDown,stepMoment); #check-out
          vetMetrics<-c();
          vetMetrics<-metricsByTRI(vetTarget, vetPredict)
          mtxMetrics<-cbind(mtxMetrics,vetMetrics)
          m1<-vetMetrics[1]
          m2<-vetMetrics[2]
          m3<-vetMetrics[3]
          m4<-vetMetrics[4]
          m5<-vetMetrics[5]
          vetMs<-c(m1,m2,m3,m4,m5)
          mtxMetrics2<-cbind(mtxMetrics2,vetMs)  
        }
        
        if((metric==4)&&(tempMSE >= dataBest)&&(meanDificult1PL>=meandificultBest)){
          dataBest<-tempMSE;
          meandificultBest<-meanDificult1PL;
          bestSlice<-dataTemp;
          vetDown<-cbind(vetDown,stepMoment); #check-out
          vetMetrics<-c();
          vetMetrics<-metricsByTRI(vetTarget, vetPredict)
          mtxMetrics<-cbind(mtxMetrics,vetMetrics)
          m1<-vetMetrics[1]
          m2<-vetMetrics[2]
          m3<-vetMetrics[3]
          m4<-vetMetrics[4]
          m5<-vetMetrics[5]
          vetMs<-c(m1,m2,m3,m4,m5)
          mtxMetrics2<-cbind(mtxMetrics2,vetMs)  
        }
        
        if((metric==5)&&(tempMSE >= dataBest)&&(meanDificult1PL>=meandificultBest)){
          dataBest<-tempMSE;
          meandificultBest<-meanDificult1PL;
          bestSlice<-dataTemp;
          vetDown<-cbind(vetDown,stepMoment); #check-out
          vetMetrics<-c();
          vetMetrics<-metricsByTRI(vetTarget, vetPredict)
          mtxMetrics<-cbind(mtxMetrics,vetMetrics)
          m1<-vetMetrics[1]
          m2<-vetMetrics[2]
          m3<-vetMetrics[3]
          m4<-vetMetrics[4]
          m5<-vetMetrics[5]
        
          vetMs<-c(m1,m2,m3,m4,m5)
          mtxMetrics2<-cbind(mtxMetrics2,vetMs)  
        }
       
        vetMetricsStep<-c()
        vetMetricsStep<-metricsByTRI(vetTarget, vetPredict)
        vetMetricsStepTranspost<-as.vector(t(vetMetricsStep))
        vetMetricsStepAll<-cbind(vetMetricsStepAll,vetMetricsStepTranspost)
        
        mtxPredict<-rbind(mtxPredict,vetPredict)
        mtxvetDificult1PL<-rbind(mtxvetDificult1PL,vetDificult1PL)
        
        vetBest<-cbind(vetBest,dataBest);  
        
        # Tabu mode
        flag1<-0
        dataOrderNew<-OutEasyData(dataTemp,SelectDifficult)
        classOuputdata<-dataOrderNew[1,ncol(dataOrderNew)-1]
        valNew<-balancedDataOutput(dataTemp,SelectDifficult,flag1)
        dataOrderNew[1:rowsOff,]<-valNew
        dataTemp<-dataOrderNew[,1:ncol(dataOrderNew)-1]
        
        
        setwd(path); getwd();
        
        #### To create historical dataset ##############
        
       #namevetDificult1YPL<-paste0(path,'/Splits/vetDificultYPL/','mtxvetDificultPL',"D",DatasetCorrente,"Alg",
       #                           algorithm,'Difficult',SelectDifficult,"_",stepMoment,".csv");  
       #write.csv(vetDificult1PL,namevetDificult1YPL)
        
       # namemtxPredictY<-paste0(path,'/Splits/ClassificationsY/','Classifications',"D",DatasetCorrente,"Alg",algorithm,
       #                       'Difficult',SelectDifficult,"_",stepMoment,".csv");  
       #write.csv(mtxPredict,namemtxPredictY)
        
        ############################################
        
        setwd("c:/TRICluster-out"); getwd();
        
        
        print("--------------------------------------")
        print(c("DatasetCorrente",DatasetCorrente,"algorithm",algorithm,"Moment",stepMoment,"tempMSE",tempMSE,"dataBest",
                dataBest,"tempMASE",tempMASE,"mean Dificult 1PL",meanDificult1PL,"meandificultBest",meandificultBest,
                "stepDifficult:",stepDifficult,"Check-out",vetDown))
        print("--------------------------------------")
        
        
        
      }
      print("#########################################")
      print("#########################################")
      
      setwd("c:/TRICluster-out"); getwd();
      
      # Storage in vetor the three models by classificator
      vetDifficult<-rbind(vetDifficult,vetBest)
      
      setwd(path); getwd();
      
      namemtxMetrics<-paste0(path,"/Splits/Metrics/","mtxMetrics",'Dataset',DatasetCorrente,"algorithm",algorithm,SelectDifficult,"PL",".csv")
      write.csv(mtxMetrics,namemtxMetrics)
      
      namemtxMetrics2<-paste0(path,"/Splits/Metricsall/","mtxMetricsAll",'Dataset',DatasetCorrente,"algorithm",algorithm,SelectDifficult,"PL",".csv")
      write.csv(mtxMetrics2,namemtxMetrics2)
      
      namebestSlice<-paste0(path,'/Splits/bestSlice/','bestSlice',"D",DatasetCorrente,"Alg",algorithm,
                            'Difficult',SelectDifficult,"_",stepMoment,".csv");  
      write.csv(bestSlice,namebestSlice)
      
      setwd("c:/TRICluster-out"); getwd();
      
      savevetDifficult(vetDown,SelectDifficult,vetDifficult,stepMoment,vetMSEAll,vetMASEAll,vetMetricsStepAll,
                       mtxPredict,mtxvetDificult1PL,vetDificult1PL)
      
      
    }
    
    setwd("c:/TRICluster-out"); getwd();
    
    print(c("#@@@@@@@@@@@@ next model",algorithm))
    
    print('xxxxxx')
    funcGraphic(vetDifficult)
    SaveParameters(dataTemp,meandificultBest,vetDificult1PL)
    saveVetDown(vetDown,SelectDifficult,vetDifficult)
    
  }
  
  plotG1(vetDifficult,metric)
}

classbestSlice<-as.factor(bestSlice[,ncol(bestSlice)])





