# Instasll packages

.lib<- c("ltm","devtools", "ggplot2","stats", "ggrepel", "wordcloud", "Hmisc", "ggfortify","grid","mirt", "dplyr", "reshape2") #"mirt"
.lib<- c("RSNNS","clusterSim","cluster","MASS","caret","kohonen","clValid","mclust","beepr", "pROC","tictoc",
         "PMCMR","pROC","ROCR","xlsx", "EvaluationMeasures","pcIRT","eRm","latticeExtra","corrplot","tm","SnowballC",
         "RColorBrewer","irr","nortest","pspearman","Kendall","gridExtra","Metrics","plot3D","readxl","pacman","rstatix","irr",
         "DescTools","rcompanion","PMCMRplus","dunn.test","effsize","FSA","smotefamily") #rcompanion
.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)

