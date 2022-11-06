# Datasets
if (DatasetCorrente==1){DatasetSelected<-read.csv(file = 'Datasets/01_chocardiogram.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==3){DatasetSelected<-read.csv(file = 'Datasets/03_heart-statlog.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==4){DatasetSelected<-read.csv(file = 'Datasets/04_ionosphere.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==5){DatasetSelected<-read.csv(file = 'Datasets/05_parkinsons.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==6){DatasetSelected<-read.csv(file = 'Datasets/06_balance-scale.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==7){DatasetSelected<-read.csv(file = 'Datasets/07_energy_s.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==8){DatasetSelected<-read.csv(file = 'Datasets/08_seeds.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==9){DatasetSelected<-read.csv(file = 'Datasets/09_teaching.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==10){DatasetSelected<-read.csv(file = 'Datasets/10_vertebral-column.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==11){DatasetSelected<-read.csv(file = 'Datasets/11_ecoli.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==12){DatasetSelected<-read.csv(file = 'Datasets/12_flags.csv_clean.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==13){DatasetSelected<-read.csv(file = 'Datasets/13_blood-transfusion-service-center.csv',header = TRUE, sep = ";")}
if (DatasetCorrente==14){DatasetSelected<-read.csv(file = 'Datasets/14_diabetes.csv',header = TRUE, sep = ";")}
if (DatasetCorrente==15){DatasetSelected<-read.csv(file = 'Datasets/15_tic-tac-toe.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==16){DatasetSelected<-read.csv(file = 'Datasets/16_madelon.csv',header = TRUE, sep = ";")}
if (DatasetCorrente==17){DatasetSelected<-read.csv(file = 'Datasets/17_Cryotherapy.csv',header = TRUE, sep = ";")}
if (DatasetCorrente==18){DatasetSelected<-read.csv(file = 'Datasets/18_car.csv',header = TRUE, sep = ",")}
if (DatasetCorrente==19){DatasetSelected<-read.csv(file = 'Datasets/19_vinnie.csv',header = TRUE, sep = ";")}
if (DatasetCorrente==20){DatasetSelected<-read.csv(file = 'Datasets/20_glass.csv',header = TRUE, sep = ";")}



if (DatasetCorrente==21){DatasetSelected<-read.csv(file = 'Datasets/21_banana.csv',header = TRUE, sep = ";")}
if (DatasetCorrente==22){DatasetSelected<-read.csv(file = 'Datasets/22_german_numer.csv',header = TRUE, sep = ";")}
if (DatasetCorrente==23){DatasetSelected<-read.csv(file = 'Datasets/23_colon-cancer.csv',header = TRUE, sep = ";")}
if (DatasetCorrente==24){DatasetSelected<-read.csv(file = 'Datasets/24_breast cancer.csv',header = TRUE, sep = ";")}



# Test and class by dataset
#dataset 1 - ok
if (DatasetCorrente==1){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)

}

#dataset 2 - corrigir (so tem classe 1)
if (DatasetCorrente==2){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
  
}

#dataset 3 - ok
if (DatasetCorrente==3){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
  
}

#dataset 4 - ok
if (DatasetCorrente==4){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-dataAnalise[1:ncol(dataAnalise)-1]
  dim(data)
  
}

#dataset 5 - ok
if (DatasetCorrente==5){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-dataAnalise[1:ncol(dataAnalise)-1]
  dim(data)
  
}

#dataset 6 - ok
if (DatasetCorrente==6){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  dataAnaliseClass<-as.numeric(dataAnaliseClass)
  dataAnaliseClass<-as.factor(dataAnaliseClass)
  
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
  
}

#dataset 7 - ok
if (DatasetCorrente==7){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-dataAnalise[1:ncol(dataAnalise)-1]
  dim(data)
  
}

#dataset 8 - ok
if (DatasetCorrente==8){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-dataAnalise[1:ncol(dataAnalise)-1]
  dim(data)
  
}

#dataset 9 - ok
if (DatasetCorrente==9){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-dataAnalise[1:ncol(dataAnalise)-1]
  dim(data)
  
}

#dataset 10 - ok
if (DatasetCorrente==10){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-dataAnalise[1:ncol(dataAnalise)-1]
  dim(data)
 
}

#dataset 11 - ok
if (DatasetCorrente==11){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
  
}

#dataset 12 - ok
if (DatasetCorrente==12){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-dataAnalise[2:ncol(dataAnalise)-1]
  dim(data)
  
}

#dataset 13 - ok
if (DatasetCorrente==13){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
  
}

#dataset 14 - ok
if (DatasetCorrente==14){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  
  for (j in 1:dataAnaliseRow){
    #for (j in 1:szdata2){
    
    if(dataAnalise[j,dataAnaliseCol] == "tested_positive"){dataAnalise[j,dataAnaliseCol]= 0} # substitutindo tested_positive letra B por 0
    if(dataAnalise[j,dataAnaliseCol] == "tested_negative"){dataAnalise[j,dataAnaliseCol]= 1} # substitutindo x letra B por 0
    
    #}
  }
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
}

#dataset 15 - OBS
if (DatasetCorrente==15){
  dataAnalise<-as.data.frame(DatasetSelected);
  
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  dataAnaliseClass<-(dataAnalise[,dataAnaliseCol])
  for (j in 1:dataAnaliseRow){
    #for (j in 1:szdata2){
    
    if(dataAnaliseClass[j] == "negative"){dataAnaliseClass[j]= 0} # substitutindo x letra B por 0
    if(dataAnaliseClass[j] == "positive"){dataAnaliseClass[j]= 1} # substitutindo x letra B por 0
    
    #}
  }
  
  dataAnaliseClass<-as.numeric(dataAnaliseClass)
  dataAnaliseClass<-as.factor(dataAnaliseClass)
  numeroClasses<-length(levels(dataAnaliseClass))
  
  data<-(dataAnalise[2:ncol(dataAnalise)-1])
  data<-as.data.frame(data)
  szdata1<-dim(data)[1]*dim(data)[2]
  for (i in 1:szdata1){
    
    if(data[i] == "x"){data[i]= 0} # substitutindo x letra B por 0
    if(data[i] == "b"){data[i]= 1} # substitutindo x letra B por 0
    if(data[i] == "o"){data[i]= 2} # substitutindo x letra B por 0
    
  }
  
  data<-scale(data)
  dim(data)
}

#dataset 16 - ok
if (DatasetCorrente==16){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
}

#dataset 17 - obs
if (DatasetCorrente==17){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
}

#dataset 18 - obs
if (DatasetCorrente==18){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-dataAnalise[2:ncol(dataAnalise)-1]
  dim(data)
}

#dataset 19 - ok
if (DatasetCorrente==19){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
 # for (j in 1:dataAnaliseRow){
    #for (j in 1:szdata2){
    
  #  if(dataAnalise[j,dataAnaliseCol] == "N"){dataAnalise[j,dataAnaliseCol]= 0} # substitutindo tested_positive letra B por 0
  #  if(dataAnalise[j,dataAnaliseCol] == "P"){dataAnalise[j,dataAnaliseCol]= 1} # substitutindo x letra B por 0
    
    #}
  
  #}
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
}

#dataset 20 - ok
if (DatasetCorrente==20){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
 
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
}

if (DatasetCorrente==21){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
  
}

if (DatasetCorrente==22){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
  
}

if (DatasetCorrente==23){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
  
}

if (DatasetCorrente==24){
  dataAnalise<-as.data.frame(DatasetSelected);
  # Estatistica do dataset selecionado
  summary(dataAnalise)
  
  #dataAnalise$f2<-as.factor(dataAnalise$f2)
  dataAnaliseRow<-dim(dataAnalise)[1]
  dataAnaliseCol<-dim(dataAnalise)[2]
  
  dataAnaliseClass<-as.factor(dataAnalise[,dataAnaliseCol])
  numeroClasses<-length(levels(dataAnaliseClass))
  data<-scale(dataAnalise[1:ncol(dataAnalise)-1])
  dim(data)
  
}
