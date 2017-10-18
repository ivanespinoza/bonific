#setwd("C:/Users/u94132/Desktop/bonifici/")
setwd("K:/PSG/1. Procedure Sistemi di BI e DW/BD_advanced_analytics/bonifici/bonifici2/byFunctions")
source("functions.R")
source("libs.R")
### Set Headers for all the months in the dataSet
months <- c("IBAN", "201601","201602","201603","201604","201605","201606",
                    "201607","201608","201609","201610","201611","201612",
                    "201701","201702","201703","201704","201705","201706",
                    "201707","201708","201709")
### Load the results of the , 
### qery file with the rows of the database
dat=readRDS("files/data.rds")

# change the rowIdexes of the dataFrames
rownames(dat)=c(1:nrow(dat))

### Set a new DataSet with all the transfers that have MUTUO in the causale
#
dat_mutuo_all=dat[grep("MUTUO",str_to_upper(dat$X_CAUSALE)),]

### Create the two matrices to store amount and number of transfers
##  impMUTUO with the sum of the importo in a month
##  numMUTUO with the number of transfers in a month

numMutuo <- data.frame(matrix(nrow=0, ncol=length(months)))
impMutuo <- data.frame(matrix(nrow=0, ncol=length(months)))

## rename the variables in the matrix with the value of "months" for 
names(numMutuo) <- c(months)
names(impMutuo) <- c(months)

for (i in 1:nrow(dat_mutuo_all)){
  rw=which(numMutuo$IBAN == dat_mutuo_all[i,]$L_IBAN_ORDINANTE)
  m_t=paste(year(dmy(dat_mutuo_all[i,]$D_ESECUZIONE)),sprintf("%02d",month(dmy(dat_mutuo_all[i,]$D_ESECUZIONE))),sep="")
  if(!any(rw)){
    numMutuo[(nrow(numMutuo)+1),"IBAN"]=dat_mutuo_all[i,]$L_IBAN_ORDINANTE
    impMutuo[(nrow(impMutuo)+1),"IBAN"]=dat_mutuo_all[i,]$L_IBAN_ORDINANTE
    numMutuo[nrow(numMutuo), m_t] = 1
    impMutuo[nrow(impMutuo), m_t] = as.numeric(dat_mutuo_all[i,]$I_IMPORTO)
    numMutuo[nrow(numMutuo), is.na(numMutuo[nrow(numMutuo),])] <- 0
    impMutuo[nrow(impMutuo), is.na(impMutuo[nrow(impMutuo),])] <- 0
    
  } else{
    numMutuo[rw, m_t]= numMutuo[rw,m_t] + 1
    impMutuo[rw, m_t]= impMutuo[rw,m_t] + as.numeric(dat_mutuo_all[i,]$I_IMPORTO)
  }
}
#impMutuo=readRDS("impMutuo.rds")
#numMutuo=readRDS("numMutuo.rds")

### OUTLIERS
## Take out some outliers or cases that are not interesting for the study
## take out IBANS that only made transfers in the first 4ths months and no more

numMutuo=numMutuo[-which(rowSums(numMutuo[,6:22])==0),]
impMutuo=impMutuo[-which(rowSums(numMutuo[,6:22])==0),]

### CLUSTERING FOR NUMERO
## Adding features to the dataFrame
#  - Sum of the quantity
#  - Count of the quantity
#  - mean (total) 
#  - mean (real) 
#  - Standart deviation

numPlus=numMutuo[-1]
numPlus$Sum=rowSums(numMutuo[-1])
numPlus$Count=rowSums(numMutuo[-1]>0)
numPlus$Mean=rowMeans(numMutuo[-1])
numPlus$Mean2=rowSums(numMutuo[-1])/rowSums(numMutuo[-1]>0)
numPlus$sd=apply(numMutuo[-1],1,sd)

## Normalize the DataFrame 
# The first set of variables the Months norm. by the entire population of the variables
# The second set of variables the additional features norm. by every variable given

numPlus_N=cbind(normalize(numMutuo[-1]),  as.data.frame(lapply(numPlus[22:26], normalize)))
summary(numPlus_N)


## Create the KMeans Clusters for comparization
clust_=0
for (i in 1:50){
  clust=kmeans(numPlus_N,5)
  clust_=clust_+sort(clust$size)
}
clust_=clust_/50
clust_
clust$size
f=c()

for(i in 1:20){
  clust=kmeans(numPlus_N,i)
  f[i]=clust$tot.withinss
}

plot(f, type="b")


clust=kmeans(numPlus_N,3)
clust$size
class_DB <- randomForest(x = numPlus_N, y = clust$cluster,  ntree=500) 
feature_rank_ord_DB <- order(class_DB$importance, decreasing = T)
feature_rank_ord_DB

numClus_1=numPlus[which(clust$cluster==1),]
numClus_2=numPlus[which(clust$cluster==2),]
numClus_3=numPlus[which(clust$cluster==3),]


# 
# clust0$ifault
# sort(clust0$size)/2+sort(clust1$size)
# 
# clust_1=as.data.frame(sort(clust1$size))
# clust_2=as.data.frame(sort(clust2$size))
# clust_3=as.data.frame(sort(clust3$size))
# 
# clust_0=cbind(clust_0,sort(clust0$size))
# 
# 
# 
# toCvs=cbind(numMutuo, clust0$cluster, clust1$cluster, clust2$cluster, clust3$cluster, clust4$cluster)
# 
# toCvs[which(toCvs$`clust4$cluster`==4),]$`clust4$cluster`=10
# toCvs[which(toCvs$`clust4$cluster`==2),]$`clust4$cluster`=20
# toCvs[which(toCvs$`clust4$cluster`==3),]$`clust4$cluster`=30
# toCvs[which(toCvs$`clust4$cluster`==5),]$`clust4$cluster`=40
# toCvs[which(toCvs$`clust4$cluster`==1),]$`clust4$cluster`=50
# toCvs[which(toCvs$`clust4$cluster`==6),]$`clust4$cluster`=60
# 
# write.csv2(toCvs, "numMutuo_clust.csv")
# 
# clust0$size
# clust1$size
# clust2$size
# clust3$size
# clust4$size
# 
# rowMeans(clust0$centers)
# rowMeans(clust1$centers)
# rowMeans(clust2$centers)
# rowMeans(clust3$centers)
# rowMeans(clust4$centers)
# 
# 
# cl3_1=numPlus[which(clust3$cluster==1),]
# summary(cl3_1)  #2000
# 
# cl3_2=numPlus[which(clust3$cluster==2),]
# summary(cl3_2) #384 (intresting)
# 
# cl3_3=numPlus[which(clust3$cluster==3),]
# summary(cl3_3) 
# 
# cl3_4=numPlus[which(clust3$cluster==4),]
# summary(cl3_4) 
# dim(cl3_4)
# 
# cl3_5=numPlus[which(clust3$cluster==5),]
# summary(cl3_5) #344 (intresting)
# dim(cl3_5)
# 
# cl3_6=numPlus[which(clust3$cluster==6),]
# summary(cl3_6) #992 (intresting)
# dim(cl3_6)
# 
# ## Analyze the correlation Matrix of both of the DataSets and between each other
# library(corrplot)
# corM=merge(x=numMutuo[,1:22], y=impMutuo[,1:22], by="IBAN", all=TRUE)
# corMatrix=cor(corM)
# corrplot(corMatrix, method="circle")
# 
# plot(impMutuo$`201703`)
# impMutuo[impMutuo==0]=NA
# impMutuo[is.na(impMutuo)]=0
# which(impMutuo$`201705` > 50000)
# 
# 
# 
# numCl=numMutuo[-1]
# impMutuoNorm <- as.data.frame(lapply(imp[-1], normalize))
# 
# 
# 
# 
# 
# hclust(corM[-1])
# clusters <- hclust(km[-1])
# 
# 
# #plot(clusters)
# plot(clusters$cluster)
# plot(km[-1])
# ### Take Out IBANs that didn't made any transfer since MAGGIO-2016
# ##  Create two matrices to store all the importo and numero values of the IBANs that are out 
# outNum_m1=data.frame()
# outImp_m1=data.frame()
# 
# for (i in 1:nrow(numMutuo)){
#   if(sum(numMutuo[i,7:22])==0){
#     outNum_m1=rbind(outNum, numMutuo[i,])
#     outImp_m1=rbind(outImp, impMutuo[i,])
#     #f=which(dat_mutuo$L_IBAN_ORDINANTE==numMutuo[i,]$IBAN)
#     #dat_mutuo=dat_mutuo[-f,]
#   }
# }


#impMutuo=impMutuo[-as.numeric(row.names(outImp)),]
#numMutuo=numMutuo[-as.numeric(row.names(outNum)),]



