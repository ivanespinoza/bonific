setwd("C:/Users/u94132/Desktop/bonifici/")
source("libs.R")
source("configuration_file.R")
#Set Headers for all the months in the dataSet
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


### Take Out IBANs that didn't made any transfer since MAGGIO-2016
##  Create two matrices to store all the importo and numero values of the IBANs that are out 
outNum_m1=data.frame()
outImp_m1=data.frame()



for (i in 1:nrow(numMutuo)){
  if(sum(numMutuo[i,7:22])==0){
    outNum_m1=rbind(outNum, numMutuo[i,])
    outImp_m1=rbind(outImp, impMutuo[i,])
    #f=which(dat_mutuo$L_IBAN_ORDINANTE==numMutuo[i,]$IBAN)
    #dat_mutuo=dat_mutuo[-f,]
  }
}

#impMutuo=impMutuo[-as.numeric(row.names(outImp)),]
#numMutuo=numMutuo[-as.numeric(row.names(outNum)),]



### Set a new DataSet with all the IBANs that had one ESTINZIONE in the causale
# outImp$SUM16=rowSums(outImp[,2:13])
# outImp$SUM17=rowSums(outImp[,14:22])
### beneficiario bank analysis
# ben_bank=data.frame(matrix(ncol=3, nrow=0))
# names(ben_bank)<-c("Code","Bank","count")
# i=10
# dat_mutuo[i,]#for(i in 1:nrow(dat_mutuo)){}
# bank_code=substr(dat_mutuo[i,]$L_IBAN_BENEFICIARIO, 6, 10)
# rw=which(ben_bank$Code == bank_code)
#   if(!any(rw)){
#     query=paste("select c_banca, s_banca from edb.abi_italia
#                            where c_banca = '",bank_code,"'", sep = "")
#     query_res <-  dbGetQuery(con, query)
#     ben_bank[]
#   }
# 
impMutuo2=impMutuo[which(impMutuo$IBAN%in%e),]
impMutuoNorm <- as.data.frame(lapply(impMutuo2[-1], normalize))
require(dbscan)
db_clusters_iris <- dbscan(impMutuoNorm, eps=0.5)
print(db_clusters_iris)

hc = hclust(dist(impMutuo2[-1,2:22]))
plot(hc, hang = -1)
plot(impMutuo2[10,-1])

d=impMutuo2[which(db_clusters_iris$cluster==0),]
#d$SUM16=rowSums(d[,2:13])
#d$SUM17=rowSums(d[,14:22])
d$mean=rowMeans(d[,2:22])
d$StDv=apply(d[,2:22],1,sd)
d$Mean2=rowSums(d[,2:22])/rowSums(d[,2:22]>0)
d3=impMutuo2[which(db_clusters_iris$cluster==1),]
d3$mean=rowMeans(d3[,2:22])
d3$StDv=apply(d3[,2:22],1,sd)
d3$Mean2=rowSums(d3[,2:22])/rowSums(d3[,2:22]>0)

summary(d)
summary(d3)
sd(d3[20,2:22])
d3[20,]
norm(impMutuo2[-1])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#summary(impMutuoNorm)
#IT95W0538702409000002068814


#sum(numMutuo[i,13:22])==0 && sum(numMutuo[i,13:18])<3 && !(numMutuo[i,13]>0 && sum(numMutuo[i,2:22])>2)){
