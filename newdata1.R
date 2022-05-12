# Survey Data May 2022
# SGH w Warszawie 
setwd("D:/NEW SGH Survey Project/Data")
options(max.print = 99999999)
library(foreign)
## Open data
pl<-read.dta("Poland-2019-full data.dta")
lv<-read.dta("Latvia-2019-full data.dta")
data<-pl
#data<-lv
dim(data)

#subset to manuf?
man<-unique(data$d1a1a)[2]
man<-as.character(man)
man<-noquote(man)
data<-data[data$d1a1a==man,]
dim(data)

#### Exports (DV)
head(data)
data[,85] #d3c, direct export share
sum(data[,85]>0)
sum(data[,85]>=0)
sum(data[,85]==0)
hist(data[data[,85]>0,85],breaks = 25,main =paste0("Histogram of Qd3c, N = ",sum(data[,85]>0)),xlab="Values")
hist(data[data[,85]>=0,85],breaks = 31,main =paste0("Histogram of Qd3c, N = ",sum(data[,85]>=0)),xlab="Values")


colnames(data)[84] # indirect
sum(data[,84]>0)
sum(data[,84]>=0)
sum(data[,84]==0)
hist(data[data[,84]>0,84],breaks = 25,main =paste0("Histogram of Qd3c, N = ",sum(data[,84]>0)),xlab="Values")
hist(data[data[,84]>=0,84],breaks = 31,main =paste0("Histogram of Qd3c, N = ",sum(data[,84]>=0)),xlab="Values")

dim(data)
# Lots of -9s
data[data[,85]>0,]
data[data[,85]>=0,]


# Subset to data[,85]>0
data<-data[data[,85]>=0,]
dim(data)


# Trade shares
tradedir<-data[,85]
tradeind<-data[,84]
tradetot<-data[,84]+data[,85]
plot(sort(tradedir),type="l")

# Explanatory variables
#RD over last 3 years
rdfirm<-data$BMh2
#Acq. of external knowledge
extkn<-data$BMh1
#RD w/other firms
rdext<-data$BMh3

table(rdfirm)
table(extkn)
table(rdext)

# Competition
mainmkt<-data$e1
compintens<-data$e2
compinf<-data$e11
table(mainmkt)
table(compintens)
table(compinf)

#product innovation, process innovation, age
prodinn<-data$h1
procinn<-data$h5
age<-2019-data$b5
age<-ifelse(age==2028,0,age)

#prodinn<-as.character(prodinn)
#prodinn=="Yes"

controls<-cbind(as.character(prodinn),as.character(procinn),as.character(rdfirm),as.character(rdext),as.character(extkn),as.character(age))
controls[,1:5]<-ifelse(controls[,1:5]=="Yes",1,0)
controls<-noquote(controls)
colnames(controls)<-c("prodinn","procinn","rdfirm","rdext","extkn","age")
controls

#############################################################
### Summary table for all variables
l1<-29:429

l2<-NULL
for(i in l1){
  d1<-na.omit(data[,i])
  d2<-unique(d1)
  l2<-c(l2,length(d2))
}
l2
l1<-l1[l2<10]
length(l1)

# Count table
counttable<-NULL
for(i in l1){
  d1<-na.omit(data[,i])
  d1<-as.character(d1)
  d2<-as.character(d2)
  l3<-length(unique(d1))
  l4<-9-l3
  l5<-rep("",l4)
  t1<-table(d1)
  t2<-rownames(t1)
  QUESTION<-c(t2,l5)
  counttable<-cbind(counttable,QUESTION,c(t1,l5))
  colnames(counttable)[ncol(counttable)]<-colnames(data)[i]
  }
counttable<-noquote(counttable)
counttable
#write.csv(counttable,"counttable.csv")


# Examining select variables
mainmkt2<-as.character(mainmkt)
mmtype<-c(1,2,-9,3) # Three types
for(i in 1:length(mmtype)){
mainmkt2[mainmkt2==unique(mainmkt2)[i]]<-mmtype[i]
  }
noquote(mainmkt2)

compintens2<-as.character(compintens)
unique(compintens2) # This is numeric, -4 = too many to count

compinf2<-as.character(compinf)
citype<-c(1,2,-9) # Yes/No
unique(compinf2)
for(i in 1:length(citype)){
  compinf2[compinf2==unique(compinf2)[i]]<-citype[i]
}
noquote(compinf2)

# Business environment
m1<-data[,294:308]
dim(m1)
m1

sum(rowSums(m1)!=sum(1:15)) # each respondent orders the list 1-15
