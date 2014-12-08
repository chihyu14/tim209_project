#Preprocessing 

train=read.csv("~/R/kdd99/kddcup.data_10_percent_corrected",header=FALSE)
header=read.csv("~/R/kdd99/colnames",header=FALSE)
names(train)=header[,1]
names(train)[42]="status"
colnames(train)[43]="category"


#Assign 23 status into 4 attack category

for(i in 1:nrow(train)){
if( (train[i,42]=="buffer_overflow.") | (train[i,42]=="rootkit.") | (train[i,42]=="loadmodule.") | (train[i,42]=="perl."))
train[i,43]="u2r."
else if( (train[i,42]=="warezclient.") | (train[i,42]=="guess_passwd.") | (train[i,42]=="warezmaster.") |
(train[i,42]=="imap.") | (train[i,42]=="ftp_write.") | (train[i,42]=="multihop.") | (train[i,42]=="phf.") | 
(train[i,42]=="spy.") )
train[i,43]="r21."
else if( (train[i,42]=="satan.") | (train[i,42]=="ipsweep.") | (train[i,42]=="portsweep.") | (train[i,42]=="nmap.") )
train[i,43]="probe."
else if( (train[i,42]=="smurf.") | (train[i,42]=="neptune.") | (train[i,42]=="back.") | (train[i,42]=="teardrop.") |
(train[i,42]=="pod.") | (train[i,42]=="land.") )
train[i,43]="dos."
else if( (train[i,42]=="normal.") )
train[i,43]="normal."
}

train.u2r=train[train[,43]=="u2r.",]
train.r21=train[train[,43]=="r21.",]
train.probe=train[train[,43]=="probe.",]
train.dos=train[train[,43]=="dos.",]
train.normal=train[train[,43]=="normal.",]

#
library(leaps)
u2r=train.u2r[,-43]
regfit.best=regsubsets(status~.,data=u2r,nvmax=41,really.big=T)




                      








