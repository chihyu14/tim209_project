#Preprocessing 
work=c(train,test)
train=read.csv("~/R/kdd99/kddcup.data_10_percent_corrected",header=FALSE)
test=read.csv("~/R/kdd99/kddcup.newtestdata_10_percent_unlabeled",header=FALSE)
colnames=read.csv("~/R/kdd99/colnames",header=FALSE)
header=FALSE
if(!header){
for(i in 1:nrow(colnames)){
x[i]=sub(": continuous.","",colnames[i,1])
header[i]=sub(": symbolic.","",x[i,1])
}
}

names(train)=header
names(test)=header
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

#logistics
//library(leaps)
//u2r=train.u2r[,-43]
//regfit.best=regsubsets(status~.,data=u2r,nvmax=41,really.big=T)


#KNN
library(class)
train.x=train[-c(2,3,4),-c(42,43)] //make train has 41 column same as test and strip out qualitative features
test.x=test[-c(2,3,4),]  //strip out qualitative features
knn.pred=knn(train.x,test,train[,42],k=5)
table(knn.pred, train[,42])


# Decision trees
attach(train)
ids_tree <- tree(status ~ duration+flag+protocol_type+src_bytes+dst_bytes+land+wrong_fragment+urgent+hot+num_failed_logins+logged_in+num_compromised+root_shell+su_attempted+num_root+num_file_creations+num_shells+num_access_files+num_outbound_cmds+is_host_login+is_guest_login+count+srv_count+serror_rate+srv_serror_rate+rerror_rate+srv_rerror_rate+same_srv_rate+diff_srv_rate+srv_diff_host_rate+dst_host_count+dst_host_srv_count+dst_host_same_srv_rate+dst_host_diff_srv_rate+dst_host_same_src_port_rate+dst_host_srv_diff_host_rate+dst_host_serror_rate+dst_host_srv_serror_rate+dst_host_rerror_rate+dst_host_srv_rerror_rate, train)



                      








