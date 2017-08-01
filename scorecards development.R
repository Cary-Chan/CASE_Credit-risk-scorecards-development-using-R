###############1.Exploratory Data Analysis (EDA)###############
#install.packages("klaR")
library(klaR);library(MASS)
data(GermanCredit)

#Sampling
train_kfold<-sample(nrow(GermanCredit),800,replace=F)
train_kfolddata<-GermanCredit[train_kfold,]
test_kfolddata<-GermanCredit[-train_kfold,]

#Turning the dependent variable as ‘0-1’ variable
credit_risk<-ifelse(train_kfolddata[,"credit_risk"]=="good",0,1)
tmp<-train_kfolddata[,-21]
data<-cbind(tmp,credit_risk)
#attach(data)
View(data)

###############2.Choose Important Variables###############
     ###2.1 Choose Important Quantitive Variables using Stepwise Method###
#Get the quantitive variables in the original dataset
quant_vars<-c("duration","amount","installment_rate","present_residence",
              "age","number_credits","people_liable","credit_risk")
quant_GermanCredit<-data[,quant_vars]

#Get the intercept of the regression model
base.mod<-lm(credit_risk~1,data=quant_GermanCredit)
#Get the full regression model 
all.mod<-lm(credit_risk~.,data=quant_GermanCredit)
#Select a formula-based model by AIC
stepMod<-step(base.mod,scope=list(lower=base.mod,upper=all.mod),
              direction="both",trace=0,steps=1000)
#Get the names in the upper step
shortlistedVars<-names(unlist(stepMod[[1]]))
#Delete the intercept 
shortlistedVars<-shortlistedVars[!shortlistedVars%in%"(Intercept)"]
print(shortlistedVars)

#The chosen important quantitive variables
quant_model_vars<-c("duration","amount","installment_rate","age")

     ###2.2 Choose Important Qualitative Variables according to Information Value###
#Get the qualitative variables in the original dataset
factor_vars<-c("status","credit_history","purpose","savings",
               "employment_duration","personal_status_sex","other_debtors",
               "property","other_installment_plans","housing",
               "job","telephone","foreign_worker")

#Calculation of Information Value for each variable
VARS<-factor_vars
IV<-numeric(length(factor_vars))
STRENGTH<-character(length(factor_vars))
all_iv<-data.frame(VARS,IV,STRENGTH,stringsAsFactors=F)
for (factor_var in factor_vars)
{
  #Get IV
  all_iv[all_iv$VARS==factor_var,"IV"]<-
    InformationValue::IV(X=data[,factor_var],Y=data$credit_risk)
  #Abstract IV
  all_iv[all_iv$VARS==factor_var,"STRENGTH"]<-
    attr(InformationValue::IV(X=data[,factor_var],Y=data$credit_risk),"howgood")
}
all_iv<-all_iv[order(-all_iv$IV),]
all_iv

#The chosen important qualitative variables
qual_model_vars<-c("status","credit_history","savings","purpose","property")

###############3.Binning for Scoring Modeling & Computes Weight of Evidence(WoE)###############
     ###3.1 Under the Condition of Quantitive Variables###

  ##3.1.1 duration
#Optimal binning & Get WoE
library(smbinning)
result_duration<-smbinning(df=data,y="credit_risk",x="duration",p=0.01)
result_duration$ivtable

#Get the data frame including Cutpoint and WoE
duration_Cutpoint<-c()
duration_WoE<-c()
duration<-data[,"duration"]
for (i in 1:length(duration))
{
  if (duration[i]<=11)
  {
    duration_Cutpoint[i]<-"<=11"
    duration_WoE[i]<--1.0127
  }
  if (duration[i]>11&&duration[i]<=33)
  {
    duration_Cutpoint[i]<-">11&&<=33"
    duration_WoE[i]<-0.0080
  }
  if (duration[i]>33)
  {
    duration_Cutpoint[i]<-">33"
    duration_WoE[i]<-0.7948
  }
}
a<-as.data.frame(cbind(duration_Cutpoint,duration_WoE))
#View(a)

  ##3.1.2 amount
#Optimal binning & Get WoE
result_amount<-smbinning(df=data,y="credit_risk",x="amount",p=0.05)
result_amount$ivtable

#Get the data frame including Cutpoint and WoE
amount_Cutpoint<-c()
amount_WoE<-c()
amount<-data[,"amount"]
for (i in 1:length(amount))
{
  if (amount[i]<=1372)
  {
    amount_Cutpoint[i]<-"<=1372"
    amount_WoE[i]<-0.1559
  }
  if (amount[i]>1372&&amount[i]<=4042)
  {
    amount_Cutpoint[i]<-">1372&&<=4042"
    amount_WoE[i]<--0.4365
  }
  if (amount[i]>4042&&amount[i]<=8947)
  {
    amount_Cutpoint[i]<-">4042&&<=8947"
    amount_WoE[i]<-0.3787
  }
  if (amount[i]>8947)
  {
    amount_Cutpoint[i]<-">8947"
    amount_WoE[i]<-1.2590
  }
}
b<-as.data.frame(cbind(amount_Cutpoint,amount_WoE))
#View(b)

  #3.1.3 age
#Optimal binning & Get WoE
result_age<-smbinning(df=data,y="credit_risk",x="age",p=0.05)
result_age$ivtable

#Get the data frame including Cutpoint and WoE
age_Cutpoint<-c()
age_WoE<-c()
age<-data[,"age"]
for (i in 1:length(age))
{
  if (age[i]<=25)
  {
    age_Cutpoint[i]<-"<=25"
    age_WoE[i]<-0.6840
  }
  if (age[i]>25)
  {
    age_Cutpoint[i]<-">25"
    age_WoE[i]<--0.1828
  }
}
c<-as.data.frame(cbind(age_Cutpoint,age_WoE))
#View(c)

  #3.1.4 installment_rate
#Binning & Get WoE
install_data<-data[,c("installment_rate","credit_risk")]
tbl<-table(install_data)
View(tbl)
total<-list()
for(i in 1:nrow(tbl))
{
  total[i]<-sum(tbl[i,])
}
t.tbl<-cbind(tbl,total)
GoodRate<-as.numeric(t.tbl[,"0"])/as.numeric(t.tbl[,"total"])
BadRate<-as.numeric(t.tbl[,"1"])/as.numeric(t.tbl[,"total"])
gb.tbl<-cbind(t.tbl,GoodRate,BadRate)
Odds<-GoodRate/BadRate
LnOdds<-log(Odds)
tt.tbl<-cbind(gb.tbl,Odds,LnOdds)
WOE<-log((as.numeric(tt.tbl[,"0"])/700)/((as.numeric(tt.tbl[,"1"])/300)))
all.tbl<-cbind(tt.tbl,WOE)
all.tbl

#Get the data frame including Cutpoint and WoE
installment_rate_Cutpoint<-c()
installment_rate_WoE<-c()
installment_rate<-data[,"installment_rate"]
for (i in 1:length(installment_rate))
{
  if (installment_rate[i]==1)
  {
    installment_rate_Cutpoint[i]<-"=1"
    installment_rate_WoE[i]<-0.1421151 
  }
  if (installment_rate[i]==2)
  {
    installment_rate_Cutpoint[i]<-"=2"
    installment_rate_WoE[i]<-0.1459539
  }
  if (installment_rate[i]==3)
  {
    installment_rate_Cutpoint[i]<-"=3"
    installment_rate_WoE[i]<-0.04652002
  }
  if (installment_rate[i]==4)
  {
    installment_rate_Cutpoint[i]<-"=4"
    installment_rate_WoE[i]<--0.07093437
  }
}
d<-as.data.frame(cbind(installment_rate_Cutpoint,installment_rate_WoE))
#View(d)

     ###3.2 Under the Condition of Qualitative Variables###
discrete_data<-data[,c("status","credit_history","savings","purpose","property","credit_risk")]

#Reducing dimensions of purpose
summary(discrete_data$purpose)
x<-discrete_data[,c("purpose","credit_risk")]
d<-as.matrix(x)
for (i in 1:nrow(d))
{
  #Merging car (new) & car (used) 
  if(as.character(d[i,"purpose"])=="car (new)")
  {
    d[i,"purpose"]<-as.character("car(new/used)")
  }
  if(as.character(d[i,"purpose"])=="car (used)")
  {
    d[i,"purpose"]<-as.character("car(new/used)")
  }
  #Merging radio/television & furniture/equipment
  if(as.character(d[i,"purpose"])=="radio/television")
  {
    d[i,"purpose"]<-as.character("radio/television/furniture/equipment")
  }
  if(as.character(d[i,"purpose"])=="furniture/equipment")
  {
    d[i,"purpose"]<-as.character("radio/television/furniture/equipment")
  }
  #Merging others、repairs & business
  if(as.character(d[i,"purpose"])=="others")
  {
    d[i,"purpose"]<-as.character("others/repairs/business")
  }
  if(as.character(d[i,"purpose"])=="repairs")
  {
    d[i,"purpose"]<-as.character("others/repairs/business")
  }
  if(as.character(d[i,"purpose"])=="business")
  {
    d[i,"purpose"]<-as.character("others/repairs/business")
  }
  #Merging retraining & education
  if(as.character(d[i,"purpose"])=="retraining")
  {
    d[i,"purpose"]<-as.character("retraining/education")
  }
  if(as.character(d[i,"purpose"])=="education")
  {
    d[i,"purpose"]<-as.character("retraining/education")
  }
}
new_data<-cbind(discrete_data[,c(-4,-6)],d)
View(new_data)
summary(new_data)

#Get WoE of all qualitative variables
woemodel<-woe(credit_risk~.,data=new_data,zerodj=0.5,applyontrain=TRUE)
woemodel$woe

  ##3.2.1 purpose
#Get the data frame including WoE
purpose<-as.matrix(new_data[,"purpose"])
colnames(purpose)<-"purpose"
purpose_WoE<-c()
for (i in 1:length(purpose))
{
  if (purpose[i]=="car(new/used)")
  {
    purpose_WoE[i]<--0.06717457
  }
  if (purpose[i]=="domestic appliances")
  {
    purpose_WoE[i]<-0.47712698
  }
  if (purpose[i]=="others/repairs/business")
  {
    purpose_WoE[i]<--0.07963519
  }
  if (purpose[i]=="radio/television/furniture/equipment")
  {
    purpose_WoE[i]<--0.21511654
  }
  if (purpose[i]=="retraining/education")
  {
    purpose_WoE[i]<--0.58354037
  }
}
aa<-as.data.frame(purpose_WoE)
#View(aa)

  ##3.2.2 status
#Get the data frame including WoE
status<-as.matrix(new_data[,"status"])
colnames(status)<-"status"
status_WoE<-c()
for (i in 1:length(status))
{
  if (status[i]=="... < 100 DM")
  {
    status_WoE[i]<--0.8060831
  }
  if (status[i]=="0 <= ... < 200 DM")
  {
    status_WoE[i]<--0.4432845
  }
  if (status[i]=="... >= 200 DM / salary for at least 1 year")
  {
    status_WoE[i]<-0.5951146
  }
  if (status[i]=="no checking account")
  {
    status_WoE[i]<-1.2046413
  }
}
bb<-as.data.frame(status_WoE)
#View(bb)

  ##3.2.3 credit_history
#Get the data frame including WoE
credit_history<-as.matrix(new_data[,"credit_history"])
colnames(credit_history)<-"credit_history"
credit_history_WoE<-c()
for (i in 1:length(credit_history))
{
  if (credit_history[i]=="no credits taken/all credits paid back duly")
  {
    credit_history_WoE[i]<--1.43083823
  }
  if (credit_history[i]=="all credits at this bank paid back duly")
  {
    credit_history_WoE[i]<--1.07189314
  }
  if (credit_history[i]=="existing credits paid back duly till now")
  {
    credit_history_WoE[i]<--0.12050541
  }
  if (credit_history[i]=="delay in paying off in the past")
  {
    credit_history_WoE[i]<-0.05553959
  }
  if (credit_history[i]=="critical account/other credits existing")
  {
    credit_history_WoE[i]<-0.76353327
  }
}
cc<-as.data.frame(credit_history_WoE)
#View(cc)

  ##3.2.4 savings
#Get the data frame including WoE
savings<-as.matrix(new_data[,"savings"])
colnames(savings)<-"savings"
savings_WoE<-c()
for (i in 1:length(savings))
{
  if (savings[i]=="... < 100 DM")
  {
    savings_WoE[i]<--0.2466379
  }
  if (savings[i]=="100 <= ... < 500 DM")
  {
    savings_WoE[i]<--0.2260845
  }
  if (savings[i]=="500 <= ... < 1000 DM")
  {
    savings_WoE[i]<-0.6451250
  }
  if (savings[i]=="... >= 1000 DM")
  {
    savings_WoE[i]<-1.1436806
  }
  if (savings[i]=="unknown/no savings account")
  {
    savings_WoE[i]<-0.7139228
  }
}
dd<-as.data.frame(savings_WoE)
#View(dd)

  ##3.2.5 property
#Get the data frame including WoE
property<-as.matrix(new_data[,"property"])
colnames(property)<-"property"
property_WoE<-c()
for (i in 1:length(property))
{
  if (property[i]=="real estate")
  {
    property_WoE[i]<-0.43192155
  }
  if (property[i]=="building society savings agreement/life insurance")
  {
    property_WoE[i]<--0.03610500
  }
  if (property[i]=="car or other")
  {
    property_WoE[i]<--0.03778963
  }
  if (property[i]=="unknown/no property")
  {
    property_WoE[i]<--0.56248696
  }
}
ee<-as.data.frame(property_WoE)
#View(ee)

###############4.Logistic Regiression###############
#The final data incuding quantitive and qualitative in the model
model_data<-cbind(data[,quant_model_vars],data[,qual_model_vars])

#WoE of different variables used in the model
credit_risk<-as.matrix(data[,"credit_risk"])
colnames(credit_risk)<-"credit_risk"
model_data_WoE<-as.data.frame(cbind(duration_WoE,amount_WoE,age_WoE,installment_rate_WoE,
                                    status_WoE,credit_history_WoE,savings_WoE,property_WoE,purpose_WoE,credit_risk))
#View(model_data_WoE)
model_data_Cutpoint<-cbind(duration_Cutpoint,amount_Cutpoint,age_Cutpoint,installment_rate_Cutpoint,
                           status,credit_history,savings,property,purpose)
#View(model_data_Cutpoint)

#Logistic regiression
m<-glm(credit_risk~.,data=model_data_WoE,family=binomial())
summary(m)
cofficients<-m$coefficients  
cofficients
alpha_beta<-function(basepoints,baseodds,pdo)
{
  beta<-pdo/log(2)
  alpha<-basepoints+beta*log(baseodds)
  return(list(alpha=alpha,beta=beta))
}
x<-alpha_beta(50,0.05,10)

###############5.Building Score Card###############
     ###5.1 Calculate Scores of Different Variables
  ##5.1.0 Base_score 
basepoint<-round(x$alpha-x$beta*cofficients[1])
  ##5.1.1 Duration_score
duration_score<-round(as.matrix(-(model_data_WoE[,"duration_WoE"]*cofficients["duration_WoE"]*x$beta)))
colnames(duration_score)<-"duration_score"
#View(duration_score)
  ##5.1.2 Amount_score
amount_score<-round(as.matrix(-(model_data_WoE[,"amount_WoE"]*cofficients["amount_WoE"]*x$beta)))
colnames(amount_score)<-"amount_score"
#View(amount_score)
  ##5.1.3 Age_score
age_score<-round(as.matrix(-(model_data_WoE[,"age_WoE"]*cofficients["age_WoE"]*x$beta)))
colnames(age_score)<-"age_score"
#View(age_score)
  ##5.1.4 Installment_rate_score
installment_rate_score<-round(as.matrix(-(model_data_WoE[,"installment_rate_WoE"]*cofficients["installment_rate_WoE"]*x$beta)))
colnames(installment_rate_score)<-"installment_rate_score"
#View(installment_rate_score)

  ##5.1.5 Status_score
status_score<-round(as.matrix(-(model_data_WoE[,"status_WoE"]*cofficients["status_WoE"]*x$beta)))
colnames(status_score)<-"status_score"
#View(status_score)
  ##5.1.6 Credit_history_score
credit_history_score<-round(as.matrix(-(model_data_WoE[,"credit_history_WoE"]*cofficients["credit_history_WoE"]*x$beta)))
colnames(credit_history_score)<-"credit_history_score"
#View(credit_history_score)
  ##5.1.7 Savings_score
savings_score<-round(as.matrix(-(model_data_WoE[,"savings_WoE"]*cofficients["savings_WoE"]*x$beta)))
colnames(savings_score)<-"savings_score"
#View(savings_score)
  ##5.1.8 Property_score
property_score<-round(as.matrix(-(model_data_WoE[,"property_WoE"]*cofficients["property_WoE"]*x$beta)))
colnames(property_score)<-"property_score"
#View(property_score)
  ##5.1.9 Purpose_score
purpose_score<-round(as.matrix(-(model_data_WoE[,"purpose_WoE"]*cofficients["purpose_WoE"]*x$beta)))
colnames(purpose_score)<-"purpose_score"
#View(purpose_score)

     ###5.2 Get The final score card
  ##5.2.0 base_score
r1<-c("","basepoint",19)
m1<-matrix(r1,nrow=1)
colnames(m1)<-c("Basepoint","Basepoint","Score")
  ##5.2.1 duration_score
duration_scoreCard<-cbind(as.matrix(c("Duration","",""),nrow=3,ncol=1),unique(cbind(duration_Cutpoint,duration_score)))
View(duration_scoreCard)
  ##5.2.2 amount_score
amount_scoreCard<-cbind(as.matrix(c("Amount","","",""),nrow=4,ncol=1),unique(cbind(amount_Cutpoint,amount_score)))
View(amount_scoreCard)
  ##5.2.3 age_score
age_scoreCard<-cbind(as.matrix(c("Age",""),nrow=2,ncol=1),unique(cbind(age_Cutpoint,age_score)))
View(age_scoreCard)
  ##5.2.4 installment_rate_score
installment_rate_scoreCard<-cbind(as.matrix(c("Installment_rate","","",""),nrow=4,ncol=1),unique(cbind(installment_rate_Cutpoint,installment_rate_score)))
View(installment_rate_scoreCard)

  ##5.2.5 status_score
status_scoreCard<-cbind(as.matrix(c("Status","","",""),nrow=4,ncol=1),unique(cbind(status,status_score)))
View(status_scoreCard)
  ##5.2.6 credit_history_score
credit_history_scoreCard<-cbind(as.matrix(c("Credit_history","","","",""),nrow=5,ncol=1),unique(cbind(credit_history,credit_history_score)))
View(credit_history_scoreCard)
  ##5.2.7 savings_score
savings_scoreCard<-cbind(as.matrix(c("Savings","","","",""),nrow=5,ncol=1),unique(cbind(savings,savings_score)))
View(savings_scoreCard)
  ##5.2.8 property_score
property_scoreCard<-cbind(as.matrix(c("Property","","",""),nrow=4,ncol=1),unique(cbind(property,property_score)))
View(property_scoreCard)
  ##5.2.9 purpose_score
purpose_scoreCard<-cbind(as.matrix(c("Purpose","","","",""),,nrow=5,ncol=1),unique(cbind(purpose,purpose_score)))
View(purpose_scoreCard)
  ## The final score card
scoreCard_CSV<-rbind(m1,duration_scoreCard,amount_scoreCard,age_scoreCard,installment_rate_scoreCard,
                     status_scoreCard,credit_history_scoreCard,savings_scoreCard,property_scoreCard,purpose_scoreCard)
View(scoreCard_CSV)

###############6.Testing Score Card###############
credit_risk<-ifelse(test_kfolddata[,"credit_risk"]=="good",0,1)
tmp_test<-test_kfolddata[,-21]
data_tmp<-as.matrix(cbind(tmp_test,credit_risk))
#View(data_tmp)

#Reducing dimensions of purpose
for (i in 1:nrow(data_tmp))
{
  #Merging car (new) & car (used) 
  if(as.character(data_tmp[i,"purpose"])=="car (new)")
  {
    data_tmp[i,"purpose"]<-as.character("car(new/used)")
  }
  if(as.character(data_tmp[i,"purpose"])=="car (used)")
  {
    data_tmp[i,"purpose"]<-as.character("car(new/used)")
  }
  #Merging radio/television & furniture/equipment
  if(as.character(data_tmp[i,"purpose"])=="radio/television")
  {
    data_tmp[i,"purpose"]<-as.character("radio/television/furniture/equipment")
  }
  if(as.character(data_tmp[i,"purpose"])=="furniture/equipment")
  {
    data_tmp[i,"purpose"]<-as.character("radio/television/furniture/equipment")
  }
  #Merging others、repairs & business
  if(as.character(data_tmp[i,"purpose"])=="others")
  {
    data_tmp[i,"purpose"]<-as.character("others/repairs/business")
  }
  if(as.character(data_tmp[i,"purpose"])=="repairs")
  {
    data_tmp[i,"purpose"]<-as.character("others/repairs/business")
  }
  if(as.character(data_tmp[i,"purpose"])=="business")
  {
    data_tmp[i,"purpose"]<-as.character("others/repairs/business")
  }
  #Merging retraining & education
  if(as.character(data_tmp[i,"purpose"])=="retraining")
  {
    data_tmp[i,"purpose"]<-as.character("retraining/education")
  }
  if(as.character(data_tmp[i,"purpose"])=="education")
  {
    data_tmp[i,"purpose"]<-as.character("retraining/education")
  }
}

data<-as.data.frame(data_tmp)
#View(data_test)
summary(data)
tot<-nrow(data)
score<-list()

for (i in 1:nrow(data))
{
  lst<-as.matrix(data[i,])
  
#duration
  score_duration<-NA
  if(lst[,"duration"]<=11)
  {
    score_duration<-10
  }
else
  if(lst[,"duration"]>11 & lst[,"duration"]<=33)
  {
  score_duration<-0
  }
else
  if(lst[,"duration"]>33)
  {
  score_duration<--7
  }

#amount
  score_amount<-NA
  if(lst[,"amount"]<=1372)
  {
  score_amount<--2
  }
else
  if(lst[,"amount"]>1372 & lst[,"amount"]<=4042)
  {
  score_amount<-5
  }
else
  if(lst[,"amount"]>4042 & lst[,"amount"]<=8947)
 {
  score_amount<--5
 }
else
  if(lst[,"amount"]>8947 )
 {
  score_amount<--15
 }

#age
score_age<-NA
  if(lst[,"age"]<=25)
  {
  score_age<--5
  }
else
  if(lst[,"age"]>25)
  {
  score_age<-1
  }

#Installment_rate
score_installment_rate<-NA
if(lst[,"installment_rate"]==1 )
 { 
  score_installment_rate<-4
 }
else
if(lst[,"installment_rate"]==2 )
 {
  score_installment_rate<-4
 }
else
if(lst[,"installment_rate"]==3 )
 {
  score_installment_rate<-1
 }
else
if(lst[,"installment_rate"]==4 )
 {
  score_installment_rate<--2
}

#status
score_status<-NA
if(lst[,"status"]=="... < 100 DM" )
{ 
  score_status<--9
}
else
  if(lst[,"status"]=="0 <= ... < 200 DM" )
  {
    score_status<--5
  }
else
  if(lst[,"status"]=="... >= 200 DM / salary for at least 1 year" )
  {
    score_status<-7
  }
else
  if(lst[,"status"]=="no checking account" )
  {
    score_status<-13
  }

#Credit_history
score_credit_history<-NA
if(lst[,"credit_history"]=="existing credits paid back duly till now" )
  { 
  score_credit_history<--1
  }
else
  if(lst[,"credit_history"]=="critical account/other credits existing" )
  {
    score_credit_history<-8
  }
else
  if(lst[,"credit_history"]=="delay in paying off in the past" )
  {
    score_credit_history<-1
  }
else
  if(lst[,"credit_history"]=="no credits taken/all credits paid back duly" )
  {
    score_credit_history<--15
  }
else
  if(lst[,"credit_history"]=="all credits at this bank paid back duly" )
  {
    score_credit_history<--11
  }
#Savings
score_savings<-NA
if(lst[,"savings"]=="... < 100 DM" )
  { 
  score_savings<--3
  }
else
  if(lst[,"savings"]=="100 <= ... < 500 DM" )
  {
    score_savings<--2
  }
else
  if(lst[,"savings"]=="500 <= ... < 1000 DM" )
  {
    score_savings<-7
  }
else
  if(lst[,"savings"]=="... >= 1000 DM" )
  {
    score_savings<-12
  }
else
  if(lst[,"savings"]=="unknown/no savings account" )
  {
    score_savings<-8
  }

#Property
score_property<-NA
if(lst[,"property"]=="car or other" )
  { 
   score_property<-0
  }
else
  if(lst[,"property"]=="building society savings agreement/life insurance" )
  {
    score_property<-0
  }
else
  if(lst[,"property"]=="real estate" )
  {
    score_property<-4
  }
else
  if(lst[,"property"]=="unknown/no property" )
  {
    score_property<--5
  }

#Purpose
score_purpose<-NA
if(lst[,"purpose"]=="radio/television/furniture/equipment" )
{ 
    score_purpose<--2
}
else
  if(lst[,"purpose"]=="car(new/used)" )
  {
    score_purpose<--1
  }
else
  if(lst[,"purpose"]=="others/repairs/business" )
  {
    score_purpose<--1
  }
else
  if(lst[,"purpose"]=="domestic appliances" )
  {
    score_purpose<-5
  }
else
  if(lst[,"purpose"]=="retraining/education" )
  {
    score_purpose<--6
  }
score[i]<-sum(19,score_duration,score_amount,score_age,score_installment_rate,
              score_status,score_credit_history,score_savings,score_property,score_purpose)
rm(lst)
}

score_M<-as.matrix(score,ncol=1)
View(score_M)
score_m<-as.numeric(score_M)
typeof(score_m)
score_data<-cbind(data,score_M)
summary(score_data)
score_risk<-score_data[,c("credit_risk","score_M")]
View(score_risk)

score_M_TRANS<-c()

for (i in 1:length(score_M))
{
  if (score_M[i]<=21)
  {
    score_M_TRANS[i]<-1
  }
  if (score_M[i]>21)
  {
    score_M_TRANS[i]<-0
  }
}
score_M_TRANS<-as.matrix(score_M_TRANS)
typeof(score_M_TRANS)
credit_risk<-as.matrix(credit_risk)
typeof(credit_risk)

###############7.evalucation of the model###############
# Model evaluation based on confusion matrix
#trueType:true variable. The type of the data is 0 and 1. 
#predType:predict result. The type of the data is 0 and 1. 
#return-->list(...)

ppe<-function(trueType,predType)
{
  #build confusion matrix
  confusionMatrix<-as.matrix(table(trueType,predType))
  
  #get TP、FN、FP、TN
  TP<-confusionMatrix[2,2] #true positive
  FN<-confusionMatrix[2,1] #false negative
  FP<-confusionMatrix[1,2] #false positive
  TN<-confusionMatrix[1,1] #true negative
  
  
  #1.Accuracy
  e.A<-TP/(TP+FP)
  
  #2.Negtive Accuracy
  e.NA<-TN/(TN+FN)
  
  #3.Total Accuracy
  e.TA<-(TP+TN)/(TP+FN+FP+TN)
  
  #4.Error Rate
  e.ER<-FP/(TP+FP)
  
  #5.Negtive Error Rate
  e.NER<-FN/(FN+TN)
  
  #6.Total Error Rate
  e.TER<-1-e.TA
  
  #7.Coverage Rate
  e.CR<-TP/(TP+FN)
  
  #8.Negtive Coverage Rate
  e.NCR<-TN/(FP+TN)
  
  #9.FP Rate
  e.FPR<-FP/(FP+TN)
  
  #10.FN Rate
  e.FNR<-FN/(TP+FN)
  
  #11.F value
  e.F<-2*e.A*e.CR/(e.A+e.CR)
  
  #12.Lift Value
  e.LV<-e.A/((TP+FN)/(TP+FN+FP+TN))
  
  #13.correlation coefficient 
  e.phi<-(TP*TN-FP*FN)/sqrt((TP+FN)*(TN+FP)*(TP+FP)*(TN+FN))
  
  #14.Kappa
  pe<-((TP+FN)*(TP+FP)+(FP+TN)*(FN+TN))/(TP+FN+FP+TN)^2
  e.Kappa<-(e.TA-pe)/(1-pe)
  
  return(list(e.A=e.A,e.NA=e.NA,e.TA=e.TA,e.ER=e.ER,e.NER=e.NER,e.TER=e.TER,
              e.CR=e.CR,e.NCR=e.NCR,e.FPR=e.FPR,e.FNR=e.FNR,e.F=e.F,e.LV=e.LV,
              e.phi=e.phi,e.Kappa=e.Kappa))
}
ppe(credit_risk, score_M_TRANS)

#plot roc
#install.packages("gplots")
library(ROCR)
#score test data set
pred<-prediction(credit_risk,score_M_TRANS)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#calculate auc
auc.obj <- performance(pred,"auc")
auc <- auc.obj@y.values[[1]]
auc

#plot Kolmogorov–Smirnov(ks) curve 
#prob：probability of predicted result where the result is positive
#labels：the actual result classification 
#n:the number of parts of probability interval for bisection 

ks.plot<-function(prob,labels,n=100)
{
  L<-NROW(prob)
  if(n>L){n<-L}
  
  #build data.frame
  tmpdata<-data.frame(prob,labels)
  
  #In this sample, the high the score, the low probability of being the pisitive classification,
  #Hence, I sort the score in ascending order 
  tmpdata<-tmpdata[order(tmpdata$prob,decreasing=F),]
  tmpdata$rowno=1:L
  
  #Split the probability interval into n parts 
  qus<-quantile(1:L,probs=seq(0,1,1/n))
  culList1<-culList0<-NULL
  out<-mapply(function(i){
    sublab<-tmpdata[tmpdata$rowno>=1 & 
                      tmpdata$rowno<ifelse(i==n,qus[i+1]+0.001,qus[i+1]),]
    culList1<<-c(culList1,sum(sublab$labels==1))
    culList0<<-c(culList0,sum(sublab$labels==0)) 
  },1:n)
  culList1<-culList1/sum(labels==1)
  culList0<-culList0/sum(labels==0)
  
  #Plot KS curve
  plot(1:n,culList1,col='white',xlab=paste(n,"fractile",sep=""),ylab="cumulative percentage",
       xlim=c(0,n),ylim=c(0,1))
  lines(1:n,culList1,col='blue',lwd=2)
  lines(1:n,culList0,col='red',lwd=2)
  legend(0,1,legend=c("cumulative percentage-1","cumulative percentage-0"),col=c("blue","red"),lty=2)
  
  #calculate the value of KS and mark it in the graph
  ks.value<-max(culList1-culList0)
  x<-(1:n)[which.max(culList1-culList0)]
  abline(v=x,lty=2,col='gray',lwd=2)
  return(ks.value)
}

ks.plot(score_m, credit_risk)




