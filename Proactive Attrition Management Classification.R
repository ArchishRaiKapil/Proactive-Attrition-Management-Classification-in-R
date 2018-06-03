setwd("G:/Current/SAS/SASUniversityEdition/myfolders/MEGA Case/Logistic in R")
options(java.parameters = "- Xmx1024m")
DATA_SET<-read.csv("Proactive Attrition Management.csv",header=T)

# Identifying Outliers
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+2*s
  LC <- m-2*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

Num_Vars <- c("REVENUE",
              "MOU",
              "RECCHRGE",
              "DIRECTAS",
              "OVERAGE",
              "ROAM",
              "CHANGEM",
              "CHANGER",
              "DROPVCE",
              "BLCKVCE",
              "UNANSVCE",
              "CUSTCARE",
              "THREEWAY",
              "MOUREC",
              "OUTCALLS",
              "INCALLS",
              "PEAKVCE",
              "OPEAKVCE",
              "DROPBLK",
              "CALLWAIT",
              "MONTHS",
              "UNIQSUBS",
              "ACTVSUBS",
              "PHONES",
              "MODELS",
              "EQPDAYS",
              "AGE1",
              "AGE2",
              "SETPRC",
              "INCOME")

Cat_Vars <- c("CHILDREN",
              "CREDITA",
              "CREDITAA",
              "CREDITB",
              "CREDITC",
              "CREDITDE",
              "CREDITGY",
              "CREDITZ",
              "PRIZMRUR",
              "PRIZMUB",
              "PRIZMTWN",
              "REFURB",
              "WEBCAP",
              "TRUCK",
              "RV",
              "OCCPROF",
              "OCCCLER",
              "OCCCRFT",
              "OCCSTUD",
              "OCCHMKR",
              "OCCRET",
              "OCCSELF",
              "OWNRENT",
              "MARRYUN",
              "MARRYYES",
              "MARRYNO",
              "MAILORD",
              "MAILRES",
              "MAILFLAG",
              "TRAVEL",
              "PCOWN",
              "CREDITCD",
              "RETCALLS",
              "RETACCPT",
              "NEWCELLY",
              "NEWCELLN",
              "MCYCLE",
              "SETPRCM",
              "RETCALL")
              
              

# Outlier              
Step1 <- DATA_SET
Outliers<-t(data.frame(apply(DATA_SET[Num_Vars], 2, mystats)))
View(Outliers)
Step1$REVENUE[Step1$REVENUE>135.39]<-135.39
Step1$MOU[Step1$MOU>1580.25]<-1580.25
Step1$RECCHRGE[Step1$RECCHRGE>85]<-85
Step1$DIRECTAS[Step1$DIRECTAS>4.21]<-4.21
Step1$OVERAGE[Step1$OVERAGE>190.375]<-190.375
Step1$ROAM[Step1$ROAM>5.09]<-5.09
Step1$CHANGEM[Step1$CHANGEM>345.25]<-345.25
Step1$CHANGER[Step1$CHANGER>46.218]<-46.218
Step1$DROPVCE[Step1$DROPVCE>22]<-22
Step1$BLCKVCE[Step1$BLCKVCE>17.33]<-17.33
Step1$UNANSVCE[Step1$UNANSVCE>97.67]<-97.67
Step1$CUSTCARE[Step1$CUSTCARE>9.33]<-9.33
Step1$THREEWAY[Step1$THREEWAY>1.33]<-1.33
Step1$MOUREC[Step1$MOUREC>440.938]<-440.938
Step1$OUTCALLS[Step1$OUTCALLS>90.33]<-90.33
Step1$INCALLS[Step1$INCALLS>35.67]<-35.67
Step1$PEAKVCE[Step1$PEAKVCE>279.67]<-279.67
Step1$OPEAKVCE[Step1$OPEAKVCE>242]<-242
Step1$DROPBLK[Step1$DROPBLK>35.33]<-35.33
Step1$CALLFWDV[Step1$CALLFWDV>0]<-0
Step1$CALLWAIT[Step1$CALLWAIT>8.67]<-8.67
Step1$MONTHS[Step1$MONTHS>37]<-37
Step1$UNIQSUBS[Step1$UNIQSUBS>3]<-3
Step1$ACTVSUBS[Step1$ACTVSUBS>2]<-2
Step1$PHONES[Step1$PHONES>4]<-4
Step1$MODELS[Step1$MODELS>3]<-3
Step1$EQPDAYS[Step1$EQPDAYS>865.75]<-865.75
Step1$AGE1[Step1$AGE1>62]<-62
Step1$AGE2[Step1$AGE2>62]<-62
Step1$REFER[Step1$REFER>0]<-0
Step1$CREDITAD[Step1$CREDITAD>0]<-0
Step1$SETPRC[Step1$SETPRC>149.99]<-149.99
Step1$INCOME[Step1$INCOME>9]<-9

Step1$REVENUE[Step1$REVENUE<33.64]<-33.64
Step1$MOU[Step1$MOU<158.25]<-158.25
Step1$RECCHRGE[Step1$RECCHRGE<30]<-30
Step1$DROPVCE[Step1$DROPVCE<0.67]<-0.67
Step1$UNANSVCE[Step1$UNANSVCE<5.33]<-5.33
Step1$MOUREC[Step1$MOUREC<8.43]<-8.43
Step1$OUTCALLS[Step1$OUTCALLS<3.33]<-3.33
Step1$PEAKVCE[Step1$PEAKVCE<23]<-23
Step1$OPEAKVCE[Step1$OPEAKVCE<11]<-11
Step1$DROPBLK[Step1$DROPBLK<1.67]<-1.67
Step1$MONTHS[Step1$MONTHS<11]<-11
Step1$EQPDAYS[Step1$EQPDAYS<204]<-204
Step1$INCOME[Step1$INCOME<1]<-1
Step1$CHANGEM[Step1$CHANGEM<(-83)]<--83
Step1$CHANGER[Step1$CHANGER<(-7.11)]<--7.11



#Missing Value Imputation
Missing <-t(data.frame(apply(Step1[Num_Vars], 2, mystats)))
View(Missing)
Step1$REVENUE[which(is.na(Step1$REVENUE))]<-58.0399296
Step1$MOU[which(is.na(Step1$MOU))]<-520.1496756
Step1$RECCHRGE[which(is.na(Step1$RECCHRGE))]<-47.720386
Step1$DIRECTAS[which(is.na(Step1$DIRECTAS))]<-0.7135794
Step1$OVERAGE[which(is.na(Step1$OVERAGE))]<-31.9998265
Step1$ROAM[which(is.na(Step1$ROAM))]<-0.5624138
Step1$CHANGEM[which(is.na(Step1$CHANGEM))]<-24.5555515
Step1$CHANGER[which(is.na(Step1$CHANGER))]<-2.8449579
Step1$DROPVCE[which(is.na(Step1$DROPVCE))]<-5.4726855
Step1$BLCKVCE[which(is.na(Step1$BLCKVCE))]<-3.0565275
Step1$UNANSVCE[which(is.na(Step1$UNANSVCE))]<-26.5413711
Step1$CUSTCARE[which(is.na(Step1$CUSTCARE))]<-1.4530721
Step1$THREEWAY[which(is.na(Step1$THREEWAY))]<-0.1925432
Step1$MOUREC[which(is.na(Step1$MOUREC))]<-106.1318788
Step1$OUTCALLS[which(is.na(Step1$OUTCALLS))]<-23.6603227
Step1$INCALLS[which(is.na(Step1$INCALLS))]<-6.8147366
Step1$PEAKVCE[which(is.na(Step1$PEAKVCE))]<-87.3940152
Step1$OPEAKVCE[which(is.na(Step1$OPEAKVCE))]<-63.3955969
Step1$DROPBLK[which(is.na(Step1$DROPBLK))]<-9.2285203
Step1$CALLFWDV[which(is.na(Step1$CALLFWDV))]<-0
Step1$CALLWAIT[which(is.na(Step1$CALLWAIT))]<-1.3419432
Step1$MONTHS[which(is.na(Step1$MONTHS))]<-18.9539882
Step1$UNIQSUBS[which(is.na(Step1$UNIQSUBS))]<-1.4701395
Step1$ACTVSUBS[which(is.na(Step1$ACTVSUBS))]<-1.2865709
Step1$PHONES[which(is.na(Step1$PHONES))]<-1.7090054
Step1$MODELS[which(is.na(Step1$MODELS))]<-1.4980857
Step1$EQPDAYS[which(is.na(Step1$EQPDAYS))]<-394.6728282
Step1$AGE1[which(is.na(Step1$AGE1))]<-30.9942553
Step1$AGE2[which(is.na(Step1$AGE2))]<-20.7568729
Step1$REFER[which(is.na(Step1$REFER))]<-0
Step1$CREDITAD[which(is.na(Step1$CREDITAD))]<-0
Step1$SETPRC[which(is.na(Step1$SETPRC))]<-34.1835971
Step1$INCOME[which(is.na(Step1$INCOME))]<-4.5840641


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

categoricals_mode<-t(data.frame(apply(Step1[Cat_Vars], 2, Mode)))
View(categoricals_mode)
Step1$CHILDREN[which(is.na(Step1$CHILDREN))]<-0
Step1$CREDITA[which(is.na(Step1$CREDITA))]<-0
Step1$CREDITAA[which(is.na(Step1$CREDITAA))]<-0
Step1$CREDITB[which(is.na(Step1$CREDITB))]<-0
Step1$CREDITC[which(is.na(Step1$CREDITC))]<-0
Step1$CREDITDE[which(is.na(Step1$CREDITDE))]<-0
Step1$CREDITGY[which(is.na(Step1$CREDITGY))]<-0
Step1$CREDITZ[which(is.na(Step1$CREDITZ))]<-0
Step1$PRIZMRUR[which(is.na(Step1$PRIZMRUR))]<-0
Step1$PRIZMUB[which(is.na(Step1$PRIZMUB))]<-0
Step1$PRIZMTWN[which(is.na(Step1$PRIZMTWN))]<-0
Step1$REFURB[which(is.na(Step1$REFURB))]<-0
Step1$WEBCAP[which(is.na(Step1$WEBCAP))]<-1
Step1$TRUCK[which(is.na(Step1$TRUCK))]<-0
Step1$RV[which(is.na(Step1$RV))]<-0
Step1$OCCPROF[which(is.na(Step1$OCCPROF))]<-0
Step1$OCCCLER[which(is.na(Step1$OCCCLER))]<-0
Step1$OCCCRFT[which(is.na(Step1$OCCCRFT))]<-0
Step1$OCCSTUD[which(is.na(Step1$OCCSTUD))]<-0
Step1$OCCHMKR[which(is.na(Step1$OCCHMKR))]<-0
Step1$OCCRET[which(is.na(Step1$OCCRET))]<-0
Step1$OCCSELF[which(is.na(Step1$OCCSELF))]<-0
Step1$OWNRENT[which(is.na(Step1$OWNRENT))]<-0
Step1$MARRYUN[which(is.na(Step1$MARRYUN))]<-0
Step1$MARRYYES[which(is.na(Step1$MARRYYES))]<-0
Step1$MARRYNO[which(is.na(Step1$MARRYNO))]<-0
Step1$MAILORD[which(is.na(Step1$MAILORD))]<-0
Step1$MAILRES[which(is.na(Step1$MAILRES))]<-0
Step1$MAILFLAG[which(is.na(Step1$MAILFLAG))]<-0
Step1$TRAVEL[which(is.na(Step1$TRAVEL))]<-0
Step1$PCOWN[which(is.na(Step1$PCOWN))]<-0
Step1$CREDITCD[which(is.na(Step1$CREDITCD))]<-1
Step1$RETCALLS[which(is.na(Step1$RETCALLS))]<-0
Step1$RETACCPT[which(is.na(Step1$RETACCPT))]<-0
Step1$NEWCELLY[which(is.na(Step1$NEWCELLY))]<-0
Step1$NEWCELLN[which(is.na(Step1$NEWCELLN))]<-0
Step1$MCYCLE[which(is.na(Step1$MCYCLE))]<-0
Step1$SETPRCM[which(is.na(Step1$SETPRCM))]<-1
Step1$RETCALL[which(is.na(Step1$RETCALL))]<-0

#Creating Dummy Variables
Step2 <- Step1
Step2$CHILDREN<- factor(Step2$CHILDREN)
Step2$CREDITA<- factor(Step2$CREDITA)
Step2$CREDITAA<- factor(Step2$CREDITAA)
Step2$CREDITB<- factor(Step2$CREDITB)
Step2$CREDITC<- factor(Step2$CREDITC)
Step2$CREDITDE<- factor(Step2$CREDITDE)
Step2$CREDITGY<- factor(Step2$CREDITGY)
Step2$CREDITZ<- factor(Step2$CREDITZ)
Step2$PRIZMRUR<- factor(Step2$PRIZMRUR)
Step2$PRIZMUB<- factor(Step2$PRIZMUB)
Step2$PRIZMTWN<- factor(Step2$PRIZMTWN)
Step2$REFURB<- factor(Step2$REFURB)
Step2$WEBCAP<- factor(Step2$WEBCAP)
Step2$TRUCK<- factor(Step2$TRUCK)
Step2$RV<- factor(Step2$RV)
Step2$OCCPROF<- factor(Step2$OCCPROF)
Step2$OCCCLER<- factor(Step2$OCCCLER)
Step2$OCCCRFT<- factor(Step2$OCCCRFT)
Step2$OCCSTUD<- factor(Step2$OCCSTUD)
Step2$OCCHMKR<- factor(Step2$OCCHMKR)
Step2$OCCRET<- factor(Step2$OCCRET)
Step2$OCCSELF<- factor(Step2$OCCSELF)
Step2$OWNRENT<- factor(Step2$OWNRENT)
Step2$MARRYUN<- factor(Step2$MARRYUN)
Step2$MARRYYES<- factor(Step2$MARRYYES)
Step2$MARRYNO<- factor(Step2$MARRYNO)
Step2$MAILORD<- factor(Step2$MAILORD)
Step2$MAILRES<- factor(Step2$MAILRES)
Step2$MAILFLAG<- factor(Step2$MAILFLAG)
Step2$TRAVEL<- factor(Step2$TRAVEL)
Step2$PCOWN<- factor(Step2$PCOWN)
Step2$CREDITCD<- factor(Step2$CREDITCD)
Step2$RETCALLS<- factor(Step2$RETCALLS)
Step2$RETACCPT<- factor(Step2$RETACCPT)
Step2$NEWCELLY<- factor(Step2$NEWCELLY)
Step2$NEWCELLN<- factor(Step2$NEWCELLN)
Step2$MCYCLE<- factor(Step2$MCYCLE)
Step2$SETPRCM<- factor(Step2$SETPRCM)
Step2$RETCALL<- factor(Step2$RETCALL)


# Variable Reduction (Factor Analysis)
Step_nums <- Step2[Num_Vars]
corrm<- cor(Step_nums)    
eigen(corrm)$values

require(dplyr)

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))

write.csv(eigen_values, "EigenValues.csv")

require(psych)
FA<-fa(r=corrm, 9 , rotate="varimax", fm="ml")  
#SORTING THE LOADINGS
FA_SORT<-fa.sort(FA)                                         
FA_SORT$loadings
Loadings<-data.frame(FA_SORT$loadings[1:ncol(Step_nums),])
write.csv(Loadings, "loadings.csv")


# Variable Reduction (Binary Categorical Variables-Chi Square)
cor.test(Step2$CHURN,Step1$CHILDREN)
cor.test(Step2$CHURN,Step1$CREDITA)
cor.test(Step2$CHURN,Step1$CREDITAA)
cor.test(Step2$CHURN,Step1$CREDITB)
cor.test(Step2$CHURN,Step1$CREDITC)
cor.test(Step2$CHURN,Step1$CREDITDE)
cor.test(Step2$CHURN,Step1$CREDITGY)
cor.test(Step2$CHURN,Step1$CREDITZ)
cor.test(Step2$CHURN,Step1$PRIZMRUR)
cor.test(Step2$CHURN,Step1$PRIZMUB)
cor.test(Step2$CHURN,Step1$PRIZMTWN)
cor.test(Step2$CHURN,Step1$REFURB)
cor.test(Step2$CHURN,Step1$WEBCAP)
cor.test(Step2$CHURN,Step1$TRUCK)
cor.test(Step2$CHURN,Step1$RV)
cor.test(Step2$CHURN,Step1$OCCPROF)
cor.test(Step2$CHURN,Step1$OCCCLER)
cor.test(Step2$CHURN,Step1$OCCCRFT)
cor.test(Step2$CHURN,Step1$OCCSTUD)
cor.test(Step2$CHURN,Step1$OCCHMKR)
cor.test(Step2$CHURN,Step1$OCCRET)
cor.test(Step2$CHURN,Step1$OCCSELF)
cor.test(Step2$CHURN,Step1$OWNRENT)
cor.test(Step2$CHURN,Step1$MARRYUN)
cor.test(Step2$CHURN,Step1$MARRYYES)
cor.test(Step2$CHURN,Step1$MARRYNO)
cor.test(Step2$CHURN,Step1$MAILORD)
cor.test(Step2$CHURN,Step1$MAILRES)
cor.test(Step2$CHURN,Step1$MAILFLAG)
cor.test(Step2$CHURN,Step1$TRAVEL)
cor.test(Step2$CHURN,Step1$PCOWN)
cor.test(Step2$CHURN,Step1$CREDITCD)
cor.test(Step2$CHURN,Step1$RETCALLS)
cor.test(Step2$CHURN,Step1$RETACCPT)
cor.test(Step2$CHURN,Step1$NEWCELLY)
cor.test(Step2$CHURN,Step1$NEWCELLN)
cor.test(Step2$CHURN,Step1$MCYCLE)
cor.test(Step2$CHURN,Step1$SETPRCM)
cor.test(Step2$CHURN,Step1$RETCALL)

#Splitting data into Training AND Testing Dataset
# calibrat 1=training 0=testing
training<-Step2[Step2$CALIBRAT=='1',]
testing<-Step2[Step2$CALIBRAT=='0',]

# Regression
fit<-glm(CHURN~
           CREDITA+
           CREDITAA+
           CREDITB+
           CREDITC+
           CREDITCD+
           CREDITDE+
           MAILORD+
           MAILRES+
           MARRYNO+
           MARRYUN+
           NEWCELLY+
           OCCRET+
           OWNRENT+
           PRIZMRUR+
           PRIZMTWN+
           PRIZMUB+
           REFURB+
           RETCALL+
           SETPRCM+
           WEBCAP+
           CHILDREN+
           OUTCALLS+
           CUSTCARE+
           PHONES+
           REVENUE+
           DIRECTAS+
           INCOME+
           ACTVSUBS+
           RETCALLS+
           MOU+
           RECCHRGE+
           OVERAGE+
           CHANGEM+
           ROAM+
           AGE1+
           DROPBLK+
           THREEWAY+
           MONTHS+
           EQPDAYS,
         data = training,
         family = binomial(logit))

summary(fit)
coeff<-fit$coef

source("Concordance.R")
Concordance(fit)

#Stepwise regression
step1=step(fit)

fit2<-glm(CHURN~
            CUSTCARE+
          PHONES+
          ACTVSUBS+
          RETCALLS+
          MOU+
          RECCHRGE+
          OVERAGE+
          CHANGEM+
          ROAM+
          AGE1+
          DROPBLK+
          THREEWAY+
          MONTHS+
          EQPDAYS,
          data=training,
         family = binomial(logit))

Concordance(fit2)


################################VALIDATION ##############################
# Decile Scoring for Training dataset

train1<- cbind(training, Prob=predict(fit2, type="response")) 
View(train1)

##Creating Deciles
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)
require(dplyr)
train1$decile<-factor(train1$decile)
decile_grp<-group_by(train1,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=Prob), max_prob=max(Prob), default_cnt=sum(CHURN), 
                             non_default_cnt=total_cnt -default_cnt )
decile_summ_train<-arrange(decile_summ_train, desc(decile))
View(decile_summ_train)

write.csv(decile_summ_train,"fit_train_DA1.csv",row.names = F)

###########################################

# Decile Scoring for Testing dataset
test1<- cbind(testing, Prob=predict(fit2,testing, type="response")) 
View(test1)

##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
names(test1)
#Decile Analysis Reports
require(sqldf)

fit_test_DA <- sqldf("select decile, count(decile) as count, min(Prob) as Min_prob
                     , max(Prob) as max_prob 
                     , sum(CHURN) as default_cnt,
                       (count(decile)-sum(CHURN)) as Non_default_Count
                     from test1
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA,"fit_test_DA1.csv",row.names = F)


