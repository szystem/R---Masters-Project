#------uploading the data, changing countries with a wrong name---------
dataALL=rbind(read.csv("AB.csv"),read.csv("CE.csv"),read.csv("FH.csv"),read.csv("IK.csv"),read.csv("LM.csv"),read.csv("NQ.csv"),read.csv("RS.csv"),read.csv("TZ.csv"),to.data.frame=TRUE)
colnames(dataALL)[1]<-"Country"
dataALL$Country=as.factor(dataALL$Country)
dataALL=subset(dataALL,Country %in% setdiff(levels(dataALL$Country)[71:302],c("TRUE","footnoteSeqID")))
dataALL$Country=as.character(dataALL$Country)
dataALL = dataALL %>% mutate(Country = replace(Country, Country=="Côte d'Ivoire", "Cote d Ivore"))
dataALL = dataALL %>% mutate(Country = replace(Country, Country=="Curaçao", "Curacao"))
dataALL = dataALL %>% mutate(Country = replace(Country, Country=="Saint Barthélemy", "Saint Barthelemy "))
for(i in unique(dataALL$Country))
  for(j in unique(filter(dataALL,Country==i)$Year))
    if(setequal(unique(filter(dataALL,Country==i,Year==j)$Marital.status),c("Total"))) dataALL=setdiff(dataALL,filter(dataALL,Country==i,Year==j))

#-----------------counting rows in the most recent censuses in every country--------

dataALLtest=data.frame(x1=c('A','A','A','A','B','B','C','C','C'),x2=c(2004,1995,2004,1998,1997,2000,2001,2012,2012))
maxage=data.frame(Country=NA,MaxAge=NA,MaxAgeRows=NA)
for(i in unique(dataALLtest$x1)){
  MA=max(filter(dataALLtest,x1==i)$x2)
  maxage=rbind(maxage,
               c(i,MA,length(filter(dataALLtest,x1==i,x2==MA)$x1)))
}
   
dataALL$Year=as.numeric(dataALL$Year)
maxage=data.frame(Country=NA,MaxAge=NA,MaxAgeRows=NA)
for(i in unique(dataALL$Country)){
  MA=max(filter(dataALL,Country==i)$Year)
  maxage=rbind(maxage,
               c(i,MA,length(filter(dataALL,Country==i,Year==MA)$Country)))
}
write.csv(maxage,"maxage.csv", row.names=TRUE)
write.csv(dataALL,"dataALL.csv", row.names=TRUE)


#-------------REMOVING CENSUSES WITHOUT MARITAL STATUSES------
dataALLtest=data.frame(Country=c('A','A','A','A','B','B','C','C','C','C'),Year=c(2004,2004,1995,1995,1997,2000,2001,2001,2012,2012),Marital.status=c('Total','Total','Total','Single','Total','Single','Total','Total','Total','Single'))
dataALLtest

for(i in unique(dataALL$Country))
  for(j in unique(filter(dataALL,Country==i)$Year))
    if(setequal(unique(filter(dataALL,Country==i,Year==j)$Marital.status),c("Total"))) dataALL=setdiff(dataALL,filter(dataALL,Country==i,Year==j))

rv=c('14-Oct',
     '20-Nov',
     '14-Dec',
     '4-Jan',
     '9-May',
     '19-Oct',
     '14-Jun',
     '19-Dec')

rv2=c('10-14',
      '11-20',
      '12-14',
      '1-4',
      '5-9',
      '10-19',
      '6-14',
      '12-19')

for(i in 1:8) dataALL$Age[dataALL$Age==rv[i]]=rv2[i]

#-----------A PLOT OF CENSUSES IN TIME---------
plot(x = 1,                 
     xlab = "X Label", 
     ylab = "Y Label",
     xlim = c(0, 250), 
     ylim = c(1940, 2020),
     main = "Blank Plotting Canvas",
     type = "n")
no=1
for(i in unique(dataALL$Country)){
  points(x=no,y=min(filter(dataALL,Country==i)$Year),col="blue",pch=16)
  points(x=no,y=max(filter(dataALL,Country==i)$Year),col="red",pch=16)

  no=no+1
}

#---------A SHEET OF NUMBER OF CELLS IN ALL CENSUSES IN ALL COUNTRIES------
min(dataALL$Year)=1948
max(dataALL$Year)=2019
bigtable=data.frame(Country=rep('',times=230))

for(i in 1948:2019) {                                   
  new <- rep(' -', nrow(bigtable))                      
  bigtable[ , ncol(bigtable) + 1] <- new                 
  colnames(bigtable)[ncol(bigtable)] <- i 
}
count=1
for(i in unique(dataALL$Country)){
  bigtable[count,1]=i
  for(j in unique(filter(dataALL,Country==i)$Year)){
    # if(length(filter(dataALL,Country==i,Year==j)$country)!=0){
      # print("in")
      if(!prod(filter(dataALL,Country==i,Year==j)$Age %in% c("Total","Unknown"))) {
        bigtable[count,j-1946]=1
      }
      else {
        if(!prod(filter(dataALL,Country==i,Year==j)$Area %in% c("Total")) | !prod(filter(dataALL,Country==i,Year==j)$Sex %in% c("Both Sexes")))  bigtable[count,j-1946]=2
        else  bigtable[count,j-1946]=3
      # }
    }
  }
  count=count+1
}
write.table(bigtable,file="bigtable.csv")
write.table(bigtable, file = "yeartable.csv", sep = ",", col.names = NA,
            qmethod = "double")
unique(dataALL$Age)

###-------------A SHEET OF THE BEST CENSUSES----------------
newtable=data.frame(Country=rep('',times=230))

for(i in 1948:2019) {                             # Head of for-loop
  new <- rep(' -', nrow(newtable))                # Create new column
  newtable[ , ncol(newtable) + 1] <- new                 
  colnames(newtable)[ncol(newtable)] <- i # Rename column name
}

count=1
for(i in unique(dataALL$Country)){
  bigtable[count,1]=i
  for(j in unique(filter(dataALL,Country==i)$Year)){
    # if(length(filter(dataALL,Country==i,Year==j)$country)!=0){
    # print("in")
    #CZY SA GRUPY WIEKOWE?
    if(!prod(filter(dataALL,Country==i,Year==j)$Age %in% c("Total","Unknown"))) {
      #CZY W GRUPACH SA PLCI
      if(length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Sex=='Male')$Country)!=0 && length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Sex=='Female')$Country!=0)){
        #CZY W GRUPACH I PLCIACH SA LOKACJE
        if(length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Sex=='Male',Area!='Total')$Country)!=0 && length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Sex=='Female',Area!='Total')$Country!=0))
          newtable[count,j-1946]=1
        else
          newtable[count,j-1946]=2
      }
      else
        if(length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Area!='Total')$Country)!=0) newtable[count,j-1946]=3
        else newtable[count,j-1946]=4
    }
  }
  count=count+1
  cat(count,' ')
}
write.table(newtable, file = "newyeartable.csv", sep = ",", col.names = NA,
            qmethod = "double")

###-------------PLOT WITH THE BEST YEAR VISIBLE----------------
R=NA
Rsq=NA
rokdataALL=filter(dataALL,Marital.status!='Total')
for(i in unique(rokdataALL$Year)){
  R[i-1947]=Rsq[i-1947]=0
  for(j in unique(rokdataALL$Country)){
    mini=min(abs(unique(filter(rokdataALL,Country==j)$Year)-i))
    R[i-1947]=R[i-1947]+mini
    Rsq[i-1947]=Rsq[i-1947]+mini^2
  }
  cat(i-1947,' ')
}
plot((1:length(R))+1947,R)
plot((1:length(R^2))+1947,R^2)

#------------SELECTED CENSUSES--------------------------
censusselection=read.csv("selectedcensuses.csv")
censusselection
censusselection[3,3]
dim(dataALL)
dataSELECT=filter(dataALL,Country==0)
for(i in 1:230)
  for(j in 1:(2019-1979+1)){
    cat(i,j,' ')
    if(censusselection[i,(j+2)]=="y" && !is.na(censusselection[i,(j+2)])){
      dataSELECT=rbind(dataSELECT,filter(dataALL,Year==j+1978,Country==unique(dataALL$Country)[i]))
    }
  }
dataSELECT
censusselection[1,(42+2)]
write.csv(dataSELECT,'selectedcensuses.csv',row.names = TRUE)
unique(dataSELECT$Marital.status)


###----END OF BASIC PREPARATIONS, NEW MORE SPECIFIC FILE IS CREATED-----
newtable2=data.frame(Country=rep('',times=230))

for(i in 1948:2019) {                             # Head of for-loop
  new <- rep(' -', nrow(newtable2))                # Create new column
  newtable2[ , ncol(newtable2) + 1] <- new                 
  colnames(newtable2)[ncol(newtable2)] <- i # Rename column name
}

count=1
for(i in unique(dataALL$Country)){
  bigtable[count,1]=i
  for(j in unique(filter(dataALL,Country==i)$Year)){
    # if(length(filter(dataALL,Country==i,Year==j)$country)!=0){
    # print("in")
    #ARE THERE DIFFERENT AGE GROUPS
    if(!prod(filter(dataALL,Country==i,Year==j)$Age %in% c("Total","Unknown",'15 +', '14 +', '12 +' ,'45 +', '13 +'))) {
      #ARE THERE SEPARATE SEXES WITHIN DIFFERENT AGE GROUPS
      if(length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Sex=='Male')$Country)!=0 && length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Sex=='Female')$Country!=0)){
        #ARE THERE AREAS
        if(length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Sex=='Male',Area!='Total')$Country)!=0 && length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Sex=='Female',Area!='Total')$Country!=0))
          newtable2[count,j-1946]=1
        else
          newtable2[count,j-1946]=2
      }
      else
        if(length(filter(dataALL,Country==i,Year==j,!Age %in% c('Total','Unknown'),Area!='Total')$Country)!=0) newtable2[count,j-1946]=3
        else newtable2[count,j-1946]=4
    }
  }
  count=count+1
  cat(count,' ')
}

write.table(newtable2, file = "newyeartable2.csv", sep = ",", col.names = NA,
            qmethod = "double")

#--SELECTED CENSUSES 2----
censusselection2=read.csv("selectedcensuses2.csv")
dataSELECT2=filter(dataALL,Country==0)
for(i in 1:230)
  for(j in 1:(2019-1979+1)){
    cat(i,j,' ')
    if(censusselection2[i,(j+2)]=="y" && !is.na(censusselection2[i,(j+2)])){
      dataSELECT2=rbind(dataSELECT2,filter(dataALL,Year==j+1978,Country==unique(dataALL$Country)[i]))
    }
  }


#-----------changes ude to insufficient categories--------------
#change Luxemburg census to 2001
#change Mauritania census to 1977
dataSELECT2=setdiff(dataSELECT2,filter(dataSELECT2,Country%in%c('Luxembourg','Mauritania')))
dataSELECT2=rbind(dataSELECT2,filter(dataALL,Country=='Luxembourg',Year==2001))
dataSELECT2=rbind(dataSELECT2,filter(dataALL,Country=='Mauritania',Year==1977))

#Saint Barthelemy,Saint-Martin (French part),Papua New Guinea deleted due to insufficient number of categories
dataSELECT2=setdiff(dataSELECT2,filter(dataSELECT2,Country%in%c('Saint Barthelemy','Saint-Martin (French part)','Papua New Guinea')))

#deleting unknown zeros
dataSELECT2=setdiff(dataSELECT2,filter(dataSELECT2,Marital.status=='Unknown',Value==0))

#more than one census' source
for(i in unique(dataSELECT2$Country))
  if(length(unique(select(filter(dataSELECT2,Country==i),Record.Type))[[1]])!=1) print(i)

# "Australia"
# delete Estimate - de jure
# [1] "Canada"
# delete Estimate - de jure
# [1] "Colombia"
# delete Census - de facto - complete tabulation
# [1] "Czechia"
# delete Estimate - de jure
# [1] "Germany"
# delete Estimate - de jure
# [1] "Latvia"
# delete Estimate - de facto
# [1] "Netherlands"
# delete Estimate - de jure
# [1] "Norway"
# delete Estimate - de jure
# [1] "Slovenia"
# delete Estimate - de jure
# [1] "Switzerland"
# delete Estimate - de jure
for(i in 1:10){
  print(nrow(filter(dataSELECT2,Country==
                      c("Australia","Canada","Colombia","Czechia","Germany","Latvia","Netherlands","Norway","Slovenia","Switzerland")[i],
                    Record.Type==
                      c('Estimate - de jure','Estimate - de jure','Census - de facto - complete tabulation','Estimate - de jure','Estimate - de jure','Estimate - de facto','Estimate - de jure','Estimate - de jure','Estimate - de jure','Estimate - de jure')[i]
  )))
  dataSELECT2=setdiff(dataSELECT2,filter(dataSELECT2,Country==
                                           c("Australia","Canada","Colombia","Czechia","Germany","Latvia","Netherlands","Norway","Slovenia","Switzerland")[i],
                                         Record.Type==
                                           c('Estimate - de jure','Estimate - de jure','Census - de facto - complete tabulation','Estimate - de jure','Estimate - de jure','Estimate - de facto','Estimate - de jure','Estimate - de jure','Estimate - de jure','Estimate - de jure')[i]
  ))
}


#more than one reliability
for(i in unique(dataSELECT2$Country))
  if(length(unique(select(filter(dataSELECT2,Country==i),Reliability))[[1]])!=1) print(i)

#show list of categories for selected censuses
unique(dataSELECT2$Marital.status)
categories2=data.frame(Country=unique(dataSELECT2$Country))

#option 1: deleting 17 countries and creating category divorced
#option 2: deleting 17 countries and creating categories divorced and separated, setting sep=0 for app 50% of countries
#option 3: counting separated and divorced together
#treating UK "M OR SEP" as separated
#errors in dataset: luxemburg population for 2000 is 8 mln

categories2[1,12]=''
catSET=c("Single (never married)",          
         "Married"
         ,"Married or in consensual union"
         ,"In consensual union"
         ,"Married or married but separated"
         ,"Married but separated"
         ,"Divorced or separated"
         ,"Divorced and not remarried"
         ,"Widowed and not remarried" 
         ,"Widowed or divorced"
         ,"Unknown") 
k=1
for(i in unique(dataSELECT2$Country)){
  for(j in 1:11)
    if(nrow(filter(dataSELECT2,Country==i,Marital.status==catSET[j]))!=0)
      categories2[k,j+1]="X"
  k=k+1
}
write.csv(categories2,"categories2.csv")

#delete Saint Barthelemy
dataSELECT2=setdiff(dataSELECT2,filter(dataSELECT2,Country%in%c('Saint Barthelemy')))

#delete Cayman Islands
dataSELECT2=setdiff(dataSELECT2,filter(dataSELECT2,Country%in%c('Cayman Islands')))

#delete Liechtenstein source year 2005
dataSELECT2=setdiff(dataSELECT2,filter(dataSELECT2,Country%in%c('Liechtenstein'),Source.Year==2005))

#change Northern Mariana Islands to 1990
dataSELECT2=setdiff(dataSELECT2,filter(dataSELECT2,Country%in%c('Northern Mariana Islands')))
dataSELECT2=rbind(dataSELECT2,filter(dataALL,Country=='Northern Mariana Islands',Year==1990))

#delete Sweden source year 2004
dataSELECT2=setdiff(dataSELECT2,filter(dataSELECT2,Country=='Sweden',Source.Year==2004))


#-------------CREATING 'OPTION 3' DATA SET-----------------
opt3=dataSELECT2
predata3=data.frame(no='',Country='',Area='',Sex='',Age='',c1='',c2='',c3='',c4='',c5='',c6='',c7='',c8='',c9='',c10='',c11='',S='')[FALSE,]


catSET3=c("Single (never married)",          
         "Married"
         ,"Married or in consensual union"
         ,"In consensual union"
         ,"Married or married but separated"
         ,"Married but separated"
         ,"Divorced or separated"
         ,"Divorced and not remarried"
         ,"Widowed and not remarried" 
         ,"Widowed or divorced"
         ,"Unknown"
         ,'Total') 
m=1
i=1
predata3[m,6:17]=0
predata3[1,]
predata3[m,2]=opt3[i,1]
predata3[m,3:5]=opt3[i,3:5]
predata3[m,5+match(opt3[i,6],catSET3)]=opt3[i,10]
predata3[m,1]=m
for(i in 1:nrow(opt3)){
  if(predata3[m,2]==opt3[i,1] & predata3[m,3]==opt3[i,3] &predata3[m,4]==opt3[i,4] &predata3[m,5]==opt3[i,5]){
    predata3[m,5+match(opt3[i,6],catSET3)]=opt3[i,10]
  }
  else{
    m=m+1
    predata3[m,6:17]=0
    predata3[m,1]=m
    predata3[m,2]=opt3[i,1]
    predata3[m,3:5]=opt3[i,3:5]
    predata3[m,5+match(opt3[i,6],catSET3)]=opt3[i,10]
  }
}

predata3$c1=as.numeric(predata3$c1)
predata3$c2=as.numeric(predata3$c2)
predata3$c3=as.numeric(predata3$c3)
predata3$c4=as.numeric(predata3$c4)
predata3$c5=as.numeric(predata3$c5)
predata3$c6=as.numeric(predata3$c6)
predata3$c7=as.numeric(predata3$c7)
predata3$c8=as.numeric(predata3$c8)
predata3$c9=as.numeric(predata3$c9)
predata3$c10=as.numeric(predata3$c10)
predata3$c11=as.numeric(predata3$c11)
predata3$S=as.numeric(predata3$S)

predata3=mutate(predata3,S2=c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11) 

predata3=setdiff(predata3,filter(predata3,S2==0))

sumoutliers3=predata3[pmax((predata3$S/predata3$S2 < 1.05) * (predata3$S/predata3$S2 > 0.95),predata3$S==0)==0,]
sumoutliers3

#changes according to the outliers file 

predata3=setdiff(predata3,filter(predata3,no==5739))
predata3=setdiff(predata3,filter(predata3,no==12316))

sumoutliers3=predata3[pmax((predata3$S/predata3$S2 < 1.05) * (predata3$S/predata3$S2 > 0.95),predata3$S==0)==0,]
#c1=6, c8=13

#manural changes
predata3[12706+15,6]=36823
predata3[12720+15,6]=16
predata3[12720+15,13]=20
predata3[12724+15 ,6]=33517
predata3[12736+15 ,13]=672
predata3[12737+15 ,13]=253
predata3[12738+15 ,6]=373
predata3[12738+15 ,13]=99
predata3[12742+15 ,13]=37099
predata3[12756+15 ,6]=43
predata3[12756+15 ,13]=6
predata3[12760+15  ,6]=32600
predata3[12760+15  ,13]=6
predata3[12774+15  ,6]=294
predata3[12774+15  ,6]=35

# rm(list=setdiff(ls(), c("predata3",'dataSELECT2','dataALL')))
predata3=mutate(predata3,S2=c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11)
predata3=setdiff(predata3,filter(predata3,S2==0))
sumoutliers3=predata3[pmax((predata3$S/predata3$S2 < 1.05) * (predata3$S/predata3$S2 > 0.95),predata3$S==0)==0,]
sumoutliers3

#change of age to the averages of intervals
unique(predata3$Age)
agechange=c(
"0 - 14",7,
"15 - 19",17,
"20 - 24",22,
"25 - 29",27,
"30 - 34",32,
"35 - 39" ,37,
"40 - 44",42,
"45 - 49" ,47,
"50 - 54" ,52,
"55 - 59" ,57,
"60 - 64" ,62,
"65 +"    ,75,
"65 - 69" ,67,
"70 - 74",72,
"75 - 79",77,
"80 - 84" ,82,
"85 - 89" ,87,
"90 - 94" ,92,
"95 - 99",97,
"100 +",100,
"Unknown",'Unknown',
"15 +",'15 +',
"75 +",'80',
"0 - 10" ,5,
"20-Nov",15.5,
"21 - 30",20.5,
"31 - 40",35.5,
"41 - 50",45.5,
"51 - 60",55.5,
"61 +",75,
"14-Dec" ,13,
"95 +",95,
"14",7,
"50 +"  , 65, 
"16 - 19" ,17,
"16 +"    ,'16 +',
"85 +"    ,85,
"14-Oct"  ,12,
"10 +"   ,'10 +',
"70 +"   ,75,
"14 +"    ,'14 +',
"12 +"   ,"12 +"   ,
"60 +"   ,70, 
"55 +"   ,65,
"80 +"  ,85,
"0 - 15"  ,7.5,
"0 - 4"  ,2,
"9-May" ,7,
"90 +"   ,90,
"0 - 9"   ,4.5,
"65 - 74" ,69.5,
"25 - 39" ,32,
"40 - 54",47,
"55 - 64" ,59.5,
"65 - 79",72,
"25 - 34",29.5,
"35 - 44",39.5, 
"45 - 54",49.5,
"19-Oct"  ,14.5,
"75 - 84" ,79.5,
"0 - 19"  ,9.5,
"20 - 29" ,24.5,
"30 - 39", 34.5,
"40 - 49", 44.5,
"50 - 59", 54.5,
"0 - 11" ,5.5,
"19-Dec"  ,15.5,
"13 - 14" ,13.5,
"13 +",'13 +'
)
agechange1=agechange[(1:69)*2-1]
agechange2=agechange[(1:69)*2]

for(i in 1:length(agechange1)) predata3[predata3[,5]==agechange1[i],5]=agechange2[i]
unique(predata3$Age)

#reduction of rows
predata3=mutate(predata3,S2=c1+c2+c3+c4+c5+c6+c7+c8+c9+c10)
predata3=setdiff(predata3,filter(predata3,S2==0))

predata3=setdiff(predata3,filter(predata3,Age%in%c('Total','Unknown','15 +','16+','10+','14+','12 +','13+','Unknown')))
type1=0
type2=0
type3=0
for(i in unique(predata3$Country)){
  if('Male' %in% unique(filter(predata3,Country==i)$Sex)){
    type1=type1+1
    predata3=setdiff(predata3,filter(predata3,Country==i,Sex=='Both Sexes'))
    if('Urban' %in% unique(filter(predata3,Country==i,Sex=='Male')$Area)){
      predata3=setdiff(predata3,filter(predata3,Country==i,Area=='Total'))
      type2=type2+1
    }
      
  }
  else
    if('Urban' %in% unique(filter(predata3,Country==i,Sex=='Both Sexes')$Area)){
      predata3=setdiff(predata3,filter(predata3,Country==i,Area=='Total'))
      type3=type3+1
    }
}

#changing numbers to proportions and changing '0' proportions to a lower bound
predata3=mutate(predata3,C1=c1/S2,C2=(c2+c3+c4)/S2,C3=(c5+c6+c7+c8)/S2,C4=c9/S2)

for(i in 1:nrow(predata3)){
  BC=0
  if(predata3$C1[i]==0){
    BC=BC+1
    predata3$C1[i]=0.0000001
  }

  if(predata3$C2[i]==0){
    BC=BC+1 
    predata3$C2[i]=0.0000001
  } 
  if(predata3$C3[i]==0){
    BC=BC+1
    predata3$C3[i]=0.0000001
  }  
  if(predata3$C4[i]==0){
    BC=BC+1
    predata3$C4[i]=0.0000001
  }  
  max=max(predata3$C1[i],predata3$C2[i],predata3$C3[i],predata3$C4[i])
  if(predata3$C1[i]==max) predata3$C1[i]=predata3$C1[i]-0.0000001*BC
  else if(predata3$C2[i]==max) predata3$C2[i]=predata3$C2[i]-0.0000001*BC
  else if(predata3$C3[i]==max) predata3$C3[i]=predata3$C3[i]-0.0000001*BC
  else if(predata3$C4[i]==max) predata3$C4[i]=predata3$C4[i]-0.0000001*BC
}
max(predata3$C1+predata3$C2+predata3$C3+predata3$C4)
predata3[min(predata3$C1)==predata3$C1,]
#save
write.csv(predata3,'predata3.csv')
predata3=read.csv('predata3.csv')
DATA3=data.frame(no='',Country='',Area='',Sex='',Age='',C1='',C2='',C3='',C4='')[FALSE,]
DATA3=rbind(DATA3,select(predata3,no,Country,Area,Sex,Age,C1,C2,C3,C4))
DATA3[DATA3$Area=='Urban',]$Area=0
DATA3[DATA3$Area=='Rural',]$Area=1
DATA3[DATA3$Area=='Total',]$Area=0.5
DATA3[DATA3$Sex=='Male',]$Sex=0
DATA3[DATA3$Sex=='Female',]$Sex=1
DATA3[DATA3$Sex=='Both Sexes',]$Sex=0.5
DATA3$no=1:nrow(DATA3)

N=1
for(i in unique(DATA3$Country)){
  DATA3[DATA3$Country==i,]$Country=N
  N=N+1
}
tail(DATA3)
DATA3[1,1]+DATA3[2,1]
DATA32=DATA3
DATA3$no=as.numeric(DATA3$no)
DATA3$Country=as.numeric(DATA3$Country)
DATA3$Area=as.numeric(DATA3$Area)
DATA3$Sex=as.numeric(DATA3$Sex)
DATA3$Age=as.numeric(DATA3$Age)
#save
write.csv(DATA3,'DATA3.csv')
DATA3=read.csv('DATA3.csv')

#----animating married,divorced,widowed in time----
predata3=mutate(predata3,S2=c2+c3+c4+c5+c6+c7+c8+c9) 
predata3=predata32
predata3=setdiff(predata3,filter(predata3,S2==0))
for(i in unique(predata3$Country)){
  for(j in unique(filter(predata3,Country==i)$Age)){
    if(!nrow(filter(predata3,Country==i,Age==j,Area=='Total',Sex=='Both Sexes'))==1){
      cat('1')
      if(nrow(filter(predata3,Country==i,Age==j,Area=='Urban',Sex=='Both Sexes'))!=0){
        predata3=rbind(predata3,list(1,i,'Total','Both Sexes',j,0,0,0,0,0,0,0,0,0,0,0,0,0))
        cat('2')
        predata3[nrow(predata3),6:18]=apply(filter(predata3,Country==i,Age==j,Area %in%c('Urban','Rural'),Sex=='Both Sexes')[,6:18],2,sum)
        
      }
      else if(nrow(filter(predata3,Country==i,Age==j,Area=='Total',Sex=='Male'))!=0){
        predata3=rbind(predata3,list(1,i,'Total','Both Sexes',j,0,0,0,0,0,0,0,0,0,0,0,0,0))
        cat('3')
        predata3[nrow(predata3),6:18]=apply(filter(predata3,Country==i,Age==j,Area=='Total',Sex %in% c('Male','Female'))[,6:18],2,sum)
        
      }
      else if(nrow(filter(predata3,Country==i,Age==j,Area=='Urban',Sex=='Male'))!=0){
        predata3=rbind(predata3,list(1,i,'Total','Both Sexes',j,0,0,0,0,0,0,0,0,0,0,0,0,0))
        cat('4')
        predata3[nrow(predata3),6:18]=apply(filter(predata3,Country==i,Age==j,Area %in%c('Urban','Rural'),Sex %in% c('Male','Female'))[,6:18],2,sum)
        
      }
    }
    predata3=setdiff(predata3,setdiff(filter(predata3,Country==i,Age==j),filter(predata3,Country==i,Age==j,Area=='Total',Sex=='Both Sexes')))
    cat('5')
  }
}
nrow(filter(predata3,Country==i,Age==j,Area=='Total',Sex=='Male'))!=0
predata3=rbind(predata3,c(1,i,'Total','Both Sexes',j,
                          apply(filter(predata3,Country==i,Age==j,Area=='Total',Sex %in% c('Male','Female'))[,6:18],2,sum)))
typeof(filter(predata3,Country==i,Age==j,Area=='Total',Sex %in% c('Male','Female'))[1,8])
typeof(predata3[1,10])
filter(predata3,Country==i,Age==j,Area=='Total',Sex %in% c('Male','Female'))[1,6]
predata3[1,7]=as.numeric(predata3[1,7])
predata3[1,7]
typeof(filter(predata3,Country==i,Age==j,Area=='Total',Sex %in% c('Male','Female'))[,6])
c(1,i,'Total','Both Sexes',j,
  apply(filter(predata3,Country==i,Age==j,Area=='Total',Sex %in% c('Male','Female'))[,6:18],2,sum))
c(1,i,'Total','Both Sexes',j)
apply(filter(predata3,Country==i,Age==j,Area=='Total',Sex %in% c('Male','Female'))[,6:18],2,sum)
p=rbind(predata3[1,],list(2,'Yes','t','s',5,4,3,3,3,3,3,3,3,3,3,3,3,3))
p[1,5:6]=c(2,3)
p
list(2,'Yes')[[1:2]]
apply(filter(predata3,Country==i,Age==j,Area=='Total',Sex %in% c('Male','Female'))[,6:18],2,sum)
predata3[1,7]+predata3[1,8]
filter(predata3,Country==i,Age==j,Area=='Total',Sex %in% c('Male','Female'))[,6:18]
predata3=rbind(predata3,list(1,i,'Total','Both Sexes',j,0,0,0,0,0,0,0,0,0,0,0,0,0))
head(rbind(predata3,c(1,i,'Total','Both Sexes',j,0,0,0,0,0,0,0,0,0,0,0,0,0)))

TBSA3=predata3


#data set with only married,separated,widowed
TBSA3$S2==apply(TBSA3[,6:15],1,sum)
TBSA3=mutate(TBSA3,S2=c2+c3+c4+c5+c6+c7+c8+c9)
TBSA3=setdiff(TBSA3,filter(TBSA3,S2==0))
TBSA3=mutate(TBSA3,C1=(c2+c3+c4)/S2,C2=(c5+c6+c7+c8)/S2,C3=c9/S2)
x=list()
y=list()
for(i in 1:9){
  x[[i]]=filter(TBSA3,Age %in% seq((i-1)*10,i*10,0.5))[PIi[1,]>0.5,19:21]
  y[[i]]=filter(TBSA3,Age %in% seq((i-1)*10,i*10,0.5))[PIi[1,]<0.5,19:21]
}

#data set with only single,married,separated
TBSA3=predata3
TBSA3$S2==apply(TBSA3[,6:15],1,sum)
TBSA3=mutate(TBSA3,S2=c2+c3+c4+c5+c6+c7+c8+c1)
TBSA3=setdiff(TBSA3,filter(TBSA3,S2==0))
TBSA3=mutate(TBSA3,C1=c1/S2,C2=(c2+c3+c4)/S2,C3=(c5+c6+c7+c8)/S2)
x=list()
y=list()
for(i in 1:9){
  x[[i]]=filter(TBSA3,Age %in% seq((i-1)*10,i*10,0.5))[PIi[1,]>0.5,19:21]
  y[[i]]=filter(TBSA3,Age %in% seq((i-1)*10,i*10,0.5))[PIi[1,]<0.5,19:21]
}
#data set with only single,married,widowed
TBSA3=predata3[,2:ncol(predata3)]
TBSA3$S2==apply(TBSA3[,6:15],1,sum)
TBSA3=mutate(TBSA3,S2=c1+c2+c3+c4+c9)
TBSA3=setdiff(TBSA3,filter(TBSA3,S2==0))
TBSA3=mutate(TBSA3,C1=c1/S2,C2=(c2+c3+c4)/S2,C3=c9/S2)
x=list()
y=list()
for(i in 1:9){
  x[[i]]=filter(TBSA3[TBSA3[,3]=='Urban',],Age %in% seq((i-1)*10,i*10,0.5))[,19:21]
  y[[i]]=filter(TBSA3[TBSA3[,3]=='Rural',],Age %in% seq((i-1)*10,i*10,0.5))[,19:21]
}
x[[1]]
for(i in 1:9)
  x[[i]]=filter(TBSA3,Age %in% seq((i-1)*10,i*10,0.5))[,19:21]


DATA4=data.frame(no='',Country='',Area='',Sex='',Age='',C1='',C2='',C3='')[FALSE,]
DATA4=rbind(DATA4,select(TBSA3,no,Country,Area,Sex,Age,C1,C2,C3))
DATA4[DATA4$Area=='Urban',]$Area=0
DATA4[DATA4$Area=='Rural',]$Area=1
DATA4[DATA4$Area=='Total',]$Area=0.5
DATA4[DATA4$Sex=='Male',]$Sex=0
DATA4[DATA4$Sex=='Female',]$Sex=1
DATA4[DATA4$Sex=='Both Sexes',]$Sex=0.5
DATA4$no=as.numeric(DATA4$no)

DATA4$no=1:nrow(DATA4)
N=1
for(i in unique(DATA4$Country)){
  DATA4[DATA4$Country==i,]$Country=N
  N=N+1
}
DATA4$Country=as.numeric(DATA4$Country)
for(i in 1:nrow(DATA4)){
  BC=0
  if(DATA4$C1[i]==0){
    BC=BC+1
    DATA4$C1[i]=0.0000001
  }
  
  if(DATA4$C2[i]==0){
    BC=BC+1 
    DATA4$C2[i]=0.0000001
  } 
  if(DATA4$C3[i]==0){
    BC=BC+1
    DATA4$C3[i]=0.0000001
  }  
  max=max(DATA4$C1[i],DATA4$C2[i],DATA4$C3[i],DATA4$C4[i])
  if(DATA4$C1[i]==max) DATA4$C1[i]=DATA4$C1[i]-0.0000001*BC
  else if(DATA4$C2[i]==max) DATA4$C2[i]=DATA4$C2[i]-0.0000001*BC
  else if(DATA4$C3[i]==max) DATA4$C3[i]=DATA4$C3[i]-0.0000001*BC
}


#--data set with genders being separate observations----
DATA5=DATA4
DATA5[DATA5$Sex==1,2]=DATA5[DATA5$Sex==1,2]+221
DATA5=DATA5[with(DATA5, order(Country)), ]
j=1
for(i in unique(DATA5$Country)){
  DATA5[DATA5$Country==i,2]=j
  j=j+1
}
for( i in 1:dim(DATA5)[1]){
  DATA5[i,1]=i
}
tail(DATA5,n=20)

#--testing subset of DATA5
DATA6=DATA5
DATA6=DATA6[DATA6$Country %in% c(10:60,230:280),]
j=1
for(i in unique(DATA6$Country)){
  DATA6[DATA6$Country==i,2]=j
  j=j+1
}
for( i in 1:dim(DATA6)[1]){
  DATA6[i,1]=i
}