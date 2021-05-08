path="C:\\Users\\picojazz\\Desktop\\TPperformances\\" #chemin du dossier o√π se trouvent les tables 
# l'utilisateur doit mettre toutes ses tables dans le meme dossier et mettre le chemin du dossier ou elles 
#sont situ√©es ici

setwd(path)#place le curseur de lecture de fichiers au chemin pr√©c√©dent





dataframe1=read.table("calls-2014-02.csv", sep=",", dec=".", header=TRUE)
df1<-na.omit(dataframe1)
print(head(df1))



#creer deux nouveaux colonnes: l'une contient la date du jour et l'autre contient l'heure
# Les deux infos sont dans la colone 'date_received'
dtimes=as.character(df1$date_received)
dtparts = t(as.data.frame(strsplit(dtimes,' ' )))
print(head(dtparts))


row.names(dtparts) = NULL
dtparts=as.data.frame(dtparts)
names(dtparts)<-c('laDate','lHeure')
#print(head(dtparts))


#split l'heure
timeOnly=as.character(dtparts$lHeure)
lHS=t(as.data.frame(strsplit(timeOnly, ':')))   
row.names(lHS)=NULL
lHS=as.data.frame(lHS)
names(lHS)<-c('HH','MM','SS')
#print(head(lHS))




dtCall = cbind(dtparts$laDate,lHS)
dtType=as.character(df1$queue_name)
dtCall = cbind(dtCall,dtType)

row.names(dtCall)=NULL
dtCall=as.data.frame(dtCall)
names(dtCall)<-c('DATE','HH','MM','SS','TYPE')
print(head(dtCall)) #jeu de donnee formatter 2017-02-01 08 05 22




#list des types d'appel   NB: on 1 seul enregistrement pour 30178
listType = unique(dtCall$TYPE)
print(listType)   




tauxParType = function(dtCall,type){

heure <- c("08","09","10","11","12","13","14","15","16","17","18","19")
dtSum <- c()


for(i in 1:length(heure)){

  periode1 = 0
  periode2 = 0
  
  
  dt <- subset(dtCall,HH == heure[i]) #recup tous les donnÈe de l'heure i'
  
  if(nrow(dt)==0){
    next
  }
  
  #print(head(dt))
  date1 = dtCall$DATE[1]  #date de la 1er ligne
  tempDate = dtCall$DATE[1]
  #print(date1)
  
  for(j in 1:nrow(dt)){

    row <- dt[j,]
    
    indice = as.integer(row$MM)/30
    if(indice < 1 && indice >=0){
      periode1 = periode1 + 1
    }else if(indice >= 1){
      periode2 = periode2 + 1
    }
    
    
    
    if(date1 != row$DATE){ #si la date change on ecrit le nombre dappel sur la periode a cette date 
      
      dtSum <- rbind(dtSum,c(date1,heure[i],1,periode1))
      dtSum <- rbind(dtSum,c(date1,heure[i],2,periode2))
      #on remet les compteurs a 0
      periode1 = 0
      periode2 = 0
      
      #on prend la nouvelle date
      date1 = row$DATE
      #print(date1)
      
    }
    
    
  }
  
  #verifier si la date n'a pas changÈ on ecrit le nombre dappel sur la periode a cette date vu qu'il n'est pas entrÈ dans le IF
  if(date1 == tempDate){
    dtSum <- rbind(dtSum,c(date1,heure[i],1,periode1))
    dtSum <- rbind(dtSum,c(date1,heure[i],2,periode2))
  }
  

}
row.names(dtSum)=NULL
dtSum=as.data.frame(dtSum)
#print(head(dtSum))
names(dtSum)<-c('date','HH','periode','totAppel')

#print(head(dtSum))

p <- c(1,2)
dtTauxPeriode <- c()


for(i in 1:length(heure)){
  
  for(j in p){
    
    dtp <- subset(dtSum,HH == heure[i] & periode == j)
    #print(head(dtp))
    totappel <- sum(as.numeric(dtp$totAppel))
    numLine = nrow(dtp)
    taux = totappel / numLine
    if(j == 1){
      dtTauxPeriode <- rbind(dtTauxPeriode,c(paste(heure[i],"H 00"),taux))
    }else{
      dtTauxPeriode <- rbind(dtTauxPeriode,c(paste(heure[i],"H 30"),taux))
    }
    
    
    
  }
  
}

#dtTauxPeriode <- cbind(dtTauxPeriode,c(1:24))

row.names(dtTauxPeriode)=NULL
dtTauxPeriode=as.data.frame(dtTauxPeriode)
names(dtTauxPeriode)<-c('HH','taux')
#print(dtTauxPeriode)

plot(dtTauxPeriode$taux ,xaxt = 'n', main=paste("taux moyen d'arrivee",type,sep = " "), type="b", xlab = "Periode", ylab = "Taux moyen d'arrivee")
axis(1,at = 1:24 ,labels=dtTauxPeriode$HH)

}





par(mfcol = c(4,4))
for(i in 1:length(listType)){

  dtt <- subset(dtCall,TYPE == listType[i] )
  tauxParType(dtCall = dtt,type = listType[i])

}













