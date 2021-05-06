path="C:\\Users\\picojazz\\Desktop\\TPperformances\\" #chemin du dossier où se trouvent les tables 
# l'utilisateur doit mettre toutes ses tables dans le meme dossier et mettre le chemin du dossier ou elles 
#sont situées ici

setwd(path)#place le curseur de lecture de fichiers au chemin précédent



#nombre_dappel_periode_mois <- function(fichier) {

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


#split la date
dtDate=as.character(dtparts$laDate)
lDate=t(as.data.frame(strsplit(dtDate, '-')))   
row.names(lDate)=NULL
lDate=as.data.frame(lDate)
names(lDate)<-c('ANNEE','MOIS','JOUR')
#print(head(lDate))

dtCall = cbind(dtparts$laDate,lHS)
row.names(dtCall)=NULL
dtCall=as.data.frame(dtCall)
names(dtCall)<-c('DATE','HH','MM','SS')
print(head(dtCall)) #jeu de donnee formatter 2017-02-01 08 05 22











heure <- c("08","09","10","11","12","13","14","15","16","17","18","19")
dtSum <- c()


for(i in 1:length(heure)){

  periode1 = 0
  periode2 = 0

  dt <- subset(dtCall,HH == heure[i]) #recup tous les donn�e de l'heure i'
  #print(head(dt))
  date1 = dtCall$DATE[1]  #date de la 1er ligne
  #print(date1)
  
  for(j in 1:nrow(dt)){

    row <- dt[j,]
    
    indice = as.integer(row$MM)/30
    if(indice < 1){
      periode1 = periode1 + 1
    }else{
      periode2 = periode2 + 1
    }
    
    
    
    if(date1 != row$DATE){ #si la date change on ecrit le nombre dappel sur la periode a cette date 
      
      dtSum <- rbind(dtSum,c(date1,heure[i],1,periode1))
      dtSum <- rbind(dtSum,c(date1,heure[i],2,periode2))
      #on remet les compteur a 0
      periode1 = 0
      periode2 = 0
      
      #on prend la nouvelle date
      date1 = row$DATE
      print(date1)
      
    }
    
  }
  
  

}
row.names(dtSum)=NULL
dtSum=as.data.frame(dtSum)
names(dtSum)<-c('date','heure','periode','totAppel')

print(dtSum)

#print(sum(as.numeric(dtSum$totAppel)))
#}





