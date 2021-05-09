path="C:\\Users\\picojazz\\Desktop\\TPperformances\\" #chemin du dossier où se trouvent les tables 
# l'utilisateur doit mettre toutes ses tables dans le meme dossier et mettre le chemin du dossier ou elles 
#sont situées ici

setwd(path)#place le curseur de lecture de fichiers au chemin précédent





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
dtAgent=as.character(df1$agent_number)
dtCall = cbind(dtCall,dtAgent)

row.names(dtCall)=NULL
dtCall=as.data.frame(dtCall)
names(dtCall)<-c('DATE','HH','MM','SS','AGENT')
print(head(dtCall)) #jeu de donnee formatter 2017-02-01 08 05 22




#list des agents   
listAgent = unique(dtCall$AGENT)
print(listAgent)   






heure <- c("08","09","10","11","12","13","14","15","16","17","18","19")
dtSum <- c()


for(i in 1:length(heure)){
  
  
  dt <- subset(dtCall,HH == heure[i]) #recup tous les donn�e de l'heure i'
  dt1 = unique(subset(dt,(as.numeric(MM) / 30) <1 & (as.numeric(MM) / 30) >=0)$AGENT) #liste agent periode 1 de l'heure i
  dt1 = subset(dt1,dt1 != "NULL")
  dt2 = unique(subset(dt,(as.numeric(MM) / 30) >1)$AGENT) #liste agent periode 2 de l'heure i
  dt2 = subset(dt2,dt2 != "NULL")
  dtSum = rbind(dtSum,c(paste(heure[i],"H 00"),length(dt1)))
  dtSum = rbind(dtSum,c(paste(heure[i],"H 30"),length(dt2)))
  

}
row.names(dtSum)=NULL
dtSum=as.data.frame(dtSum)
#print(head(dtSum))
names(dtSum)<-c('HH','nbreAgent')

print(dtSum)



plot(dtSum$nbreAgent ,xaxt = 'n', main="Nombre d'agents par periode (Staffing) ", type="b", xlab = "Periode", ylab = "nombre d'agent")
axis(1,at = 1:24 ,labels=dtSum$HH)





















