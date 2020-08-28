library(readxl)
library(plotly)
library(ggplot2)
library(dplyr)
library(psych)
library(graphics)

radar<- read_excel("C:/Users/Besitzer/Dropbox/Master/Radar.xlsx", col_names=TRUE)

names(radar)[30]<-"SW2"
names(radar)[3]<-"SW1"

#U4n

radar$U4nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$U4n[i]==7){
    radar$U4nr[i]<-1
  }
  if (radar$U4n[i]==6){
    radar$U4nr[i]<-2
  }
  if(radar$U4n[i]==5){
    radar$U4nr[i]<-3
  } 
  if (radar$U4n[i]==4){
    radar$U4nr[i]<-4
  }
  if (radar$U4n[i]==3){
    radar$U4nr[i]<-5
  }
  if (radar$U4n[i]==2){
    radar$U4nr[i]<-6
  }
  if (radar$U4n[i]==1){
    radar$U4nr[i]<-7
  }
}

#MB1n

radar$MB1nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$MB1n[i]==7){
    radar$MB1nr[i]<-1
  }
  if (radar$MB1n[i]==6){
    radar$MB1nr[i]<-2
  }
  if(radar$MB1n[i]==5){
    radar$MB1nr[i]<-3
  } 
  if (radar$MB1n[i]==4){
    radar$MB1nr[i]<-4
  }
  if (radar$MB1n[i]==3){
    radar$MB1nr[i]<-5
  }
  if (radar$MB1n[i]==2){
    radar$MB1nr[i]<-6
  }
  if (radar$MB1n[i]==1){
    radar$MB1nr[i]<-7
  }
}

#IV1n

radar$IV1nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$IV1n[i]==7){
    radar$IV1nr[i]<-1
  }
  if (radar$IV1n[i]==6){
    radar$IV1nr[i]<-2
  }
  if(radar$IV1n[i]==5){
    radar$IV1nr[i]<-3
  } 
  if (radar$IV1n[i]==4){
    radar$IV1nr[i]<-4
  }
  if (radar$IV1n[i]==3){
    radar$IV1nr[i]<-5
  }
  if (radar$IV1n[i]==2){
    radar$IV1nr[i]<-6
  }
  if (radar$IV1n[i]==1){
    radar$IV1nr[i]<-7
  }
}

#WA2n

radar$WA2nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$WA2n[i]==7){
    radar$WA2nr[i]<-1
  }
  if (radar$WA2n[i]==6){
    radar$WA2nr[i]<-2
  }
  if(radar$WA2n[i]==5){
    radar$WA2nr[i]<-3
  } 
  if (radar$WA2n[i]==4){
    radar$WA2nr[i]<-4
  }
  if (radar$WA2n[i]==3){
    radar$WA2nr[i]<-5
  }
  if (radar$WA2n[i]==2){
    radar$WA2nr[i]<-6
  }
  if (radar$WA2n[i]==1){
    radar$WA2nr[i]<-7
  }
}

#BM3n

radar$BM3nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$BM3n[i]==7){
    radar$BM3nr[i]<-1
  }
  if (radar$BM3n[i]==6){
    radar$BM3nr[i]<-2
  }
  if(radar$BM3n[i]==5){
    radar$BM3nr[i]<-3
  } 
  if (radar$BM3n[i]==4){
    radar$BM3nr[i]<-4
  }
  if (radar$BM3n[i]==3){
    radar$BM3nr[i]<-5
  }
  if (radar$BM3n[i]==2){
    radar$BM3nr[i]<-6
  }
  if (radar$BM3n[i]==1){
    radar$BM3nr[i]<-7
  }
}

#FS1n

radar$FS1nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$FS1n[i]==7){
    radar$FS1nr[i]<-1
  }
  if (radar$FS1n[i]==6){
    radar$FS1nr[i]<-2
  }
  if(radar$FS1n[i]==5){
    radar$FS1nr[i]<-3
  } 
  if (radar$FS1n[i]==4){
    radar$FS1nr[i]<-4
  }
  if (radar$FS1n[i]==3){
    radar$FS1nr[i]<-5
  }
  if (radar$FS1n[i]==2){
    radar$FS1nr[i]<-6
  }
  if (radar$FS1n[i]==1){
    radar$FS1nr[i]<-7
  }
}

#FS2n

radar$FS2nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$FS2n[i]==7){
    radar$FS2nr[i]<-1
  }
  if (radar$FS2n[i]==6){
    radar$FS2nr[i]<-2
  }
  if(radar$FS2n[i]==5){
    radar$FS2nr[i]<-3
  } 
  if (radar$FS2n[i]==4){
    radar$FS2nr[i]<-4
  }
  if (radar$FS2n[i]==3){
    radar$FS2nr[i]<-5
  }
  if (radar$FS2n[i]==2){
    radar$FS2nr[i]<-6
  }
  if (radar$FS2n[i]==1){
    radar$FS2nr[i]<-7
  }
}

#MB2n

radar$MB2nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$MB2n[i]==7){
    radar$MB2nr[i]<-1
  }
  if (radar$MB2n[i]==6){
    radar$MB2nr[i]<-2
  }
  if(radar$MB2n[i]==5){
    radar$MB2nr[i]<-3
  } 
  if (radar$MB2n[i]==4){
    radar$MB2nr[i]<-4
  }
  if (radar$MB2n[i]==3){
    radar$MB2nr[i]<-5
  }
  if (radar$MB2n[i]==2){
    radar$MB2nr[i]<-6
  }
  if (radar$MB2n[i]==1){
    radar$MB2nr[i]<-7
  }
}

#IV2n

radar$IV2nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$IV2n[i]==7){
    radar$IV2nr[i]<-1
  }
  if (radar$IV2n[i]==6){
    radar$IV2nr[i]<-2
  }
  if(radar$IV2n[i]==5){
    radar$IV2nr[i]<-3
  } 
  if (radar$IV2n[i]==4){
    radar$IV2nr[i]<-4
  }
  if (radar$IV2n[i]==3){
    radar$IV2nr[i]<-5
  }
  if (radar$IV2n[i]==2){
    radar$IV2nr[i]<-6
  }
  if (radar$IV2n[i]==1){
    radar$IV2nr[i]<-7
  }
}

#U3n

radar$U3nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$U3n[i]==7){
    radar$U3nr[i]<-1
  }
  if (radar$U3n[i]==6){
    radar$U3nr[i]<-2
  }
  if(radar$U3n[i]==5){
    radar$U3nr[i]<-3
  } 
  if (radar$U3n[i]==4){
    radar$U3nr[i]<-4
  }
  if (radar$U3n[i]==3){
    radar$U3nr[i]<-5
  }
  if (radar$U3n[i]==2){
    radar$U3nr[i]<-6
  }
  if (radar$U3n[i]==1){
    radar$U3nr[i]<-7
  }
}

#IV3n

radar$IV3nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$IV3n[i]==7){
    radar$IV3nr[i]<-1
  }
  if (radar$IV3n[i]==6){
    radar$IV3nr[i]<-2
  }
  if(radar$IV3n[i]==5){
    radar$IV3nr[i]<-3
  } 
  if (radar$IV3n[i]==4){
    radar$IV3nr[i]<-4
  }
  if (radar$IV3n[i]==3){
    radar$IV3nr[i]<-5
  }
  if (radar$IV3n[i]==2){
    radar$IV3nr[i]<-6
  }
  if (radar$IV3n[i]==1){
    radar$IV3nr[i]<-7
  }
}

#FS3n

radar$FS3nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$FS3n[i]==7){
    radar$FS3nr[i]<-1
  }
  if (radar$FS3n[i]==6){
    radar$FS3nr[i]<-2
  }
  if(radar$FS3n[i]==5){
    radar$FS3nr[i]<-3
  } 
  if (radar$FS3n[i]==4){
    radar$FS3nr[i]<-4
  }
  if (radar$FS3n[i]==3){
    radar$FS3nr[i]<-5
  }
  if (radar$FS3n[i]==2){
    radar$FS3nr[i]<-6
  }
  if (radar$FS3n[i]==1){
    radar$FS3nr[i]<-7
  }
}

#U2n

radar$U2nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$U2n[i]==7){
    radar$U2nr[i]<-1
  }
  if (radar$U2n[i]==6){
    radar$U2nr[i]<-2
  }
  if(radar$U2n[i]==5){
    radar$U2nr[i]<-3
  } 
  if (radar$U2n[i]==4){
    radar$U2nr[i]<-4
  }
  if (radar$U2n[i]==3){
    radar$U2nr[i]<-5
  }
  if (radar$U2n[i]==2){
    radar$U2nr[i]<-6
  }
  if (radar$U2n[i]==1){
    radar$U2nr[i]<-7
  }
}

#WA3n

radar$WA3nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$WA3n[i]==7){
    radar$WA3nr[i]<-1
  }
  if (radar$WA3n[i]==6){
    radar$WA3nr[i]<-2
  }
  if(radar$WA3n[i]==5){
    radar$WA3nr[i]<-3
  } 
  if (radar$WA3n[i]==4){
    radar$WA3nr[i]<-4
  }
  if (radar$WA3n[i]==3){
    radar$WA3nr[i]<-5
  }
  if (radar$WA3n[i]==2){
    radar$WA3nr[i]<-6
  }
  if (radar$WA3n[i]==1){
    radar$WA3nr[i]<-7
  }
}

#MB3n/BM4n
names(radar)[21]<-"MB3nBM4n"
radar$MB3nBM4nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$MB3nBM4n[i]==7){
    radar$MB3nBM4nr[i]<-1
  }
  if (radar$MB3nBM4n[i]==6){
    radar$MB3nBM4nr[i]<-2
  }
  if(radar$MB3nBM4n[i]==5){
    radar$MB3nBM4nr[i]<-3
  } 
  if (radar$MB3nBM4n[i]==4){
    radar$MB3nBM4nr[i]<-4
  }
  if (radar$MB3nBM4n[i]==3){
    radar$MB3nBM4nr[i]<-5
  }
  if (radar$MB3nBM4n[i]==2){
    radar$MB3nBM4nr[i]<-6
  }
  if (radar$MB3nBM4n[i]==1){
    radar$MB3nBM4nr[i]<-7
  }
}

#U1n

radar$U1nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$U1n[i]==7){
    radar$U1nr[i]<-1
  }
  if (radar$U1n[i]==6){
    radar$U1nr[i]<-2
  }
  if(radar$U1n[i]==5){
    radar$U1nr[i]<-3
  } 
  if (radar$U1n[i]==4){
    radar$U1nr[i]<-4
  }
  if (radar$U1n[i]==3){
    radar$U1nr[i]<-5
  }
  if (radar$U1n[i]==2){
    radar$U1nr[i]<-6
  }
  if (radar$U1n[i]==1){
    radar$U1nr[i]<-7
  }
}

#IV4n

radar$IV4nr<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$IV4n[i]==7){
    radar$IV4nr[i]<-1
  }
  if (radar$IV4n[i]==6){
    radar$IV4nr[i]<-2
  }
  if(radar$IV4n[i]==5){
    radar$IV4nr[i]<-3
  } 
  if (radar$IV4n[i]==4){
    radar$IV4nr[i]<-4
  }
  if (radar$IV4n[i]==3){
    radar$IV4nr[i]<-5
  }
  if (radar$IV4n[i]==2){
    radar$IV4nr[i]<-6
  }
  if (radar$IV4n[i]==1){
    radar$IV4nr[i]<-7
  }
}

radar$WA1p<- as.numeric(radar$WA1p)
radar$BM1p<- as.numeric(radar$BM1p)
radar$BM2p<- as.numeric(radar$BM2p)
radar$U4nr<- as.numeric(radar$U4nr)
radar$U3nr<- as.numeric(radar$U3nr)
radar$U2nr<- as.numeric(radar$U2nr)
radar$U1nr<- as.numeric(radar$U1nr)
radar$MB1nr<- as.numeric(radar$MB1nr)
radar$MB2nr<- as.numeric(radar$MB2nr)
radar$MB3nBM4nr<- as.numeric(radar$MB3nBM4nr)
radar$WA2nr<- as.numeric(radar$WA2nr)
radar$WA3nr<- as.numeric(radar$WA3nr)
radar$BM3nr<- as.numeric(radar$BM3nr)
radar$IV1nr<- as.numeric(radar$IV1nr)
radar$IV2nr<- as.numeric(radar$IV2nr)
radar$IV3nr<- as.numeric(radar$IV3nr)
radar$IV4nr<- as.numeric(radar$IV4nr)
radar$FS1nr<- as.numeric(radar$FS1nr)
radar$FS2nr<- as.numeric(radar$FS2nr)
radar$FS3nr<- as.numeric(radar$FS3nr)

#Rechnung

radar$UB <- ((radar$U4nr + radar$U3nr + radar$U2nr + radar$U1nr)/4)
radar$MB <- (radar$MB1nr + radar$MB2nr + radar$MB3nBM4nr)/3
radar$WA <- (radar$WA1p + radar$WA2nr + radar$WA3nr)/3
radar$BM <- (radar$BM1p + radar$BM2p + radar$BM3nr + radar$MB3nBM4nr)/4
radar$IV <- (radar$IV1nr + radar$IV2nr + radar$IV3nr + radar$IV4nr)/4
radar$FS <- (radar$FS1nr + radar$FS2nr + radar$FS3nr)/3

#Radar Chart gesamt####

plot_ly(radar,  type= 'scatterpolar',
        r=c(mean(radar$UB), mean(radar$MB), mean(radar$WA), mean(radar$BM), mean(radar$IV), mean(radar$FS)),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'),
        fill= 'toself',
        name="Gesamt")%>%
  layout( title="Gesamt",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#nudging zustimmung####

j=1
for (j in 1:length(radar$N1)){
  if(radar$N1[j]==2){
    radar$N1[j]<-0
  }
  if(radar$N2[j]==2){
      radar$N2[j]<-0
  }
  if(radar$N3[j]==2){
    radar$N3[j]<-0
  }
  if(radar$N4[j]==2){
    radar$N4[j]<-0
  }
  if(radar$N5[j]==2){
    radar$N5[j]<-0
  }
  if(radar$N6[j]==2){
    radar$N6[j]<-0
  }
  if(radar$N7[j]==2){
    radar$N7[j]<-0
  }
  if(radar$N8[j]==2){
    radar$N8[j]<-0
  }
  if(radar$N9[j]==2){
    radar$N9[j]<-0
  }
  if(radar$N10[j]==2){
    radar$N10[j]<-0
  }
}
radar$zust<- radar$N1 + radar$N2+ radar$N3+radar$N4+radar$N5+radar$N6+radar$N7+radar$N8+radar$N9+radar$N10
oben_zust<- subset(radar, radar$zust>="6")
unten_zust<-subset(radar, radar$zust<="4")

plot_ly( type= 'scatterpolar', fill= 'toself')%>%
  add_trace(oben_zust,
        r=c(mean(oben_zust$UB), mean(oben_zust$MB), mean(oben_zust$WA), mean(oben_zust$BM), mean(oben_zust$IV), mean(oben_zust$FS)),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'),
        name="Oberes Drittel"
        )%>%
  add_trace(unten_zust,
    r=c(mean(unten_zust$UB), mean(unten_zust$MB), mean(unten_zust$WA), mean(unten_zust$BM), mean(unten_zust$IV), mean(unten_zust$FS)),
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'),
    name="Unteres Drittel"
  )%>%
  layout(title="Zustimmung Nudging", polar = list(radialaxis = list(visible = T,range = c(0,7))))

#nach Geschlecht

names(radar)[31]<- "sex"
nach_sex<- group_by(radar,sex)
sex<-data.frame(summarize(nach_sex, mean(UB), mean(MB), mean(WA), mean(BM), mean(IV), mean(FS)))

plot_ly(sex, type= 'scatterpolar', fill='toself',
    r=c(sex$mean.UB.[3], sex$mean.MB.[3], sex$mean.WA.[3], sex$mean.BM.[3], sex$mean.IV.[3], sex$mean.FS.[3]),
    name='weiblich',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
    )%>%
  add_trace(
    r=c(sex$mean.UB.[2], sex$mean.MB.[2], sex$mean.WA.[2], sex$mean.BM.[2], sex$mean.IV.[2], sex$mean.FS.[2]),
    name='m‰nnlich',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
    )%>%
  layout(title="Geschlecht",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Histogramme
hist(radar$UB, main = paste("Umweltbewusstsein"), xlab="Grad der Zustimmung", ylab = "H‰ufigkeit", col = "blue")
hist(radar$MB, main = paste("Monet‰re Beeinflussung"), xlab="Grad der Zustimmung", ylab = "H‰ufigkeit", col = "blue")
hist(radar$IV, main = paste("Imageverbesserung"), xlab="Grad der Zustimmung", ylab = "H‰ufigkeit", col = "blue")
hist(radar$WA, main = paste("Wahrnehmung Anreiz"), xlab="Grad der Zustimmung", ylab = "H‰ufigkeit", col = "blue")
hist(radar$BM, main = paste("Bereitschaft Mehraufwand"), xlab="Grad der Zustimmung", ylab = "H‰ufigkeit", col = "blue")
hist(radar$FS, main = paste("Freiheitsstreben"), xlab="Grad der Zustimmung", ylab = "H‰ufigkeit", col = "blue")

#Heatmaps####
ggplot(radar, aes(x=UB, y=MB)) + geom_hex()+
  scale_fill_gradient(low="gray", high="blue") 
ggplot(radar, aes(x=BM, y=UB)) + geom_hex()+
  scale_fill_gradient(low="gray", high="blue") 
ggplot(radar, aes(x=BM, y=MB)) + geom_hex()+
  scale_fill_gradient(low="gray", high="blue") 

#Korrelation####
cor(radar$UB, radar$MB)
#-0.006866752
cor(radar$UB, radar$BM)
#0.5144401
cor(radar$UB, radar$IV)
#0.6206744
cor(radar$UB, radar$FS)
#0.2065317
cor(radar$UB, radar$WA)
#0.3524718
cor(radar$MB, radar$BM)
#0.1763526
cor(radar$MB, radar$IV)
#0.07552059
cor(radar$MB, radar$FS)
#-0.05769196
cor(radar$MB, radar$WA)
#0.09859887
cor(radar$FS, radar$IV)
#0.2845396
cor(radar$FS, radar$WA)
#0.050343
cor(radar$FS, radar$BM)
#0.08298545
cor(radar$IV, radar$BM)
#0.3554593
cor(radar$IV, radar$WA)
#0.2972509
cor(radar$WA, radar$BM)
#0.1754109
cor(radar$UB, radar$LZ)

cor(radar$LZ,radar$SW1)

cor(radar$LZ,radar$SW2)


cor(radar$UB,radar$SW1)
cor(radar$UB,radar$SW2)

cor(radar$BM,radar$SW1)
cor(radar$BM,radar$SW2)

cor(radar$MB,radar$SW1)
cor(radar$MB,radar$SW2)

cor(radar$LZ,radar$UB)
cor(radar$LZ,radar$IV)
cor(radar$LZ,radar$MB)
cor(radar$LZ,radar$BM)
cor(radar$LZ,radar$FS)
cor(radar$LZ,radar$WA)


#Lebenszufriedenheit
radar$LZ1r<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$LZ1[i]==7){
    radar$LZ1r[i]<-1
  }
  if (radar$LZ1[i]==6){
    radar$LZ1r[i]<-2
  }
  if(radar$LZ1[i]==5){
    radar$LZ1r[i]<-3
  } 
  if (radar$LZ1[i]==4){
    radar$LZ1r[i]<-4
  }
  if (radar$LZ1[i]==3){
    radar$LZ1r[i]<-5
  }
  if (radar$LZ1[i]==2){
    radar$LZ1r[i]<-6
  }
  if (radar$LZ1[i]==1){
    radar$LZ1r[i]<-7
  }
}

radar$LZ2r<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$LZ2[i]==7){
    radar$LZ2r[i]<-1
  }
  if (radar$LZ2[i]==6){
    radar$LZ2r[i]<-2
  }
  if(radar$LZ2[i]==5){
    radar$LZ2r[i]<-3
  } 
  if (radar$LZ2[i]==4){
    radar$LZ2r[i]<-4
  }
  if (radar$LZ2[i]==3){
    radar$LZ2r[i]<-5
  }
  if (radar$LZ2[i]==2){
    radar$LZ2r[i]<-6
  }
  if (radar$LZ2[i]==1){
    radar$LZ2r[i]<-7
  }
}

radar$LZ3r<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$LZ3[i]==7){
    radar$LZ3r[i]<-1
  }
  if (radar$LZ3[i]==6){
    radar$LZ3r[i]<-2
  }
  if(radar$LZ3[i]==5){
    radar$LZ3r[i]<-3
  } 
  if (radar$LZ3[i]==4){
    radar$LZ3r[i]<-4
  }
  if (radar$LZ3[i]==3){
    radar$LZ3r[i]<-5
  }
  if (radar$LZ3[i]==2){
    radar$LZ3r[i]<-6
  }
  if (radar$LZ3[i]==1){
    radar$LZ3r[i]<-7
  }
}

radar$LZ4r<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$LZ4[i]==7){
    radar$LZ4r[i]<-1
  }
  if (radar$LZ4[i]==6){
    radar$LZ4r[i]<-2
  }
  if(radar$LZ4[i]==5){
    radar$LZ4r[i]<-3
  } 
  if (radar$LZ4[i]==4){
    radar$LZ4r[i]<-4
  }
  if (radar$LZ4[i]==3){
    radar$LZ4r[i]<-5
  }
  if (radar$LZ4[i]==2){
    radar$LZ4r[i]<-6
  }
  if (radar$LZ4[i]==1){
    radar$LZ4r[i]<-7
  }
}

radar$LZ5r<-"r"
i=1
for(i in 1:length(radar$U4n))
{
  if (radar$LZ5[i]==7){
    radar$LZ5r[i]<-1
  }
  if (radar$LZ5[i]==6){
    radar$LZ5r[i]<-2
  }
  if(radar$LZ5[i]==5){
    radar$LZ5r[i]<-3
  } 
  if (radar$LZ5[i]==4){
    radar$LZ5r[i]<-4
  }
  if (radar$LZ5[i]==3){
    radar$LZ5r[i]<-5
  }
  if (radar$LZ5[i]==2){
    radar$LZ5r[i]<-6
  }
  if (radar$LZ5[i]==1){
    radar$LZ5r[i]<-7
  }
}

radar$LZ1r<- as.numeric(radar$LZ1r)
radar$LZ2r<- as.numeric(radar$LZ2r)
radar$LZ3r<- as.numeric(radar$LZ3r)
radar$LZ4r<- as.numeric(radar$LZ4r)
radar$LZ5r<- as.numeric(radar$LZ5r)

radar$LZ <- radar$LZ1r + radar$LZ2r + radar$LZ3r + radar$LZ4r + radar$LZ5r
radar$happy<-"happy"
i=1
for (i in 1:length(radar$LZ4r))
  {
  if (radar$LZ[i]<=35){
    radar$happy[i]<-"VHS"
  }
  if(radar$LZ[i]<=29){
    radar$happy[i]<-"HS"
  }
  if (radar$LZ[i]<=24){
    radar$happy[i]<-"AS"
  }
  if (radar$LZ[i]<=19){
    radar$happy[i]<-"BS"
  }
  if (radar$LZ[i]<=14){
    radar$happy[i]<-"DS"
  }
  if (radar$LZ[i]<=9){
    radar$happy[i]<-"EDS"
  }
}

nach_happy<- group_by(radar, happy)
happy<- data.frame(summarise(nach_happy,mean(UB), mean(MB), mean(WA), mean(BM), mean(IV), mean(FS)))

plot_ly(happy, type= 'scatterpolar', fill='toself', fillcolor="half-transparent", name="AS",
        r= c(happy$mean.UB.[1], happy$mean.MB.[1], happy$mean.WA.[1], happy$mean.BM.[1], happy$mean.IV.[1], happy$mean.FS.[1]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
        layout(title= "AS", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(happy, type= 'scatterpolar', fill='toself', 
        r= c(happy$mean.UB.[2], happy$mean.MB.[2], happy$mean.WA.[2], happy$mean.BM.[2], happy$mean.IV.[2], happy$mean.FS.[2]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="BS", polar = list(radialaxis = list(visible = T,range = c(0,7))), polar = list(angularaxis = list(visible = T)))

plot_ly(happy, type= 'scatterpolar', fill='tonext',
        r= c(happy$mean.UB.[3], happy$mean.MB.[3], happy$mean.WA.[3], happy$mean.BM.[3], happy$mean.IV.[3], happy$mean.FS.[3]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="DS", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(happy, type= 'scatterpolar', fill='tonext',
        r= c(happy$mean.UB.[4], happy$mean.MB.[4], happy$mean.WA.[4], happy$mean.BM.[4], happy$mean.IV.[4], happy$mean.FS.[2]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="HS", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(happy, type= 'scatterpolar', fill='tonext',
        r= c(happy$mean.UB.[5], happy$mean.MB.[5], happy$mean.WA.[5], happy$mean.BM.[5], happy$mean.IV.[5], happy$mean.FS.[2]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="VHS", polar = list(radialaxis = list(visible = T,range = c(0,7))))

#LZ hˆchstes und mittleres
plot_ly(happy, type= 'scatterpolar', fill='toself',fillcolor="half-transparent",
        r=c(happy$mean.UB.[5], happy$mean.MB.[5], happy$mean.WA.[5], happy$mean.BM.[5], happy$mean.IV.[5], happy$mean.FS.[2]),
        name='VHS',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r=c(happy$mean.UB.[1], happy$mean.MB.[1], happy$mean.WA.[1], happy$mean.BM.[1], happy$mean.IV.[1], happy$mean.FS.[1]),
    name='AS',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Lebenszufriedenheit 1",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#LZ VHS und HS
plot_ly(happy, type= 'scatterpolar', fill='toself',fillcolor="half-transparent",
        r=c(happy$mean.UB.[5], happy$mean.MB.[5], happy$mean.WA.[5], happy$mean.BM.[5], happy$mean.IV.[5], happy$mean.FS.[2]),
        name='VHS',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(happy$mean.UB.[4], happy$mean.MB.[4], happy$mean.WA.[4], happy$mean.BM.[4], happy$mean.IV.[4], happy$mean.FS.[2]),
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'),
    name="HS"
  )%>%
  layout(title="Lebenszufriedenheit 2",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#LZ HS und BS
plot_ly(happy, type= 'scatterpolar', fill='toself',fillcolor="half-transparent",
        r= c(happy$mean.UB.[4], happy$mean.MB.[4], happy$mean.WA.[4], happy$mean.BM.[4], happy$mean.IV.[4], happy$mean.FS.[2]),
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'),
    name="HS"
    )%>%
  add_trace(
   r= c(happy$mean.UB.[2], happy$mean.MB.[2], happy$mean.WA.[2], happy$mean.BM.[2], happy$mean.IV.[2], happy$mean.FS.[2]),
        name='BS',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Lebenszufriedenheit 3",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Alter Gesamt
nach_alter<- group_by(radar,Alter)
Alter<-data.frame(summarize(nach_alter, mean(UB), mean(MB), mean(WA), mean(BM), mean(IV), mean(FS)))

plot_ly(Alter, type= 'scatterpolar', fill='tonext',
        r= c(Alter$mean.UB.[1], Alter$mean.MB.[1], Alter$mean.WA.[1], Alter$mean.BM.[1], Alter$mean.IV.[1], Alter$mean.FS.[1]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="20-30", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(Alter, type= 'scatterpolar', fill='tonext',
        r= c(Alter$mean.UB.[2], Alter$mean.MB.[2], Alter$mean.WA.[2], Alter$mean.BM.[2], Alter$mean.IV.[2], Alter$mean.FS.[2]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="31-40", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(Alter, type= 'scatterpolar', fill='tonext',
        r= c(Alter$mean.UB.[3], Alter$mean.MB.[3], Alter$mean.WA.[3], Alter$mean.BM.[3], Alter$mean.IV.[3], Alter$mean.FS.[3]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="41-50", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(Alter, type= 'scatterpolar', fill='tonext',
        r= c(Alter$mean.UB.[4], Alter$mean.MB.[4], Alter$mean.WA.[4], Alter$mean.BM.[4], Alter$mean.IV.[4], Alter$mean.FS.[4]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="51-60", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(Alter, type= 'scatterpolar', fill='tonext',
        r= c(Alter$mean.UB.[5], Alter$mean.MB.[5], Alter$mean.WA.[5], Alter$mean.BM.[5], Alter$mean.IV.[5], Alter$mean.FS.[5]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="61-70", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(Alter, type= 'scatterpolar', fill='tonext',
        r= c(Alter$mean.UB.[6], Alter$mean.MB.[6], Alter$mean.WA.[6], Alter$mean.BM.[6], Alter$mean.IV.[6], Alter$mean.FS.[6]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="71-80", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(Alter, type= 'scatterpolar', fill='tonext',
        r= c(Alter$mean.UB.[7], Alter$mean.MB.[7], Alter$mean.WA.[7], Alter$mean.BM.[7], Alter$mean.IV.[7], Alter$mean.FS.[7]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="¸ber 80", polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Alter gemeinsam

plot_ly(Alter, type= 'scatterpolar', fill='toself',
        r=c(Alter$mean.UB.[2], Alter$mean.MB.[2], Alter$mean.WA.[2], Alter$mean.BM.[2], Alter$mean.IV.[2], Alter$mean.FS.[2]),
    name='31-40',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
      r=c(Alter$mean.UB.[1], Alter$mean.MB.[1], Alter$mean.WA.[1], Alter$mean.BM.[1], Alter$mean.IV.[1], Alter$mean.FS.[1]),
        name='20-30',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  layout(title="Alter, Zielgruppe", polar = list(radialaxis = list(visible = T,range = c(0,7))))

#jung und alt
plot_ly(Alter, type= 'scatterpolar', fill='toself',
        r=c(Alter$mean.UB.[1], Alter$mean.MB.[1], Alter$mean.WA.[1], Alter$mean.BM.[1], Alter$mean.IV.[1], Alter$mean.FS.[1]),
        name='20-30',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r=c(Alter$mean.UB.[4], Alter$mean.MB.[4], Alter$mean.WA.[4], Alter$mean.BM.[4], Alter$mean.IV.[4], Alter$mean.FS.[4]),
    name='51-60',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Alter, jung/alt",polar = list(radialaxis = list(visible = T,range = c(0,7))))


#Einkommen
nach_einkommen<- group_by(radar,Einkommen)
einkommen<-data.frame(summarize(nach_einkommen, mean(UB), mean(MB), mean(WA), mean(BM), mean(IV), mean(FS)))

plot_ly(einkommen, type= 'scatterpolar', fill='tonext',
        r= c(einkommen$mean.UB.[1], einkommen$mean.MB.[1], einkommen$mean.WA.[1], einkommen$mean.BM.[1], einkommen$mean.IV.[1], einkommen$mean.FS.[1]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="1.500 bis 3.000 Euro", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(einkommen, type= 'scatterpolar', fill='tonext',
        r= c(einkommen$mean.UB.[2], einkommen$mean.MB.[2], einkommen$mean.WA.[2], einkommen$mean.BM.[2], einkommen$mean.IV.[2], einkommen$mean.FS.[2]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="3.000 bis 4.500 Euro", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(einkommen, type= 'scatterpolar', fill='tonext',
        r= c(einkommen$mean.UB.[3], einkommen$mean.MB.[3], einkommen$mean.WA.[3], einkommen$mean.BM.[3], einkommen$mean.IV.[3], einkommen$mean.FS.[3]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="4.500 bis 6.000 Euro", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(einkommen, type= 'scatterpolar', fill='tonext',
        r= c(einkommen$mean.UB.[4], einkommen$mean.MB.[4], einkommen$mean.WA.[4], einkommen$mean.BM.[4], einkommen$mean.IV.[4], einkommen$mean.FS.[4]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="6.000 bis 7.500 Euro", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(einkommen, type= 'scatterpolar', fill='tonext',
        r= c(einkommen$mean.UB.[5], einkommen$mean.MB.[5], einkommen$mean.WA.[5], einkommen$mean.BM.[5], einkommen$mean.IV.[5], einkommen$mean.FS.[5]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="7.500 bis 9.000 Euro", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(einkommen, type= 'scatterpolar', fill='tonext',
        r= c(einkommen$mean.UB.[6], einkommen$mean.MB.[6], einkommen$mean.WA.[6], einkommen$mean.BM.[6], einkommen$mean.IV.[6], einkommen$mean.FS.[6]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Keine Angabe", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(einkommen, type= 'scatterpolar', fill='tonext',
        r= c(einkommen$mean.UB.[7], einkommen$mean.MB.[7], einkommen$mean.WA.[7], einkommen$mean.BM.[7], einkommen$mean.IV.[7], einkommen$mean.FS.[7]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Mehr als 9.000 Euro", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(einkommen, type= 'scatterpolar', fill='tonext',
        r= c(einkommen$mean.UB.[8], einkommen$mean.MB.[8], einkommen$mean.WA.[8], einkommen$mean.BM.[8], einkommen$mean.IV.[8], einkommen$mean.FS.[8]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Weniger als 1.500 Euro", polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Einkommen Geringverdiener/Gutverdiener
plot_ly(einkommen, type= 'scatterpolar', fill='toself',
        r= c(einkommen$mean.UB.[8], einkommen$mean.MB.[8], einkommen$mean.WA.[8], einkommen$mean.BM.[8], einkommen$mean.IV.[8], einkommen$mean.FS.[8]),
        name='Weniger als 1.500 Euro',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(einkommen$mean.UB.[1], einkommen$mean.MB.[1], einkommen$mean.WA.[1], einkommen$mean.BM.[1], einkommen$mean.IV.[1], einkommen$mean.FS.[1]),
    name='3.000 bis 4.500',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Einkommen, niedrig/mittel",polar = list(radialaxis = list(visible = T,range = c(0,7))))


#Wohnort
nach_ort<- group_by(radar,Wohnort)
ort<-data.frame(summarize(nach_ort, mean(UB), mean(MB), mean(WA), mean(BM), mean(IV), mean(FS)))

plot_ly(ort, type= 'scatterpolar', fill='tonext',
        r= c(ort$mean.UB.[1], ort$mean.MB.[1], ort$mean.WA.[1], ort$mean.BM.[1], ort$mean.IV.[1], ort$mean.FS.[1]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Dorf", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(ort, type= 'scatterpolar', fill='tonext',
        r= c(ort$mean.UB.[2], ort$mean.MB.[2], ort$mean.WA.[2], ort$mean.BM.[2], ort$mean.IV.[2], ort$mean.FS.[2]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Groﬂstadt ", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(ort, type= 'scatterpolar', fill='tonext',
        r= c(ort$mean.UB.[3], ort$mean.MB.[3], ort$mean.WA.[3], ort$mean.BM.[3], ort$mean.IV.[3], ort$mean.FS.[3]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Kleinstadt", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(ort, type= 'scatterpolar', fill='tonext',
        r= c(ort$mean.UB.[4], ort$mean.MB.[4], ort$mean.WA.[4], ort$mean.BM.[4], ort$mean.IV.[4], ort$mean.FS.[4]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Stadt", polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Wohnort Groﬂstadt/Dorf
plot_ly(ort, type= 'scatterpolar', fill='toself',
        r= c(ort$mean.UB.[2], ort$mean.MB.[2], ort$mean.WA.[2], ort$mean.BM.[2], ort$mean.IV.[2], ort$mean.FS.[2]),
        name='Groﬂstadt',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(ort$mean.UB.[1], ort$mean.MB.[1], ort$mean.WA.[1], ort$mean.BM.[1], ort$mean.IV.[1], ort$mean.FS.[1]),
    name='Dorf',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Wohnort Groﬂstadt/Dorf",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Wohnort Groﬂstadt/Stadt
plot_ly(ort, type= 'scatterpolar', fill='toself',
        r= c(ort$mean.UB.[2], ort$mean.MB.[2], ort$mean.WA.[2], ort$mean.BM.[2], ort$mean.IV.[2], ort$mean.FS.[2]),
        name='Groﬂstadt',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(ort$mean.UB.[4], ort$mean.MB.[4], ort$mean.WA.[4], ort$mean.BM.[4], ort$mean.IV.[4], ort$mean.FS.[4]),
    name='Stadt',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Wohnort Groﬂstadt/Stadt",polar = list(radialaxis = list(visible = T,range = c(0,7))))


#Bildung
nach_bildung<- group_by(radar,Bildung)
bildung<-data.frame(summarize(nach_bildung, mean(UB), mean(MB), mean(WA), mean(BM), mean(IV), mean(FS)))

plot_ly(bildung, type= 'scatterpolar', fill='tonext',
        r= c(bildung$mean.UB.[1], bildung$mean.MB.[1], bildung$mean.WA.[1], bildung$mean.BM.[1], bildung$mean.IV.[1], bildung$mean.FS.[1]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Abgeschlossene Ausbildung", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(bildung, type= 'scatterpolar', fill='tonext',
        r= c(bildung$mean.UB.[2], bildung$mean.MB.[2], bildung$mean.WA.[2], bildung$mean.BM.[2], bildung$mean.IV.[2], bildung$mean.FS.[2]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Abitur", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(bildung, type= 'scatterpolar', fill='tonext',
        r= c(bildung$mean.UB.[3], bildung$mean.MB.[3], bildung$mean.WA.[3], bildung$mean.BM.[3], bildung$mean.IV.[3], bildung$mean.FS.[3]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Bachelor", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(bildung, type= 'scatterpolar', fill='tonext',
        r= c(bildung$mean.UB.[4], bildung$mean.MB.[4], bildung$mean.WA.[4], bildung$mean.BM.[4], bildung$mean.IV.[4], bildung$mean.FS.[4]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Doktor", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(bildung, type= 'scatterpolar', fill='tonext',
        r= c(bildung$mean.UB.[5], bildung$mean.MB.[5], bildung$mean.WA.[5], bildung$mean.BM.[5], bildung$mean.IV.[5], bildung$mean.FS.[5]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Keine Angabe", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(bildung, type= 'scatterpolar', fill='tonext',
        r= c(bildung$mean.UB.[6], bildung$mean.MB.[6], bildung$mean.WA.[6], bildung$mean.BM.[6], bildung$mean.IV.[6], bildung$mean.FS.[6]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Master", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(bildung, type= 'scatterpolar', fill='tonext',
        r= c(bildung$mean.UB.[7], bildung$mean.MB.[7], bildung$mean.WA.[7], bildung$mean.BM.[7], bildung$mean.IV.[7], bildung$mean.FS.[7]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Mittlere Reife", polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Bildung Master Abitur
plot_ly(bildung, type= 'scatterpolar', fill='toself',
        r= c(bildung$mean.UB.[2], bildung$mean.MB.[2], bildung$mean.WA.[2], bildung$mean.BM.[2], bildung$mean.IV.[2], bildung$mean.FS.[2]),
    name='Abitur',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(bildung$mean.UB.[6], bildung$mean.MB.[6], bildung$mean.WA.[6], bildung$mean.BM.[6], bildung$mean.IV.[6], bildung$mean.FS.[6]),
        name='Master',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Bildung Master/Abitur",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Bildung Bachelor/Ausbildung
plot_ly(bildung, type= 'scatterpolar', fill='toself',
        r= c(bildung$mean.UB.[3], bildung$mean.MB.[3], bildung$mean.WA.[3], bildung$mean.BM.[3], bildung$mean.IV.[3], bildung$mean.FS.[3]),
        name='Bachelor',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(bildung$mean.UB.[1], bildung$mean.MB.[1], bildung$mean.WA.[1], bildung$mean.BM.[1], bildung$mean.IV.[1], bildung$mean.FS.[1]),
    name='Abgeschlossene Ausbildung',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Bildung Bachelor/Ausbildung",polar = list(radialaxis = list(visible = T,range = c(0,7))))



#Erwerbsstatus
nach_erwerb<- group_by(radar,Erwerbsstatus)
erwerb<-data.frame(summarize(nach_erwerb, mean(UB), mean(MB), mean(WA), mean(BM), mean(IV), mean(FS)))

plot_ly(erwerb, type= 'scatterpolar', fill='tonext',
        r= c(erwerb$mean.UB.[1], erwerb$mean.MB.[1], erwerb$mean.WA.[1], erwerb$mean.BM.[1], erwerb$mean.IV.[1], erwerb$mean.FS.[1]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Angestellt, Teilzeit", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(erwerb, type= 'scatterpolar', fill='tonext',
        r= c(erwerb$mean.UB.[2], erwerb$mean.MB.[2], erwerb$mean.WA.[2], erwerb$mean.BM.[2], erwerb$mean.IV.[2], erwerb$mean.FS.[2]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Angestellt, Vollzeit", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(erwerb, type= 'scatterpolar', fill='tonext',
        r= c(erwerb$mean.UB.[3], erwerb$mean.MB.[3], erwerb$mean.WA.[3], erwerb$mean.BM.[3], erwerb$mean.IV.[3], erwerb$mean.FS.[3]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Arbeitssuchend", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(erwerb, type= 'scatterpolar', fill='tonext',
        r= c(erwerb$mean.UB.[4], erwerb$mean.MB.[4], erwerb$mean.WA.[4], erwerb$mean.BM.[4], erwerb$mean.IV.[4], erwerb$mean.FS.[4]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Beamte/-r", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(erwerb, type= 'scatterpolar', fill='tonext',
        r= c(erwerb$mean.UB.[5], erwerb$mean.MB.[5], erwerb$mean.WA.[5], erwerb$mean.BM.[5], erwerb$mean.IV.[5], erwerb$mean.FS.[5]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Keine Angabe", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(erwerb, type= 'scatterpolar', fill='tonext',
        r= c(erwerb$mean.UB.[6], erwerb$mean.MB.[6], erwerb$mean.WA.[6], erwerb$mean.BM.[6], erwerb$mean.IV.[6], erwerb$mean.FS.[6]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Rentner/-in", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(erwerb, type= 'scatterpolar', fill='tonext',
        r= c(erwerb$mean.UB.[7], erwerb$mean.MB.[7], erwerb$mean.WA.[7], erwerb$mean.BM.[7], erwerb$mean.IV.[7], erwerb$mean.FS.[7]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Selbstst‰ndig", polar = list(radialaxis = list(visible = T,range = c(0,7))))

plot_ly(erwerb, type= 'scatterpolar', fill='tonext',
        r= c(erwerb$mean.UB.[8], erwerb$mean.MB.[8], erwerb$mean.WA.[8], erwerb$mean.BM.[8], erwerb$mean.IV.[8], erwerb$mean.FS.[8]),
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben'))%>%
  layout(title="Student/-in", polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Erwerbsstatus Student/Rentner
plot_ly(erwerb, type= 'scatterpolar', fill='toself',
        r= c(erwerb$mean.UB.[8], erwerb$mean.MB.[8], erwerb$mean.WA.[8], erwerb$mean.BM.[8], erwerb$mean.IV.[8], erwerb$mean.FS.[8]),
        name='Student',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(erwerb$mean.UB.[6], erwerb$mean.MB.[6], erwerb$mean.WA.[6], erwerb$mean.BM.[6], erwerb$mean.IV.[6], erwerb$mean.FS.[6]),
    name='Rentner',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Erwerbsstatus Student/Rentner",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Erwerbsstatus Student/Vollzeit
plot_ly(erwerb, type= 'scatterpolar', fill='toself',
        r= c(erwerb$mean.UB.[8], erwerb$mean.MB.[8], erwerb$mean.WA.[8], erwerb$mean.BM.[8], erwerb$mean.IV.[8], erwerb$mean.FS.[8]),
        name='Student',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(erwerb$mean.UB.[2], erwerb$mean.MB.[2], erwerb$mean.WA.[2], erwerb$mean.BM.[2], erwerb$mean.IV.[2], erwerb$mean.FS.[2]),
    name='Vollzeit',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Erwerbsstatus Student/Vollzeit",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Erwerbsstatus Teilzeit/Vollzeit
plot_ly(erwerb, type= 'scatterpolar', fill='toself',
        r= c(erwerb$mean.UB.[2], erwerb$mean.MB.[2], erwerb$mean.WA.[2], erwerb$mean.BM.[2], erwerb$mean.IV.[2], erwerb$mean.FS.[2]),
        name='Vollzeit',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(erwerb$mean.UB.[1], erwerb$mean.MB.[1], erwerb$mean.WA.[1], erwerb$mean.BM.[1], erwerb$mean.IV.[1], erwerb$mean.FS.[1]),
    name='Teilzeit',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Erwerbsstatus Teilzeit/Vollzeit",polar = list(radialaxis = list(visible = T,range = c(0,7))))

#Erwerbsstatus Selbstst‰ndig/Vollzeit
plot_ly(erwerb, type= 'scatterpolar', fill='toself',
        r= c(erwerb$mean.UB.[2], erwerb$mean.MB.[2], erwerb$mean.WA.[2], erwerb$mean.BM.[2], erwerb$mean.IV.[2], erwerb$mean.FS.[2]),
        name='Vollzeit',
        theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
)%>%
  add_trace(
    r= c(erwerb$mean.UB.[7], erwerb$mean.MB.[7], erwerb$mean.WA.[7], erwerb$mean.BM.[7], erwerb$mean.IV.[7], erwerb$mean.FS.[7]),
    name='Selbstst‰ndig',
    theta = c('Umweltbewusstsein','Monet‰re Beeinflussung','Wahrnehmung', 'Bereitschaft Mehraufwand', 'Imageverbesserung', 'Freiheitsstreben')
  )%>%
  layout(title="Erwerbsstatus Selbstst‰ndig/Vollzeit",polar = list(radialaxis = list(visible = T,range = c(0,7))))



#Itemanalyse####

alpha(radar[c("UB","MB","WA","BM","IV", "FS")], check.keys=TRUE)

mean(radar$MB)
mean(radar$MB1nr)
mean(radar$MB2nr)
mean(radar$MB3nBM4nr)

sd(radar$MB)
sd(radar$MB1nr)
sd(radar$MB2nr)
sd(radar$MB3nBM4nr)

alpha(radar[c("MB1nr", "MB2nr", "MB3nBM4nr")], check.keys=TRUE)

mean(radar$WA)
mean(radar$WA1p)
mean(radar$WA2nr)
mean(radar$WA3nr)

sd(radar$WA)
sd(radar$WA1p)
sd(radar$WA2nr)
sd(radar$WA3nr)

alpha(radar[c("WA1p", "WA2nr", "WA3nr")], check.keys=TRUE)

mean(radar$BM)
mean(radar$BM1p)
mean(radar$BM2p)
mean(radar$BM3nr)
mean(radar$MB3nBM4nr)

sd(radar$BM)
sd(radar$BM1p)
sd(radar$BM2p)
sd(radar$BM3nr)
sd(radar$MB3nBM4nr)

alpha(radar[c("BM1p", "BM2p", "BM3nr", "MB3nBM4nr")], check.keys=TRUE)

mean(radar$IV)
mean(radar$IV1nr)
mean(radar$IV2nr)
mean(radar$IV3nr)
mean(radar$IV4nr)

sd(radar$IV)
sd(radar$IV1nr)
sd(radar$IV2nr)
sd(radar$IV3nr)
sd(radar$IV4nr)

alpha(radar[c("IV1nr", "IV2nr", "IV3nr", "IV4nr")], check.keys=TRUE)

mean(radar$FS)
mean(radar$FS1nr)
mean(radar$FS2nr)
mean(radar$FS3nr)

sd(radar$FS)
sd(radar$FS1nr)
sd(radar$FS2nr)
sd(radar$FS3nr)

alpha(radar[c("FS1nr", "FS2nr", "FS3nr")], check.keys=TRUE)

mean(radar$UB)
mean(radar$U1nr)
mean(radar$U2nr)
mean(radar$U3nr)
mean(radar$U4nr)

sd(radar$UB)
sd(radar$U1nr)
sd(radar$U2nr)
sd(radar$U3nr)
sd(radar$U4nr)

alpha(radar[c("U4nr", "U3nr", "U2nr", "U1nr")], check.keys=TRUE)

#Regression####
#UB

model_sexUB <- lm(UB ~ sex, radar)
lm(formula = UB ~ sex, data = radar)
summary(model_sexUB)

model_LZUB <- lm(UB ~ LZ, radar)
lm(formula = UB ~ LZ, data = radar)
summary(model_LZUB)

model_AlterUB <- lm(UB ~ Alter, radar)
lm(formula = UB ~ Alter, data = radar)
summary(model_AlterUB)

model_bildungUB <- lm(UB ~ Bildung, radar)
lm(formula = UB ~ Bildung, data = radar)
summary(model_bildungUB)

model_einkommenUB <- lm(UB ~ Einkommen, radar)
lm(formula = UB ~ Einkommen, data = radar)
summary(model_einkommenUB)

model_erwerbUB <- lm(UB ~ Erwerbsstatus, radar)
lm(formula = UB ~ Erwerbsstatus, data = radar)
summary(model_erwerbUB)

model_ortUB <- lm(UB ~ Wohnort, radar)
lm(formula = UB ~ Wohnort, data = radar)
summary(model_ortUB)

model_nudgeUB <- lm(UB ~ zust, radar)
lm(formula = UB ~ zust, data = radar)
summary(model_nudgeUB)

#Schrittweise Regression####
auswahl = na.omit(data.frame(radar$UB, radar$sex, radar$zust, radar$Wohnort, radar$Erwerbsstatus, radar$Einkommen, radar$Bildung, radar$Alter, radar$LZ))
radar2 <- subset(radar, radar$sex != "Keine Angabe")
radar2 <- subset(radarX, radarX$Einkommen != "Keine Angabe")

#UB
step.inputUB <- lm(radar2$UB ~ 1, data = auswahl)
step.outputUB <- step(step.inputUB,
                    scope =~ radar2$sex + radar2$zust + radar2$Wohnort + 
                      radar2$Erwerbsstatus + radar2$Einkommen + radar2$Bildung + 
                      radar2$Alter + radar2$LZ)
summary(step.outputUB)

#IV
step.inputIV <- lm(radar2$IV ~ 1, data = auswahl)
step.outputIV <- step(step.inputIV,
                      scope =~ radar2$sex + radar2$zust + radar2$Wohnort + 
                        radar2$Erwerbsstatus + radar2$Einkommen + radar2$Bildung + 
                        radar2$Alter + radar2$LZ)
summary(step.outputIV)

#FS
step.inputFS <- lm(radar2$FS ~ 1, data = auswahl)
step.outputFS <- step(step.inputFS,
                      scope =~ radar2$sex + radar2$zust + radar2$Wohnort + 
                        radar2$Erwerbsstatus + radar2$Einkommen + radar2$Bildung + 
                        radar2$Alter + radar2$LZ)
summary(step.outputFS)

contrasts(factor(radar$sex))

#MB
step.inputMB <- lm(radar2$MB ~ 1, data = auswahl)
step.outputMB <- step(step.inputMB,
                      scope =~ radar2$sex + radar2$zust + radar2$Wohnort + 
                        radar2$Erwerbsstatus + radar2$Einkommen + radar2$Bildung + 
                        radar2$Alter + radar2$LZ)
summary(step.outputMB)

#WA
step.inputWA <- lm(radar2$WA ~ 1, data = auswahl)
step.outputWA <- step(step.inputWA,
                      scope =~ radar2$sex + radar2$zust + radar2$Wohnort + 
                        radar2$Erwerbsstatus + radar2$Einkommen + radar2$Bildung + 
                        radar2$Alter + radar2$LZ)
summary(step.outputWA)

#BM
step.inputBM <- lm(radar2$BM ~ 1, data = auswahl)
step.outputBM <- step(step.inputBM,
                      scope =~ radar2$sex + radar2$zust + radar2$Wohnort + 
                        radar2$Erwerbsstatus + radar2$Einkommen + radar2$Bildung + 
                        radar2$Alter + radar2$LZ)
summary(step.outputBM)

t.test(radar$SW1, radar$SW2, paired = TRUE, alternative = "greater")
mean(radar$SW1)
mean(radar$SW2)

#Residuen####
radar$ResiduenWA <- dffits(step.outputWA)


par(mfrow = c(2,2))
plot(step.outputWA)
plot(step.outputBM)
plot(step.outputFS)
plot(step.outputIV)
plot(step.outputMB)
plot(step.outputUB)
mean(radar$LZ)

