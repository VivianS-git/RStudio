library(readxl)
library(plotly)
library(ggplot2)
library(dplyr)


daten_vivi<- read_excel("C:/Users/Besitzer/Dropbox/Master/Fragebogen/Codierung BFI.xlsx", col_names=TRUE)



#A1 anpassen

daten_vivi$A2r<-"r"
i=1
for(i in 1:length(daten_vivi$A1))
{
  if (daten_vivi$A2[i]==5){
    daten_vivi$A2r[i]<-1
  }
  if (daten_vivi$A2[i]==3){
    daten_vivi$A2r[i]<-3
  }
  if(daten_vivi$A2[i]==4){
    daten_vivi$A2r[i]<-2
  } 
  if (daten_vivi$A2[i]==2){
    daten_vivi$A2r[i]<- 4
  }
  if (daten_vivi$A2[i]==1){
    daten_vivi$A2r[i]<- 5
  }
}



#B1 anpassen

daten_vivi$B1r<-"r"

i=1
for(i in 1:length(daten_vivi$B1))
{
  if (daten_vivi$B1[i]==5){
    daten_vivi$B1r[i]<-1
  }
  if (daten_vivi$B1[i]==3){
    daten_vivi$B1r[i]<-3
  }
  if(daten_vivi$B1[i]==4){
    daten_vivi$B1r[i]<-2
  } 
  if (daten_vivi$B1[i]==2){
    daten_vivi$B1r[i]<- 4
  }
  if (daten_vivi$B1[i]==1){
    daten_vivi$B1r[i]<- 5
  }
}



#C1 anpassen

daten_vivi$C1r<-"r"
i=1
for(i in 1:length(daten_vivi$C1))
{
  if (daten_vivi$C1[i]==5){
    daten_vivi$C1r[i]<-1
  }
  if (daten_vivi$C1[i]==3){
    daten_vivi$C1r[i]<-3
  }  
  if(daten_vivi$C1[i]==4){
    daten_vivi$C1r[i]<-2
  } 
  if (daten_vivi$C1[i]==2){
    daten_vivi$C1r[i]<- 4
  }
  if (daten_vivi$C1[i]==1){
    daten_vivi$C1r[i]<- 5
  }
}



#D1 anpassen

daten_vivi$D1r<-"r"
i=1
for(i in 1:length(daten_vivi$D1))
{
  if (daten_vivi$D1[i]==5){
    daten_vivi$D1r[i]<-1
  }
  if (daten_vivi$D1[i]==3){
    daten_vivi$D1r[i]<-3
  }  
  if(daten_vivi$D1[i]==4){
    daten_vivi$D1r[i]<-2
  } 
  if (daten_vivi$D1[i]==2){
    daten_vivi$D1r[i]<- 4
  }
  if (daten_vivi$D1[i]==1){
    daten_vivi$D1r[i]<- 5
  }
}



#E1 anpassen

daten_vivi$E1r<-"r"
i=1
for(i in 1:length(daten_vivi$E1))
{
  if (daten_vivi$E1[i]==5){
    daten_vivi$E1r[i]<-1
  }
  if (daten_vivi$E1[i]==3){
    daten_vivi$E1r[i]<-3
  } 
  if(daten_vivi$E1[i]==4){
    daten_vivi$E1r[i]<-2
  } 
  if (daten_vivi$E1[i]==2){
    daten_vivi$E1r[i]<- 4
  }
  if (daten_vivi$E1[i]==1){
    daten_vivi$E1r[i]<- 5
  }
}

daten_vivi$A1<- as.numeric(daten_vivi$A1)
daten_vivi$A2r<- as.numeric(daten_vivi$A2r)
daten_vivi$B1r<- as.numeric(daten_vivi$B1r)
daten_vivi$B2<- as.numeric(daten_vivi$B2)
daten_vivi$C1r<- as.numeric(daten_vivi$C1r)
daten_vivi$C2<- as.numeric(daten_vivi$C2)
daten_vivi$D1r<- as.numeric(daten_vivi$D1r)
daten_vivi$D2<- as.numeric(daten_vivi$D2)
daten_vivi$E1r<- as.numeric(daten_vivi$E1r)
daten_vivi$E2<- as.numeric(daten_vivi$E2)

daten_vivi$aggree<- daten_vivi$A1 + daten_vivi$A2r
daten_vivi$consc <- daten_vivi$B1r + daten_vivi$B2
daten_vivi$extra <- daten_vivi$C1r + daten_vivi$C2
daten_vivi$neuro <- daten_vivi$D1r + daten_vivi$D2
daten_vivi$open <-  daten_vivi$E1r + daten_vivi$E2

plot_ly(daten_vivi,  type= 'scatterpolar', 
        r=c(mean(daten_vivi$aggree), mean(daten_vivi$consc), mean(daten_vivi$extra), mean(daten_vivi$neuro), mean(daten_vivi$open)),
        theta = c('Verträglichkeit','Gewissenhaftigkeit','Extraversion', 'Neurotizismus', 'Offenheit'),
        fill= 'toself')%>%
  layout(title="BFI-10 gesamt", polar = list(radialaxis = list(visible = T,range = c(0,10))))

