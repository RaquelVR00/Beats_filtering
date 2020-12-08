library("lubridate")

setwd("C:/Users/RAQUEL/Desktop/RHRV")

file_name = "113_times.txt"
vector_tiempos <- read.table(file_name, header =FALSE)

#Funcion que pasa el tiempo a formato MM:ss:mm 
time_format <- c(length(vector_tiempos$V1))
for(a in 1:length(vector_tiempos$V1)-1){
  minute = (vector_tiempos$V1[a])/60
  MM = floor(minute)
  segundo = (minute-floor(minute))*60
  ss = floor(segundo)
  milisegundo = (segundo-floor(segundo))*1000
  mm= floor(milisegundo)
  time_format[a] = paste(c(MM,ss,mm), collapse = ":")
}

#Me interesa crear cuantiles para saber cuanto va a poder variar mis R-R
#Para ello miro la diferencia entre latidos
vector_RR <- diff(vector_tiempos$V1)*1000
vector_flags=final_flags=rep('',length(vector_RR))


#Ahora que tengo el vector_RR que contiene el tiempo que transcurre entre los
#latidos calculo los criterios necesarios para evaluar despues los latidos
#Maximum expected beat MED= 3.32*(IQR(vector_RR)/2)
#Minimum artifact difference 
#SEB= mean(vector_RR)-2.9*(IQR(vector_RR)/2) #Shortest veridical beat
#MAD=SEB/3
Criteria= ((mean(vector_RR)-2.9*(IQR(vector_RR)/2))/3+3.32*(IQR(vector_RR)/2))/2



#Algoritmo de bertson (pag 594,595,596)

l <- NULL
for(l in 1:(length(vector_RR)-3)){
  beat_evaluated = vector_RR[l+1]
  if(abs(beat_evaluated-vector_RR[l+2])>Criteria){
    vector_flags[l+1] <-("ON")
    if(abs(beat_evaluated-vector_RR[l])<Criteria){
      if(beat_evaluated-vector_RR[l+2]<0){
        if(abs(vector_RR[l+2]-vector_RR[l+3])<Criteria){
          x <- beat_evaluated/2
          if(x-vector_RR[l+2]<(-Criteria) && x-vector_RR[l]<(-Criteria)){
            final_flags[l+1] <- ("ON")
            vector_flags[l+1]<-("OFF")
          }
        }else{final_flags[l+1]<- "Can not evaluate"}
        
      }else{
        if(abs(vector_RR[l+2]-vector_RR[l+3])<Criteria){
          if(vector_RR[l]<vector_RR[l+2]){
            x2=vector_RR[l]+beat_evaluated
          }else{ x2= vector_RR[l+2]+beat_evaluated}
          if(x2-vector_RR[l]>Criteria && x2-vector_RR[l+2]>Criteria){
            final_flags[l+1] <-"ON"
            vector_flags[l+1] <-"OFF"
          }
        }else{ final_flags[l+1] <- "Can not evaluate"}
      }
    }else{ final_flags[l+1]<- "Can not evaluate"}
  }else{ vector_flags[l+1]<-("Normal")}
  
  
}

#Segundo algoritmo de bernston que filtra más
#l <- NULL
f#or(l in 1:(length(vector_RR)-3)){
 # beat_evaluated = vector_RR[l+1]
 # if(abs(beat_evaluated-vector_RR[l+2])>Criteria){
 #   vector_flags[l+1] <-("ON")
 # }else{ vector_flags[l+1]<-("Normal")}
#}

#vector_solo_ON =which(vector_flags=="ON")


#for(l in 1:(length(vector_solo_ON)-3)){
#  for(z in 1:(length(vector_solo_ON)-3)){
#    if(vector_solo_ON[z]==l){
#      beat_evaluated=vector_RR[l+1]
#      if(abs(beat_evaluated-vector_RR[l])<Criteria){
#        if(beat_evaluated-vector_RR[l+2]<0){
#          if(abs(vector_RR[l+2]-vector_RR[l+3])<Criteria){
#            x <- beat_evaluated/2
#            if(x-vector_RR[l+2]<(-Criteria) && x-vector_RR[l]<(-Criteria)){
#              final_flags[l+1] <- ("ON")
#              vector_flags[l+1]<-("OFF")
#            }
#          }else{final_flags[l+1]<- "Can not evaluate"}
          
#        }
#      }else{ final_flags[l+1]<- "Can not evaluate"}
#    }
#  }
#}
#for(l in 1:(length(vector_solo_ON)-3)){
#  for(o in 1:(length(vector_solo_ON)-3)){
#    if(vector_solo_ON[l]==l){
#      beat_evaluated=vector_RR[l+1]
#      if(abs(beat_evaluated-vector_RR[l])<Criteria){
#         if(beat_evaluated-vector_RR[l+2]>0){
#          if(abs(vector_RR[l+2]-vector_RR[l+3])<Criteria){
#            if(vector_RR[l]<vector_RR[l+2]){
#              x2=vector_RR[l]+beat_evaluated
#            }else{ x2= vector_RR[l+2]+beat_evaluated}
#            if(x2-vector_RR[l]>Criteria && x2-vector_RR[l+2]>Criteria){
#              final_flags[l+1] <-"ON"
#              vector_flags[l+1] <-"OFF"
#            }
#          }else{ final_flags[l+1] <- "Can not evaluate"}
#        }
#      }else{ final_flags[l+1]<- "Can not evaluate"}
#    }
#  }
#}


#MED CRITERIA
MED=  3.32*(IQR(vector_RR)/2)
MAD=(mean(vector_RR)-2.9*(IQR(vector_RR)/2))/3
for(e in 1:(length(vector_RR)-3)){
  beat_evaluated_2= vector_RR[e+1]
  if(abs(vector_RR[e+2]-beat_evaluated)<MED){
    if(abs(beat_evaluated-vector_RR[e])<MED){
      vector_flags[e+1]<-"ON"
    }
  }
}



#Tabla que contiene los resultados
tabla_datos = data.frame(time_format,vector_RR,vector_flags,final_flags)
write.table(tabla_datos, file="Tabla_Filtrados2.txt")



#Aqui estoy intentando extraer las posiciones en las que se encuentran los 
#beats marcados para filtrar
vector_solo_ON =which(vector_flags=="ON")

#Imprimir el grafico marcando las flags
#install.packages("Rtools")
#install.packages("RHRV")

library("RHRV") 
## .Renviron on Windows
PATH = "c:\\cygwin\\bin"
DYLD_LIBRARY_PATH = "c:\\cygwin\\lib"
# HRVData structure containing the heart beats
data("HRVData")
# HRVData structure storing the results of processing the
# heart beats: the beats have been filtered, interpolated, ...
data("HRVProcessedData")

hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE )
hrv.data = LoadBeatAscii(hrv.data, "113_times.txt",RecordPath = "." )
hrv.data= BuildNIHR(hrv.data)

windows()
PlotNIHR(hrv.data)
points(hrv.data$Beat$Time[ vector_solo_ON + 1], 
       hrv.data$Beat$niHR[ vector_solo_ON + 1], col='red', bg='red', pch=22)

vector_filtrados <- vector_tiempos$V1[-vector_solo_ON]
write.table(vector_filtrados, file="vector_Filtrados.txt", row.names = FALSE, col.names = FALSE)
hrv.data = LoadBeatAscii(hrv.data, "vector_Filtrados.txt",RecordPath = "." )

hrv.data = BuildNIHR(hrv.data)
windows()
PlotNIHR(hrv.data)
#Una vez que ya hayamos filtrado por si se quiere modificar algo más
windows()
hrv.data= EditNIHR(hrv.data)


