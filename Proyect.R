library("lubridate")

setwd("C:/Users/RAQUEL/Desktop/RHRV/Files")

file_name = "101_times.txt"
vector_tiempos <- read.table(file_name, header =FALSE)

#Función que pasa el tiempo a formato MM:ss:mm 
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
vector_flags=final_flags=rep('0',length(vector_RR))


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
    vector_flags[l+1] <-("1")
    if(abs(beat_evaluated-vector_RR[l])<Criteria){
      if(beat_evaluated-vector_RR[l+2]<0){
        if(abs(vector_RR[l+2]-vector_RR[l+3])<Criteria){
          x <- beat_evaluated/2
          if(x-vector_RR[l+2]<(-Criteria) && x-vector_RR[l]<(-Criteria)){
            final_flags[l+1] <- ("1")
            vector_flags[l+1]<-("0")
          }
        }else{final_flags[l+1]<- "Can not evaluate"}
        
      }else{
        if(abs(vector_RR[l+2]-vector_RR[l+3])<Criteria){
          if(vector_RR[l]<vector_RR[l+2]){
            x2=vector_RR[l]+beat_evaluated
          }else{ x2= vector_RR[l+2]+beat_evaluated}
          if(x2-vector_RR[l]>Criteria && x2-vector_RR[l+2]>Criteria){
            final_flags[l+1] <-"1"
            vector_flags[l+1] <-"0"
          }
        }else{ final_flags[l+1] <- "Can not evaluate"}
      }
    }else{ final_flags[l+1]<- "Can not evaluate"}
  }else{ vector_flags[l+1]<-("0")}
  
}

vector_solo_Off =which(vector_flags=="0")
l<-null
vector_RR_2<-c()
n=0
for(l in 1:(length(vector_solo_Off))){
  vector_RR_2[n]<-vector_RR[vector_solo_Off[l]]
  n=n+1
}

Criteria= ((mean(vector_RR_2)-2.9*(IQR(vector_RR_2)/2))/3+3.32*(IQR(vector_RR_2)/2))/2

MED=  3.32*(IQR(vector_RR_2)/2)

#Algortimo para detectar latidos prematuros

l<-NULL
for(l in 1:(length(vector_RR)-3)){
  beat_evaluated = vector_RR[l+1]
  if((beat_evaluated-vector_RR[l])<(-MED)){
    if((beat_evaluated-vector_RR[l+2])<(-MED)){
      
      if(vector_RR[l+2]-vector_RR[l+3]>Criteria){
        vector_flags[l+1]<-"1"
        final_flags[l+1]<-" "
      }
    }
  }else{
    vector_flags[l+1]<-"0"
  }
}




#Tabla que contiene los resultados
name = paste("Tabla_filtrados_",file_name,".txt")
tabla_datos = data.frame(time_format,vector_RR,vector_flags,final_flags)
write.table(tabla_datos, file= name)



#Aquí estoy intentando extraer las posiciones en las que se encuentran los 
#latidos marcados para filtrar
vector_solo_ON =which(vector_flags=="1")

#Imprimir el gráfico marcando las flags
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
hrv.data = LoadBeatAscii(hrv.data, file_name ,RecordPath = "." )
hrv.data= BuildNIHR(hrv.data)

windows()
PlotNIHR(hrv.data)
points(hrv.data$Beat$Time[ vector_solo_ON + 1], 
       hrv.data$Beat$niHR[ vector_solo_ON + 1], col='red', bg='red', pch=22)

#Matriz de confusión
#install.packages('yardstick')
library('yardstick')
file_name_anotaciones = "101_Annotations.txt"
tabla_real = read.table(file_name_anotaciones, header =TRUE)
truth_filtrados_vector<-c()
m<-NULL
for(m in 1:length(tabla_real$TYPE)){
  if((tabla_real$TYPE[m]== "N")|(tabla_real$TYPE[m]=="F")){
    truth_filtrados_vector[m]<- "0"
  }else{
    truth_filtrados_vector[m]<-"1"
  }
}

estimate_filtrados <- vector_flags[1:length(truth_filtrados_vector)]
df = data.frame('reference' = factor(truth_filtrados_vector, levels=c('0','1')), 'predictions' = factor(estimate_filtrados,levels=c('0','1')))
Matriz_confusion = conf_mat(df, truth = reference, estimate = predictions)
Matriz_confusion



