library("lubridate")
#Se llamaran a los registros dentro del bucle for para ir aumentando el numero
#del registro que se quiere analizar
numero_registro=0

#Para calcular la especificidad y la sensibilidad final
#(Se irá realizando un sumatorio de la sensibilidad y especificidad que se va calculando
#por registro)
especificidad_final=0
sensibilidad_final=0


setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")
#Directorio donde están los archivos rr y ann

b<-as.vector(list.files())
c=length(b)/2
#En este directorio tengo los archivos de anotaciones y de RR es por ello que
#es necesario dividir la lenght del directorio entre dos

for(i in 1:c){

numero_registro=numero_registro+1
#Funcionara para ir llamando a los archivos 

if(numero_registro<10){
nombre_file=paste("I0",numero_registro,"_rr.txt",sep="")
}else{
  nombre_file=paste("I",numero_registro,"_rr.txt",sep="")
}
nombre_file
file_name =nombre_file
tabla_RR <- read.table(file_name, header =FALSE)
vector_RR <- c(length(tabla_RR$v1))

vector_RR <- (tabla_RR$V1)*1000
vector_flags=final_flags=rep('0',length(vector_RR))


vector_tiempos_n <- c(length(tabla_RR$V1))
vector_tiempos_n[1]<-vector_RR[1]
for( i in 1:length(vector_RR)){
  vector_tiempos_n[i+1]<-vector_tiempos_n[i]+vector_RR[i+1]
}
time_format_n <- c(length(vector_tiempos_n))
for(a in 1:length(vector_tiempos_n)-1){
  minute = (vector_tiempos_n[a])/1000/60
  MM = floor(minute)
  segundo = (minute-floor(minute))*60
  ss = floor(segundo)
  milisegundo = (segundo-floor(segundo))*1000
  mm= floor(milisegundo)
  time_format_n[a] = paste(c(MM,ss,mm), collapse = ":")
}


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
l<-NULL
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
      vector_flags[l+1]<-"1"
    final_flags[l+1]<-" "
    }
  }else{
    vector_flags[l+1]<-"0"
  }
}




#Tabla que contiene los resultados

name = paste("Tabla_filtrados_",file_name,".txt",sep="")
tabla_datos = data.frame(time_format_n,vector_RR,vector_flags,final_flags)

setwd("C:/Users/RAQUEL/Desktop/RHRV/Outputs")
#Directorio donde deseo guardar la tabla de resultados 

write.table(tabla_datos, file= name)

name_tiempos = paste("tiempos_",file_name,".txt")
write(vector_tiempos_n, file= name_tiempos)

setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")


#Aquí estoy intentando extraer las posiciones en las que se encuentran los 
#latidos marcados para filtrar
vector_solo_ON =which(vector_flags=="1")


#Imprimir el gráfico marcando las flags
#install.packages("Rtools")
#install.packages("RHRV")

#library("RHRV") 
## .Renviron on Windows
#PATH = "c:\\cygwin\\bin"
#DYLD_LIBRARY_PATH = "c:\\cygwin\\lib"
# HRVData structure containing the heart beats
#data("HRVData")
# HRVData structure storing the results of processing the
# heart beats: the beats have been filtered, interpolated, ...
#data("HRVProcessedData")

#hrv.data = CreateHRVData()
#hrv.data = SetVerbose(hrv.data, TRUE )
#hrv.data = LoadBeatAscii(hrv.data, name_tiempos ,RecordPath = "." )
#hrv.data= BuildNIHR(hrv.data)

#windows()
#PlotNIHR(hrv.data)
#points(hrv.data$Beat$Time[ vector_solo_ON + 1], 
#       hrv.data$Beat$niHR[ vector_solo_ON + 1], col='red', bg='red', pch=22)



#Matriz de confusión
#install.packages('yardstick')
library('yardstick')
if(numero_registro<10){
  nombre_anotations=paste("I0",numero_registro,"_ann.txt",sep="")
}else{
  nombre_anotations=paste("I",numero_registro,"_ann.txt",sep="")
}
file_name_anotaciones = nombre_anotations
tabla_real <- read.table(file_name_anotaciones, skip=1, stringsAsFactors = FALSE, fill=TRUE, header=FALSE)
truth_filtrados_vector<-c()


for(m in 1:length(tabla_real$V3)){
  if((tabla_real$V3[m]== "N")|(tabla_real$V3[m]=="F")|(tabla_real$V3[m]=="R")){
    truth_filtrados_vector[m]<- "0"
  }else{
    truth_filtrados_vector[m]<-"1"
  }
}

estimate_filtrados <- vector_flags[1:length(truth_filtrados_vector)]
df = data.frame('reference' = factor(truth_filtrados_vector, levels=c('0','1')), 'predictions' = factor(estimate_filtrados,levels=c('0','1')))

matriz_confusion = conf_mat(df, truth = reference, estimate = predictions)

#Si comentamos el bucle for e insertamos directamente el nombre del registro 
#tanto en la linea 180 como en la 25
#se mostrara la matriz de confusión por pantalla
matriz_confusion

cm_summary = summary(matriz_confusion)
especificidad= as.data.frame(cm_summary[cm_summary$.metric=="sens", ".estimate"]) 
especificidad_final = especificidad_final+especificidad
sensibilidad = as.data.frame(cm_summary[cm_summary$.metric=="spec", ".estimate"]) 
sensibilidad_final = sensibilidad_final+sensibilidad

}

#Mostramos por pantalla la media de la sensibilidad y especificidad obtenida
#Dividimos entre 74 porque es el número de registros
sensibilidad_final/74
especificidad_final/74
