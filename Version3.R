
library("lubridate")
#Se llamaran a los registros dentro del bucle for para ir aumentando el numero
#del registro que se quiere analizar
numero_registro=0
change<-c(0.1,0.15,0.25,0.2,0.25,0.30,0.35,0.40,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1,2,3)

vector_sensibilidad=vector_especificidad=rep('0',length(change))


Resultados = "ResultadosV3.txt"
#write("Results ", file=Resultados, append=FALSE)

cat("This is a log file for Results ", file=Resultados, append=FALSE, sep = "\n")


#Matriz para sumatorio

#Para calcular la especificidad y la sensibilidad final
#(Se ir� realizando un sumatorio de la sensibilidad y especificidad que se va calculando
#por registro)
especificidad_final=0
sensibilidad_final=0

#Directorio donde solo estan los registros rr y ann
setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")

#Como tenemos por cada registro dos archivos de texto
#rr y ann del total de registros obtenidos
#quitamos la mitad para obtener el numero de registros que hay
b<-as.vector(list.files())
c=length(b)/2
  
#Comenzamos el bucle por el que pasar�n todos los registros
for (j in 1:length(change)){
  setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")
  numero_registro=0
  matriz_suma=matrix(0,nrow=2,ncol=2)
  
for(i in 1:c){
    
    #Aqui vamos aumentando el n�mero de registro
    
    numero_registro=numero_registro+1
  
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
  vector_flags2=final_flags2=rep('0',length(vector_RR))
  vector_flags_finales=final_flags=rep('0',length(vector_RR))
  
  
  
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
  MED=MED*change[j]
  
  
  
  #Algortimo para detectar latidos prematuros
  #En este caso pasaremos el algoritmo desarrollado para deteccion de prematuros
  #sobre el vector_RR sin artefactos, guardaremos las flags obtenidas en un nuevo
  #vector_flags
  
  l<-NULL
  for(l in 1:(length(vector_RR_2)-3)){
    beat_evaluated = vector_RR_2[l+1]
    
    if((beat_evaluated-vector_RR_2[l])<(-MED)){
      if((beat_evaluated-vector_RR_2[l+2])<(-MED)){
        
        vector_flags2[l+1]<-"1"
        final_flags2[l+1]<-" "
      }
    }else{
      vector_flags2[l+1]<-"0"
    }
  }
  vector_flags_mapping=rep('0',length(vector_RR))
  n=0
  
  #Con el fin de obtener de vuelta un vector_flags con las flags en las posiciones
  #correspondientes en el vector_RR inicial realizamos un "mapping".
  #Buscamos aquellas posiciones donde el vector_flags2 esta encendido "1",
  #a continuacion buscamos a que posicion en el vector_RR incial
  #corresponde el RR marcado como flag, para ello
  #vamos al vector_solo_off el cual contiene las posiciones reales en el vector_RR 
  #inicial
  
  for(k in 1:(length(vector_RR)-3)){
    if(vector_flags2[k]=="1"){
      n=vector_solo_Off[k]
      vector_flags_mapping[n]="1"
    }
    
  }
  k=0
  
  #Para obtener finalmente un vector_flags com�n realizamos un vector_flags_finales
  #el cual tendr� un "1" en aquellas posiciones donde vector_flags (salida de bersnton) o 
  #vector_flags_finales (salida de prematuros para aquells RR que no fueron artefactos)
  # sea 1
  
  for(k in 1:(length(vector_RR)-3)){
    if(vector_flags[k]=="1"||vector_flags_mapping[k]=="1")
      vector_flags_finales[k]<-"1"
  }
  
  
  #Tabla que contiene los resultados
  name = paste("Tabla_filtrados_",file_name,".txt",sep="")
  tabla_datos = data.frame(time_format_n,vector_RR,vector_flags_finales,final_flags)
  setwd("C:/Users/RAQUEL/Desktop/RHRV/Outputs")
  #Directorio donde deseo guardar la tabla de resultados 
  
  write.table(tabla_datos, file= name)
  
  name_tiempos = paste("tiempos_",file_name,".txt")
  write(vector_tiempos_n, file= name_tiempos)
  
  setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")
  
  #El codigo comentado a continuaci�n se usaba para representar usando RHRV
  #Aqu� estoy intentando extraer las posiciones en las que se encuentran los 
  #latidos marcados para filtrar
  #vector_solo_ON =which(vector_flags=="1")
  
  #Imprimir el gr�fico marcando las flags
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
  
  
  
  #Matriz de confusi�n
  #install.packages('yardstick')
  library('yardstick')
  if(numero_registro<10){
    nombre_anotations=paste("I0",numero_registro,"_ann.txt",sep="")
  }else{
    nombre_anotations=paste("I",numero_registro,"_ann.txt",sep="")
  }
  file_name_anotaciones = nombre_anotations
  tabla_real <- read.table(file_name_anotaciones, skip = 1, stringsAsFactors = FALSE, header=FALSE, fill=TRUE)
  truth_filtrados_vector<-c()
  
  
  for(m in 1:length(tabla_real$V3)){
    if((tabla_real$V3[m]== "N")|(tabla_real$V3[m]=="F")|(tabla_real$V3[m]=="R")){
      truth_filtrados_vector[m]<- "0"
    }else{
      truth_filtrados_vector[m]<-"1"
    }
  }
  
  estimate_filtrados <- vector_flags_finales[1:length(truth_filtrados_vector)]
  df = data.frame('reference' = factor(truth_filtrados_vector, levels=c('0','1')), 'predictions' = factor(estimate_filtrados,levels=c('0','1')))

  matriz_confusion_tabla = conf_mat(df, truth = reference, estimate = predictions)
  
  #Si comentamos el bucle for e insertamos directamente el nombre del registro 
  #tanto en la linea 34 como en la 223
  #se mostrara la matriz de confusi�n por pantalla
  matriz_confusion=as.matrix(matriz_confusion_tabla$table)
  matriz_suma=matriz_suma+matriz_confusion
}


matriz_suma
sensibilidad=(matriz_suma[2,2]/(matriz_suma[2,2]+matriz_suma[1,2]))*100
sensibilidad
especificidad=(matriz_suma[1,1]/(matriz_suma[1,1]+matriz_suma[2,1]))*100
especificidad

name = paste("Resultados_",change[j],"_V3",".txt",sep="")
vector_especificidad[j]= especificidad
vector_sensibilidad[j]= sensibilidad


setwd("C:/Users/RAQUEL/Desktop/RHRV/Outputs")
#Directorio donde deseo guardar la tabla de resultados 

#Directorio donde deseo guardar la tabla de resultados 
matriz_suma

#write(name, file=Resultados, append=TRUE)
cat(name, file=Resultados, append=TRUE, sep = "\n")
cat("especificidad", file=Resultados, append=TRUE, sep = "\n")
cat(especificidad, file=Resultados, append=TRUE, sep = "\n")
cat("sensibilidad", file=Resultados, append=TRUE, sep = "\n")
cat(sensibilidad, file=Resultados, append=TRUE, sep = "\n")
cat(matriz_suma, file=Resultados, append=TRUE)
cat(" ", file=Resultados, append=TRUE, sep = "\n")
cat(" ", file=Resultados, append=TRUE, sep = "\n")

}

par(mfrow=c(1,2))
plot(change,vector_sensibilidad,type = "l",col="blue",ylab="sensibilidad",xlab="n*MED")
plot(change,vector_especificidad,type = "l",col="red",ylab="especificidad",xlab="n*MED")