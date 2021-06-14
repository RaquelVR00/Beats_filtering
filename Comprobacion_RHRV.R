library("lubridate")
library("RHRV")

#Inicializacion
especificidad_final=0
sensibilidad_final=0
numero_registro=0
matriz_suma=matrix(0,nrow=2,ncol=2)


Resultados = "Resultados_RHRV.txt"


b<-as.vector(list.files())
c=length(b)/2
  
for(i in 1:c){
  setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")
  
  numero_registro=numero_registro+1
  #Funcionara para ir llamando a los archivos 
  
  if(numero_registro<10){
    nombre_file=paste("I0",numero_registro,"_ann.txt",sep="")
  }else{
    nombre_file=paste("I",numero_registro,"_ann.txt",sep="")
  }
  
  
  file_name =nombre_file
  dat <- read.table(file_name_anotaciones, skip=1, stringsAsFactors = FALSE, fill=TRUE, header=FALSE)
  # 257 is the sampling freq
  
  heartbeats <- dat[, 2] / 257
  annotations_df <- data.frame(Time = heartbeats, annotation = dat[, 3])
  
  
  
  hd <- LoadBeatVector(CreateHRVData(), heartbeats)
  hd <- BuildNIHR(hd)
  hd <- FilterNIHR(hd)
  filtered_df <- merge(hd$Beat, annotations_df, by='Time', all.y = TRUE)
  filtered_df$filtered <- is.na(filtered_df$RR)
  # Remove useless columns
  filtered_df$niHR <- NULL
  filtered_df$RR <- NULL
  
  
  truth_filtrados_vector<-c()
  estimate_filtrados<-c()
  
  for(z in 1:length(heartbeats)){
    if(filtered_df$filtered[z]== "FALSE"){
      estimate_filtrados[z]<- "0"
    }else{
      estimate_filtrados[z]<-"1"
    }
  }
  
  for(m in 1:length(heartbeats)){
    if((filtered_df$annotation[m]== "N")|(filtered_df$annotation[m]=="F")|(filtered_df$annotation[m]=="R"|filtered_df$annotation[m]=="A")){
      truth_filtrados_vector[m]<- "0"
    }else{
      truth_filtrados_vector[m]<-"1"
    }
  }
  
 
  df = data.frame('reference' = factor(truth_filtrados_vector, levels=c('0','1')), 'predictions' = factor(estimate_filtrados,levels=c('0','1')))
  
  matriz_confusion_tabla = conf_mat(df, truth = reference, estimate = predictions)
  matriz_confusion=as.matrix(matriz_confusion_tabla$table)
  matriz_suma=matriz_suma+matriz_confusion
  
} 

sensibilidad=(matriz_suma[2,2]/(matriz_suma[2,2]+matriz_suma[1,2]))*100
especificidad=(matriz_suma[1,1]/(matriz_suma[1,1]+matriz_suma[2,1]))*100

setwd("C:/Users/RAQUEL/Desktop/RHRV/Outputs")
#Directorio donde deseo guardar la tabla de resultados 

cat(name, file=Resultados, append=TRUE, sep = "\n")
cat("especificidad", file=Resultados, append=TRUE, sep = "\n")
cat(especificidad, file=Resultados, append=TRUE, sep = "\n")
cat("sensibilidad", file=Resultados, append=TRUE, sep = "\n")
cat(sensibilidad, file=Resultados, append=TRUE, sep = "\n")
cat(matriz_suma, file=Resultados, append=TRUE)
cat(" ", file=Resultados, append=TRUE, sep = "\n")
cat(" ", file=Resultados, append=TRUE, sep = "\n")


