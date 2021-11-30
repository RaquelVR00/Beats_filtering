#Leer vector RR
library("lubridate")
library('yardstick')

#Servira para ir llamando a cada archivo de RRs
numero_registro = 0

#Carpeta donde estan los RRs modificados
setwd("C:/Users/RAQUEL/Desktop/RHRV/Modificados")


#Cuento el numero de archivos que hay en la carpeta
b<-as.vector(list.files())
c<-length(b)


#Genero las matrices suma que me ayudaran a generar la sensibilidad y
#especificidad de berntson con la base de datos usando como vector
#anotaciones B y C
vector_especificidad=vector_sensibiilidad=rep('0',length(c))
matriz_suma=matrix(0,nrow=2,ncol=2)
matriz_suma2=matrix(0,nrow=2,ncol=2)



#Recorro todos los archivos
for( j in 1:c){
  setwd("C:/Users/RAQUEL/Desktop/RHRV/Modificados")
  numero_registro=numero_registro+1
  #Funcionara para ir llamando a los archivos 
  
  if(numero_registro<10){
    nombre_file=paste("Modificados_I0",numero_registro,"_rr.txt.txt",sep="")
  }else{
    nombre_file=paste("Modificados_I",numero_registro,"_rr.txt.txt",sep="")
  }
  
  tabla_modificados <- read.table(nombre_file, skip=1, stringsAsFactors = FALSE, fill=TRUE,header=FALSE)
  #Vector RR
  vector_RR<-tabla_modificados$V2
  #vector A
  anotaciones<-tabla_modificados$V3
  #vector B
  anotaciones_solo_modificados<-tabla_modificados$V4
  #Vector C
  anotaciones_modificados_adapted<-tabla_modificados$V5
  vector_flags=final_flags=rep('0',length(vector_RR))
  
  #Pasar  por Berntson
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
          #Long beat
          if(abs(vector_RR[l+2]-vector_RR[l+3])<Criteria){
            x <- beat_evaluated/2
            if(x-vector_RR[l+2]<(-Criteria) && x-vector_RR[l]<(-Criteria)){
              final_flags[l+1] <- ("1")
              vector_flags[l+1]<-("0")
            }
          }else{final_flags[l+1]<- "Can not evaluate"}
          
        }else{
          #Short beat
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
  

#Generar especificidad y sensibilidad
  
  
  
  #Tabla que contiene los resultados
  
  name = paste("Tabla_filtrados_",nombre_file,".txt",sep="")
  tabla_datos = data.frame(vector_RR,vector_flags,anotaciones_solo_modificados,anotaciones_modificados_adapted)
  
  setwd("C:/Users/RAQUEL/Desktop/RHRV/Modificados_Outputs")
  #Directorio donde deseo guardar la tabla de resultados 
  
  write.table(tabla_datos, file= name)
  setwd("C:/Users/RAQUEL/Desktop/RHRV/Modificados")
  
  
 
  #vector anotaciones A
  estimate_filtrados <- vector_flags[1:length(anotaciones)]
  df = data.frame('reference' = factor(anotaciones, levels=c('0','1')), 'predictions' = factor(estimate_filtrados,levels=c('0','1')))
  
  matriz_confusion_tabla = conf_mat(df, truth = reference, estimate = predictions)

  matriz_confusion=as.matrix(matriz_confusion_tabla$table)
  
  #vector anotaciones B
  
  df = data.frame('reference' = factor(anotaciones_solo_modificados, levels=c('0','1')), 'predictions' = factor(estimate_filtrados,levels=c('0','1')))
  
  matriz_confusion_tabla_2 = conf_mat(df, truth = reference, estimate = predictions)
  
  matriz_confusion_2=as.matrix(matriz_confusion_tabla_2$table)
  
  matriz_suma=matriz_suma+matriz_confusion_2
  
  
  #Vector anotaciones C
  df = data.frame('reference' = factor(anotaciones_modificados_adapted, levels=c('0','1')), 'predictions' = factor(estimate_filtrados,levels=c('0','1')))
  
  matriz_confusion_tabla_3 = conf_mat(df, truth = reference, estimate = predictions)
  
  matriz_confusion_3=as.matrix(matriz_confusion_tabla_3$table)
  
  
  matriz_suma2=matriz_suma2+matriz_confusion_3
  
  
  #vector A

  sensibilidad=(matriz_confusion[2,2]/(matriz_confusion[2,2]+matriz_confusion[1,2]))*100            
  especificidad=(matriz_confusion[1,1]/(matriz_confusion[1,1]+matriz_confusion[2,1]))*100
  
  #vector B
  
  sensibilidad_2=(matriz_confusion_2[2,2]/(matriz_confusion_2[2,2]+matriz_confusion_2[1,2]))*100            
  especificidad_2=(matriz_confusion_2[1,1]/(matriz_confusion_2[1,1]+matriz_confusion_2[2,1]))*100
  
  #vector C
  
  sensibilidad_3=(matriz_confusion_3[2,2]/(matriz_confusion_3[2,2]+matriz_confusion_3[1,2]))*100            
  especificidad_3=(matriz_confusion_3[1,1]/(matriz_confusion_3[1,1]+matriz_confusion_3[2,1]))*100
  
  
  setwd("C:/Users/RAQUEL/Desktop/RHRV/Modificados_Outputs")
  #Directorio donde deseo guardar la tabla de resultados 

  #Genero un archivo para cada archivo de la base de datos
  Resultados=  paste("Resultado_",nombre_file,sep="")
 
  cat(name, file=Resultados, append=TRUE, sep = "\n")
  cat("especificidad_todo", file=Resultados, append=TRUE, sep = "\n")
  cat(especificidad, file=Resultados, append=TRUE, sep = "\n")
  cat("sensibilidad_todo", file=Resultados, append=TRUE, sep = "\n")
  cat(sensibilidad, file=Resultados, append=TRUE, sep = "\n")
  cat(matriz_confusion, file=Resultados, append=TRUE)
  cat(" ", file=Resultados, append=TRUE, sep = "\n")
  cat(" ", file=Resultados, append=TRUE, sep = "\n")
  cat("especificidad_modificados", file=Resultados, append=TRUE, sep = "\n")
  cat(especificidad_2, file=Resultados, append=TRUE, sep = "\n")
  cat("sensibilidad_modificados", file=Resultados, append=TRUE, sep = "\n")
  cat(sensibilidad_2, file=Resultados, append=TRUE, sep = "\n")
  cat(matriz_confusion_2, file=Resultados, append=TRUE)
  cat(" ", file=Resultados, append=TRUE, sep = "\n")
  cat(" ", file=Resultados, append=TRUE, sep = "\n")
  cat("especificidad_modificados_adapted", file=Resultados, append=TRUE, sep = "\n")
  cat(especificidad_3, file=Resultados, append=TRUE, sep = "\n")
  cat("sensibilidad_modificados_adapted", file=Resultados, append=TRUE, sep = "\n")
  cat(sensibilidad_3, file=Resultados, append=TRUE, sep = "\n")
  cat(matriz_confusion_3, file=Resultados, append=TRUE)
  cat(" ", file=Resultados, append=TRUE, sep = "\n")
  cat(" ", file=Resultados, append=TRUE, sep = "\n")
  
  
  
}
setwd("C:/Users/RAQUEL/Desktop/RHRV/Modificados_Outputs")
#Directorio donde deseo guardar la tabla de resultados 
#Genero un archivo que permita una vista generica del comportamiento del algoritmo 


Resultados= "ResultadoTotal.txt"

#Vector B
sensibilidad_final=(matriz_suma[2,2]/(matriz_suma[2,2]+matriz_suma[1,2]))*100            
especificidad_final=(matriz_suma[1,1]/(matriz_suma[1,1]+matriz_suma[2,1]))*100

cat(name, file=Resultados, append=TRUE, sep = "\n")
cat("Especificidad", file=Resultados, append=TRUE, sep = "\n")
cat(especificidad_final, file=Resultados, append=TRUE, sep = "\n")
cat("Sensibilidad", file=Resultados, append=TRUE, sep = "\n")
cat(sensibilidad_final, file=Resultados, append=TRUE, sep = "\n")
cat("Matriz_suma_total", file=Resultados, append=TRUE, sep = "\n")
cat(matriz_suma, file=Resultados, append=TRUE, sep = "\n")
cat(" ", file=Resultados, append=TRUE, sep = "\n")
cat(" ", file=Resultados, append=TRUE, sep = "\n")


#vector C

sensibilidad_final2=(matriz_suma2[2,2]/(matriz_suma2[2,2]+matriz_suma2[1,2]))*100            
especificidad_final2=(matriz_suma2[1,1]/(matriz_suma2[1,1]+matriz_suma2[2,1]))*100

cat(name, file=Resultados, append=TRUE, sep = "\n")
cat("Especificidad_adapted", file=Resultados, append=TRUE, sep = "\n")
cat(especificidad_final2, file=Resultados, append=TRUE, sep = "\n")
cat("Sensibilidad_adapted", file=Resultados, append=TRUE, sep = "\n")
cat(sensibilidad_final2, file=Resultados, append=TRUE, sep = "\n")
cat("Matriz_suma_total_adapted", file=Resultados, append=TRUE, sep = "\n")
cat(matriz_suma2, file=Resultados, append=TRUE, sep = "\n")
  
  
  
  
  
