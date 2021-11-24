#Cambiar RR 
#Leo y genero vector
setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")

numero_registro=0

#Leo y genero vector

b<-as.vector(list.files())
c=length(b)/2


for(i in 1:c){
  setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")
  numero_registro=numero_registro+1
  
  if(numero_registro<10){
    nombre_file=paste("I0",numero_registro,"_rr.txt",sep="")  
    }else{
    nombre_file=paste("I",numero_registro,"_rr.txt",sep="")
  }
 
  tabla_RR <- read.table(nombre_file, header =FALSE)
  vector_RR <- c(length(tabla_RR$v1))
  vector_RR <- (tabla_RR$V1)*1000
  positions_choosen <- c()
  
  #Vector vacio para generar nuevos
  vector_final=rep('0',(length(vector_RR)+15))
  vector_final_2=rep('0',length(vector_RR))
  final=length(vector_RR)
    
  #Uso funcion sample para coger x posiciones
  total=(length(vector_RR)-30)
  positions_divide <- sample(total, 15,replace = FALSE, prob=NULL)
  
  #Vector donde guardaré las posiciones finales de las flags introducidas
  vector_positions=rep('0',30)
  
  #Para dividir 
  i=1
  j=1
  k=1
  while(TRUE){
    
    if(is.element(i,positions_divide)){
      new_RR<-floor((vector_RR[i]/2))
      vector_final[j]<-new_RR
      vector_positions[k]<-j
      k=k+1
      j=j+1
      vector_final[j]<-new_RR
      vector_positions[k]<-j
      k=k+1
      
      
    }else{
      vector_final[j]<-vector_RR[i]
      
    }
    
    i=i+1
    j=j+1
    
    if(final<i){
      break
    }
    
  }
  total=(length(vector_RR)-30)
  positions_unify <- sample(total, 15,replace = FALSE, prob=NULL)
  
  #Para unir
  i=1
  j=1
  
  while(TRUE){
    
    if(is.element(i, positions_unify)){
      new_RR<-(strtoi(vector_final[i])+strtoi(vector_final[i+1]))
      vector_final_2[j]<-new_RR
      vector_positions[k]<-j
      k=k+1
      i=i+2
    }else{
      vector_final_2[j]<-strtoi(vector_final[i])
      i=i+1
    }
    j=j+1
    if(final<j){
      break
    }
    
  }
  
  #Generamos nuevas anotaciones
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

  truth_filtrados_vector=modificados_detectados=rep('0',length(tabla_real$V3))
  m<-NULL
  for(m in 1:length(tabla_real$V3)){
    if((tabla_real$V3[m]== "N")|(tabla_real$V3[m]=="F")|(tabla_real$V3[m]=="R")){
      truth_filtrados_vector[m]<- "0"
    }else{
      truth_filtrados_vector[m]<-"1"
    }
  }
  z<-NULL
  for(z in 1:length(vector_positions)){
    position = strtoi(vector_positions[z])
    truth_filtrados_vector[position]<-"1"
    modificados_detectados[position]<-"1"
  }
  
  truth_filtrados_guardar <- truth_filtrados_vector[1:length(vector_final_2)]
  modificados_guardar <- modificados_detectados[1:length(vector_final_2)]
  
  #Generamos un file de anotaciones nuevas
  name = paste("Modificados_",nombre_file,".txt",sep="")
  tabla_datos = data.frame(vector_final_2,truth_filtrados_guardar,modificados_guardar)
  
  setwd("C:/Users/RAQUEL/Desktop/RHRV/Modificados")
  #Directorio donde deseo guardar la tabla de resultados 
  
  write.table(tabla_datos, file = name)
}