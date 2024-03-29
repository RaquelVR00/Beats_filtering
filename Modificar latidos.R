#Voy al directorio que contiene los archivos de RRs
setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")

#Servira para ir llamando a cada archivo de RRs
numero_registro=0


#Cuento el numero de archivos que hay en la carpeta
b<-as.vector(list.files())
#Lo divido entre dos porque tambi�n estan contenidos los archivos de anotaciones
c=length(b)/2


#Bucle que recorrera todos los archivos de RRs
for(i in 1:c){
  setwd("C:/Users/RAQUEL/Desktop/RHRV/RRs")
  #Suma 1 para ir cambiando el nombre del archivo de RRs
  numero_registro=numero_registro+1
  
  #Generamos nuevas anotaciones
  #Matriz de confusi�n
  #install.packages('yardstick')
  #Genero nombre del archivo anotaciones
  library('yardstick')
  if(numero_registro<10){
    nombre_anotations=paste("I0",numero_registro,"_ann.txt",sep="")
  }else{
    nombre_anotations=paste("I",numero_registro,"_ann.txt",sep="")
  }
  file_name_anotaciones = nombre_anotations
  #Cojo las anotaciones 
  tabla_real <- read.table(file_name_anotaciones, skip=1, stringsAsFactors = FALSE, fill=TRUE, header=FALSE)
  
 
  #Genero los vectores A, B, C, D, V
  truth_filtrados_vector=truth_ventriculares=truth_ventriculares_final=truth_ventriculares_final2=truth_filtrados_vector_final2=truth_filtrados_vector_final=modificados_detectados=modificados_adapted_detected=rep('0',length(tabla_real$V3))
  
  #Genero A
  m<-NULL
  m<-1
  while(TRUE){
    if((tabla_real$V3[m]== "N")|(tabla_real$V3[m]=="F")|(tabla_real$V3[m]=="R")){
      if((tabla_real$V3[m+1]== "N")|(tabla_real$V3[m+1]=="F")|(tabla_real$V3[m+1]=="R")){
      truth_filtrados_vector[m]<- "0"
      }else{
        truth_filtrados_vector[m]<-"1"
      }
    }else{
      truth_filtrados_vector[m]<-"1"
    
    }
    m=m+1
    if(m>=length(tabla_real$V3)){
      break
    }
  }
  m<-NULL
  m<-1
  #Genero para los ventriculares
  while(TRUE){
    if((tabla_real$V3[m]!= "V")){
      if((tabla_real$V3[m+1]!= "V")){
      truth_ventriculares[m]<- "0"
      }else{
        truth_ventriculares[m]<-"1"
      }
    }else{
      truth_ventriculares[m]<-"1"
    }
    m=m+1
    if(m>=length(tabla_real$V3)){
      break
    }
  }
  
  #Construye el nombre del archivo de RRs
  if(numero_registro<10){
    nombre_file=paste("I0",numero_registro,"_rr.txt",sep="")  
    }else{
    nombre_file=paste("I",numero_registro,"_rr.txt",sep="")
  }
  
  #Coge los datos del archivo
  tabla_RR <- read.table(nombre_file, header =FALSE)
  #Genera el vector de RRs del archivo
  vector_RR <- c(length(tabla_RR$v1))
  vector_RR <- (tabla_RR$V1)*1000
  vector_RR <- vector_RR[2:length(vector_RR)]
  
  #Vector para escoger las posiciones de los que seran modificados
  positions_choosen <- c()
  
  #Vector vacio para generar los nuevos RRs (anteriores + modificados)
  #Usado para a�adir los que se generan dividiendo
  vector_final=rep('0',(length(vector_RR)+15))
  #Usado para a�adir los que se generan sumando
  vector_final_2=rep('0',length(vector_RR))
  final=length(vector_RR)
    
  #Uso funcion sample para coger x posiciones
  #le resto 30 al total de donde escogera aleatoriamente
  #porque al a�adir ( dividiendo RR normal) o quitar (sumando RR normales)
  #varia la longitud del vector, asi evito que escoja una de las posiciones
  #finales del vector y que esta no exista
  total=(length(vector_RR)-30) 
  positions_divide <- sample(total, 15,replace = FALSE, prob=NULL)
  
  #Vector donde guardar� las posiciones finales de las flags introducidas
  #Vector_position1 tiene en cuenta que solo se marca el 2 RR generado
  #al dividir un RR normal
  vector_positions1=vector_positions=rep('0',45)
  
  #Para dividir RRs
  i=1
  j=1
  k=1
  o=1
  while(TRUE){
    #Si esta contenida la posicion dentro de los RRs que deben ser modificados
    #entro en el if
    if(is.element(i,positions_divide)){
      #Divido el RR
      new_RR<-floor((vector_RR[i]/2))
      #Lo introduzco en el vector RR
      vector_final[j]<-new_RR
      #Guardo la posicion del RR modificado a�adido
      vector_positions[k]<-j
      truth_filtrados_vector_final[j]<-"0"
      truth_ventriculares_final[j]<-"0"
      k=k+1
      j=j+1
      #Lo introduzco de nuevo en el vector RR ya que al dividir genero 2 RRs iguales
      vector_final[j]<-new_RR
      truth_filtrados_vector_final[j]<-"0"
      truth_ventriculares_final[j]<-"0"
      #Guardo la posicion del RR modificado a�adido
      vector_positions[k]<-j
      #Guardo la posicion del RR modificado a�adido
      vector_positions1[o]<-j
      k=k+1
      o=o+1
      
      
    }else{
      #Si la posicion no corresponde con un RR a modificar, meto el RR en el vector
      #sin modificarlo
      vector_final[j]<-vector_RR[i]
      truth_filtrados_vector_final[j]<-truth_filtrados_vector[i]
      truth_ventriculares_final[j]<-truth_ventriculares[i]
   
    }
    
    i=i+1
    j=j+1
    
    if(final<i){
      break
    }
    
  }
  
  #Vuelvo a coger 15 posiciones aleatorias
  total=(length(vector_RR)-30)
  positions_unify <- sample(total, 15,replace = FALSE, prob=NULL)
  
  #Para unir RRs
  i=1
  j=1
  while(TRUE){
    #SI la posicion esta contenida en RRs que deben ser  modificados entro en el if
    
    if(is.element(i, positions_unify)){
      #Sumo los dos RRs, l escogido y el siguiente
      new_RR<-(strtoi(vector_final[i])+strtoi(vector_final[i+1]))
      #Guardo el RR modificado en el vector RR
      vector_final_2[j]<-new_RR
      truth_filtrados_vector_final2[j]<-"0"
      truth_ventriculares_final2[j]<-"0"
      #GUardo la posicion del RR modificado
      vector_positions[k]<-j
      #GUardo la posicion del RR modifciado
      vector_positions1[o]<-j
      
      
      #Como cambian las posiciones de los RRs modificados
      #Si estos se encuentran despues de una union, le resto 1 posicion
      #a cada posicion de RR modificado que se encuentre despues de la posicion
      #del RR que se acaba de generar uniendo dos RRs normales
      l<-NULL
      for(l in 1:length(vector_positions)){
        if(strtoi(vector_positions[l])>j){
          vector_positions[l]<-(strtoi(vector_positions[l])-1)
          
        }
      }
      for(l in 1:length(vector_positions1)){
        if(strtoi(vector_positions1[l])>j){
          vector_positions1[l]<-(strtoi(vector_positions1[l])-1)
        }
      }
      
      k=k+1
      i=i+2
      o=o+1
    }else{
      vector_final_2[j]<-strtoi(vector_final[i])
      truth_filtrados_vector_final2[j]<-strtoi(truth_filtrados_vector_final[i])
      truth_ventriculares_final2[j]<-strtoi(truth_ventriculares_final[i])
      i=i+1
    }
    j=j+1
    if(final<j){
      break
    }
    
  }
  
  
  
  truth_filtrados_vector_final3=truth_filtrados_vector_final2
  
  
  #Incluyo en A las anotaciones de B y genero B
  z<-NULL
  for(z in 1:length(vector_positions)){
    position = strtoi(vector_positions[z])
    truth_filtrados_vector_final2[position]<-"1"
    modificados_detectados[position]<-"1"
    
  }
  #Genero D (anotaciones mas las anotaciones nuevas adaptadas)
  #Genero C
  z<-NULL
  for(z in 1:length(vector_positions1)){
    position = strtoi(vector_positions1[z])
    truth_filtrados_vector_final3[position]<-"1"
    modificados_adapted_detected[position]<-"1"
  }
  
 
  
  A <- truth_filtrados_vector_final2[1:length(vector_final_2)]
  B <- modificados_detectados[1:length(vector_final_2)]
  C <-modificados_adapted_detected[1:length(vector_final_2)]
  D <- truth_filtrados_vector_final3[1:length(vector_final_2)]
  V <- truth_ventriculares_final2[1:length(vector_final_2)]
  L <- truth_filtrados_vector[1:length(vector_RR)] #anotaciones originales de la base de datos

  #Generamos un file con los RRs modificados y las anotaciones nuevas
  name = paste("Modificados_",nombre_file,".txt",sep="")
  tabla_datos = data.frame(vector_final_2,A,B,C,D,V,vector_RR,L)
  
  setwd("C:/Users/RAQUEL/Desktop/RHRV/Modificados_ventricular/Modificados4")
  #Directorio donde deseo guardar la tabla de resultados 
  
  write.table(tabla_datos, file = name)
}