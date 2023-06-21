                                        #PRÁCTICA 3 LORENZO OTERO RAMOS

rm(list=ls()) # Borrado de todas las variables del workspace 
#dev.off()     # Quitar los graficos pasados de memoria 
options(scipen = 999) #Quitar notacion cientifica
# Instalacion de paquetes (salvo en caso de no estar instalados previamente):

if (!require(WRS2)){ 
  install.packages("WRS2") 
}
if (!require(readxl)){ 
  install.packages("readxl") 
}
if (!require(stringi)){ 
  install.packages("stringi") 
} 
if (!require(digest)){ 
  install.packages("digest") 
}
if (!require(VIM)){ 
  install.packages("VIM") 
}
if (!require(ggcorrplot)){ 
  install.packages("ggcorrplot") 
} 
if (!require(dplyr)){ 
  install.packages("dplyr") 
}


# Cargo de librerías necesarias.
library(WRS2)
library(readxl)
library(stringi)
library(digest)
library(VIM)
library(ggcorrplot)
library(dplyr)
library(readr)

#Cargamos el DataFrame
DATA_P3 <- read_delim("C:/Users/Lorenzo/Desktop/Máster Datahack Analytics/Estadística-20230127/Tablas/winequality-white.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(DATA_P3) #Resumen de los datos

aggr(DATA_P3, numbers = TRUE) #No hay datos faltantes
summary(aggr(DATA_P3, numbers = TRUE)) #100% de valores cubiertos. Algunos de ellos pueden ser duplicados.

# Deteccion de columnas con al menos un registro con valor nulo:
sapply(DATA_P3, function(x) all(!is.na(x)))
#Ninguna columna tiene valores nulos
# Funcion para conocer si una variable concreta estÃ¡ totalmente documentada o si contiene nulos:

NA_COUNT<-function(x){
  for(i in 1:length(x)){
    if(is.na(x[i])) {
      print("Algun valor de la variable es nulo")
      break}
    else(print("La variable no tiene valores nulos"))
    break
  }}
#Esta funcion me permite recorrer cada variable para saber si tiene algun valor nulo. 

NA_COUNT(DATA_P3$density)
NA_COUNT(DATA_P3$`fixed acidity`) #LLamo a la función para comprobar que no hay valores nulos


# Detección de duplicados:

DATA_P3_SIN_DUP <- unique(DATA_P3)
#Funcion que me elimina las filas(registros) duplicadas del Dataframe (filas idénticas), dejando sólo las filas únicas.
#A partir de ahora trabajaremos con el dataframe DATA_P3_SIN_DUP

cor(DATA_P3_SIN_DUP)
ggcorrplot(cor(DATA_P3_SIN_DUP, use="complete.obs"), hc.order=TRUE, type="lower", lab=TRUE)
p.mat <- cor_pmat(DATA_P3_SIN_DUP)
ggcorrplot(cor(DATA_P3_SIN_DUP, use = "complete.obs"), hc.order = TRUE,  #Gráfico de correlación tachando valores insignificantes
           lab=T,type = "lower", p.mat = p.mat)

DATA_P3_SIN_DUP$quality <- as.factor(DATA_P3_SIN_DUP$quality)
class(DATA_P3_SIN_DUP$quality) #Modificando el atributo Quality a Factor.


DATA_P3_SIN_DUP$calidad <- ifelse(DATA_P3_SIN_DUP$quality=="3",                             "Baja Calidad", 
                          
                           ifelse(DATA_P3_SIN_DUP$quality=="4" ,                            "Baja Calidad", 
                                 
                           ifelse(DATA_P3_SIN_DUP$quality=="5" ,                            "Baja Calidad",
                          
                           ifelse(DATA_P3_SIN_DUP$quality=="6" ,                            "Baja Calidad",
                          
                           ifelse(DATA_P3_SIN_DUP$quality=="7" ,                            "Alta Calidad",
                          
                           ifelse(DATA_P3_SIN_DUP$quality=="8" ,                            "Alta Calidad",
                          
                           ifelse(DATA_P3_SIN_DUP$quality=="9" ,                            "Alta Calidad",
                                               
                                               "NULL")))))))
#Creo la nueva variables "calidad" a partir de la variable Quality(formato factor).
#El resultado es una nueva columna con dos niveles (Alta calidad/ Baja calidad) en función de Quality

table(DATA_P3_SIN_DUP$calidad, useNA = "always")
#Resumen de la columna Calidad, sin valores nulos. Todos los registros de la tabla tienen asignados una calidad.

DATA_P3_SIN_DUP$calidad = as.factor(DATA_P3_SIN_DUP$calidad)
#Convierto la nueva variable en factor.
class(DATA_P3_SIN_DUP$calidad) #Compruebo que se ha modificado el tipo de variable correctamente
levels(DATA_P3_SIN_DUP$calidad) #Compruebo los niveles de la nueva variable factor.

DATA_P3_SIN_DUP$calidad_binaria <- ifelse(DATA_P3_SIN_DUP$calidad == "Alta Calidad", 1, 0)
#Creo la variable calidad_Binaria a partir de la variable calidad. 1=Calidad Alta, 0= Calidad Baja

DATA_P3_SIN_DUP$calidad_binaria = as.factor(DATA_P3_SIN_DUP$calidad_binaria)
class(DATA_P3_SIN_DUP$calidad_binaria) #La paso a factor

proporcion_calidad <- prop.table(table(DATA_P3_SIN_DUP$calidad))
proporcion_calidad
porcentaje_alta_calidad <- proporcion_calidad[1] * 100
porcentaje_alta_calidad

porcentaje_baja_calidad <- proporcion_calidad[2] * 100
porcentaje_baja_calidad

cat(paste("Porcentaje de vinos de alta calidad:", round(porcentaje_alta_calidad, 2), "%\n"))
cat(paste("Porcentaje de vinos de baja calidad:", round(porcentaje_baja_calidad, 2), "%\n"))

set.seed(123) #Semilla para que la división de los datos sea la misma en diferentes ejecuciones de código. 

training <- DATA_P3_SIN_DUP %>% #Con la librería dplyr, cojo el 70 % de las muestras del DataFrame original sin duplicados para el modelo predictivo
  sample_frac(0.7)

test <- DATA_P3_SIN_DUP %>%     #Cojo el resto para las pruebas de eficiencia. Utilizo el antijoin para asegurarme
  anti_join(training)           #de que no cojo ningún registro de la muestra de entrenamiento y evitar valores duplicados entre ellos
                                #Anti_join = Registros de DATA_P3_SIN_DUP - training
#Ahora tengo 3 dataframes. El original sin duplicados, y los otros 2 que representan el 70 y 30% del anterior.
#Training = 2773 registros. 70 % de 3961
#Test = 1188 Registros. 30% de 3961


#Con el conjunto de training, desarrollaré una REGRESIÓN LOGÍSTICA MÚLTIPLE.

modelo<-glm(calidad_binaria ~ `fixed acidity`+`volatile acidity`+`citric acid`+`residual sugar` + chlorides +`free sulfur dioxide` +`total sulfur dioxide`+ density + pH + sulphates + alcohol, data=training, family=binomial)
summary(modelo)
#La salida del summary de la glm nos muestra que la mayoría de las variables tienen un p-valor<0,05, por lo
#que rechazamos la hipótesis nula y afirmamos que las variables son buenas predictoras de la variable dependiente calidad_binaria.

modelo_final <- step(modelo, direction = "backward")
summary(modelo_final)
#El modelo Step hacia atrás(backward) nos descarta la variable "critic acid", cuyo p-valor es 0.58. 
#Tenemos evidencias de que la variable no contribuye a la predicción y procedemos a descartarla.
#Tambíen descarta la variable total_sulfur_dioxide y alcohol
#Ahora todas las variables del modelo son significativas en términos de p-valor

(residuos_modelo_final=resid(modelo_final))
#Mayoria de residuos cercanos a 0, el modelo tiene una buena aproximación a los datos

predicciones <- predict(modelo_final, newdata = test, type = "response")
predicciones
#Probabilidades de que un registro sea de buena calidad


etiquetas <- ifelse(predicciones > 1/3, "buen vino", "mal vino")


test <- test %>%
  mutate(etiquetas = ifelse(predicciones > 1/3, "buen vino", "mal vino"))


tabla <- table(test$calidad_binaria, test$etiquetas)
tabla
#Predicciones acertadas y falladas segun nuestro modelo

precision <- (tabla[1,2] + tabla[2,1]) / sum(tabla)
precision
#Precision del 79% en el dataFrame test. Nuestro modelo tiene bastante buena fiabilidad

prop_buenos_vinos_detectados <- tabla[2,1] / sum(tabla[2,])
prop_buenos_vinos_detectados
#Precision del 61% de vinos buenos acertados

prop_malos_vinos_detectados <- tabla[1,2] / sum(tabla[1,])
prop_malos_vinos_detectados
#Precision del 84% de vinos malos acertados


