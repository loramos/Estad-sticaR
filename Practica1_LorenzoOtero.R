                                   #PRÁCTICA 1 LORENZO OTERO RAMOS


rm(list=ls()) #Borrado de variables del espacio de trabajo
#dev.off() Apaga el dispositivo gráfico de prácticas anteriores
options(scipen = 999) #Quitar notación científica

# Librerias necesarias

function_install_packages <- function(x)
{
  for(i in x)
  {
    if(!require(i, character.only = TRUE))
    {
      install.packages(i, dependencies = TRUE)
      require(i, character.only = TRUE)
    }
  }
}

function_install_packages(c("ggplot2", "readr", "gridExtra", "grid", "dplyr", "e1071", "readxl", 
                            "ggcorrplot", "vcd", "hier.part"))

library(ggplot2)
library(readr)
library(gridExtra)
library(grid)
library(dplyr)
library(e1071)
library(readxl)
library(ggcorrplot)
library(vcd)
library(hier.part)

# Lectura de datos. Cargamos el DataFrame con el que vamos a trabajar

DATA_P1 <- read_excel("C:/Users/Lorenzo/Desktop/Máster Datahack Analytics/2.Estadística/Tablas/01.- Práctica Análisis Estadístico de Datos (Tabla).xlsx")
class(DATA_P1)
DATA_P1 <- as.data.frame(DATA_P1)

#Creo un nuevo dataFrame a partir del original, modificando el nombre de algunas variables para que me resulte más cómodo trabajar con ellas.
#Para ello utilizo el comando rename de mutate, a partir de la librería dplyr
DATA_P1_RENAMED <- DATA_P1 %>%
  rename(registro = REGISTRO,
         provincia = PROVINCIA,
         productividad = PRODUCTIVIDAD,
         ventas = VENTAS,
         num_empleados = `NÚMERO DE EMPLEADOS`)

#Primera aproximación a los datos, donde podemos ver un resumen del DataFrame

str(DATA_P1_RENAMED)
names(DATA_P1_RENAMED)
summary(DATA_P1_RENAMED)
head(DATA_P1_RENAMED)

nrow(DATA_P1_RENAMED)  #Numero de filas del DataFrame
table(DATA_P1_RENAMED$provincia, useNA = "always") #Distribución de las frecuencias por Provincia. Número de tiendas por provincia
round(100 * table(DATA_P1_RENAMED$provincia, useNA = "always") / sum(table(DATA_P1_RENAMED$provincia, useNA = "always")), digits = 2) # Porcentaje de tiendas por provincia

# Mínimo, primer cuartil, mediana, tercer cuartil y máximo de Variable VENTAS. ESTADÍSTICOS DE POSICION DE VARIABLE VENTAS

fivenum(DATA_P1_RENAMED$ventas)
round(fivenum(DATA_P1_RENAMED$ventas), digits = 1)

# Mínimo, primer cuartil, mediana, tercer cuartil y máximo de Variable PRODUCTIVIDAD. ESTADÍSTICOS DE POSICION VARIABLE PRODUCTIVIDAD

fivenum(DATA_P1_RENAMED$productividad)
round(fivenum(DATA_P1_RENAMED$productividad), digits = 1)

# Mínimo, primer cuartil, mediana, tercer cuartil y máximo de Variable EMPLEADOS. ESTADÍSTICOS DE POSICION VARIABLE EMPLEADOS

fivenum(DATA_P1_RENAMED$num_empleados)
round(fivenum(DATA_P1_RENAMED$num_empleados), digits = 1)


# Estadísticas de las variables
# ESTADISTICAS DE DISPERSIÓN VARIABLE VENTAS. Media, Mediana, Moda, Varianza y Desviación Típica

mean(DATA_P1_RENAMED$ventas)
median(DATA_P1_RENAMED$ventas)
mode(DATA_P1_RENAMED$ventas)
var(DATA_P1_RENAMED$ventas)
sd(DATA_P1_RENAMED$ventas)

# ESTADÍSTICAS DE DISPERSIÓN VARIABLE PRODUCTIVIDAD.  Media, Mediana, Moda, Varianza y Desviación Típica

mean(DATA_P1_RENAMED$productividad)
median(DATA_P1_RENAMED$productividad)
mode(DATA_P1_RENAMED$productividad)
var(DATA_P1_RENAMED$productividad)
sd(DATA_P1_RENAMED$productividad)

# ESTADÍSTICAS DE DISPERSIÓN VARIABLE EMPLEADOS  Media, Mediana, Moda, Varianza y Desviación Típica

mean(DATA_P1_RENAMED$num_empleados)
median(DATA_P1_RENAMED$num_empleados)
mode(DATA_P1_RENAMED$num_empleados)
var(DATA_P1_RENAMED$num_empleados)
sd(DATA_P1_RENAMED$num_empleados)

#ESTADISTICAS DE FORMA

# Coeficiente de Variación de Pearson(Variable Ventas) Media pobremente representativa. Mucha variabilidad en los datos

(COEF_VARIACION_PEARSON <- round(sd(DATA_P1_RENAMED$ventas) / mean(DATA_P1_RENAMED$ventas), digits = 2))

# Coeficiente de Variación de Pearson(Variable Productividad) Media pobremente representativa. Mucha variabilidad en los datos

(COEF_VARIACION_PEARSON <- round(sd(DATA_P1_RENAMED$productividad) / mean(DATA_P1_RENAMED$productividad), digits = 2))

# Coeficiente de Variación de Pearson(Variable EMPLEADOS) Media pobremente representativa. Mucha variabilidad en los datos 

(COEF_VARIACION_PEARSON <- round(sd(DATA_P1_RENAMED$num_empleados) / mean(DATA_P1_RENAMED$num_empleados), digits = 2))


skewness(DATA_P1_RENAMED$ventas)                  #Distribución asimétrica hacia la derecha. Media>Mediana
skewness(DATA_P1_RENAMED$productividad)           #Distribución asimétrica hacia la derecha. Media>Mediana
skewness(DATA_P1_RENAMED$num_empleados)           #Distribución asimétrica hacia la derecha. Media>Mediana

kurtosis(DATA_P1_RENAMED$ventas)                #La distribución tiene una forma más puntiaguda o concentrada que una distribución normal(distribución leptocúrtica) 
kurtosis(DATA_P1_RENAMED$productividad)         #La distribución tiene una forma más puntiaguda o concentrada que una distribución normal(distribución leptocúrtica) 
kurtosis(DATA_P1_RENAMED$num_empleados)         #La distribución tiene una forma más puntiaguda o concentrada que una distribución normal(distribución leptocúrtica) 


# Gráfico de dispersión.

  plot(x = DATA_P1_RENAMED$productividad, y = DATA_P1_RENAMED$ventas,  
     xlab = "PRODUCTIVIDAD", ylab = "VENTAS", main = "PROD/VENTAS")
  #Aumento de productividad no implica aumento de ventas

  plot(x = DATA_P1_RENAMED$num_empleados, y = DATA_P1_RENAMED$productividad,  
     xlab = "EMPLEADOS", ylab = "PRODUCTIVIDAD", main = "EMPL/PROD")
  #Como podemos observar, la variable Empleados no está relacionada con productividad. Más empleados no indica más productividad.
  #Más adelante veremos en la matriz de correlaciones NUMERO DE EMPLEADOS/ PRODUCTIVIDAD= -0.0829, por lo que cuando una variable aumenta, la otra disminuye levemente(Próxima a 0)
  
  plot(x = DATA_P1_RENAMED$num_empleados, y = DATA_P1_RENAMED$ventas,  
       xlab = "EMPLEADOS", ylab = "VENTAS", main = "EMPL/VENTAS")
  #En este caso sí que podemos observar una relación directa entre las dos variables, cuando el número de empleados de una tienda aumenta, las ventas también
  
  ggplot(DATA_P1_RENAMED,aes(x=provincia,y=ventas)) +
    geom_col(fill="#ff6fd3") +
    labs(title="Ventas",
         subtitle="por provincia",
         y="VENTAS",x="PROVINCIAS",
         caption="Fuente: Lorenzo Otero") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #Gráfico donde observamos las provincias con mayor número de Ventas(Madrid, Barcelona, Valencia y Albacete)
  
  ggplot(DATA_P1_RENAMED,aes(x=provincia,y=productividad)) +
    geom_col(fill="#c6ef36") +
    labs(title="Productividad",
         subtitle="por provincia",
         y="PRODUCTIVIDAD",x="PROVINCIAS",
         caption="Fuente: Lorenzo Otero") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #Madrid es la provincia con mayor Productividad
  
  ggplot(DATA_P1_RENAMED,aes(x=provincia,y=num_empleados)) +
    geom_col(fill="#fffb78") +
    labs(title="Empleados",
         subtitle="por provincia",
         y="`NÚMERO DE EMPLEADOS`",x="PROVINCIAS",
         caption="Fuente: Lorenzo Otero") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #Barcelona es la provincia con mayor número de empleados
  
plot(DATA_P1_RENAMED$ventas,DATA_P1_RENAMED$productividad,
     pch=20,
     col="#1da3bf",
     xlab="Productividad",
     ylab="Ventas",
     main="Productividad y ventas") 

  #Aumento de la productividad no implica aumento de ventas

# Histograma. Como podemos ver, la variable Empleados tiene más registros en rangos pequeños
Histograma <- ggplot(data = DATA_P1_RENAMED, aes(x = num_empleados)) + 
  geom_histogram(binwidth = 20, color = "black") +  
  xlab("Rango de la variable Empleados") +  
  ylab("Cantidad de registros") + 
  theme(legend.position = "none")+
  ggtitle("Histograma de la variable Empleados") 

Histograma

Histograma_2 <- ggplot(data = DATA_P1_RENAMED, aes(x = edad)) + 
  geom_histogram(binwidth = 8, color = "#5d5a8a") +  
  xlab("Rango de la variable edad") +  
  ylab("Cantidad de registros") + 
  theme(legend.position = "none")+
  ggtitle("Histograma tiendas por edad")

Histograma_2

boxplot2 <- function(x,y)
{
  stats = boxplot.stats(x)$stats
  f = fivenum(x)
  stats2 <- c(f[1], stats, f[5])
  stats3 <- c(f[1], f[5])
  
  boxplot(x, main = y, col = "#ab4881")
  abline(h = stats[1], lty = 2, col = "red")
  abline(h = stats[5], lty = 2, col = "red")
  text(rep(1.35, 5), stats, labels = c('BIGOTE INFERIOR', 'PRIMER CUARTIL', 'MEDIANA', 'TERCER CUARTIL', 'BIGOTE SUPERIOR'))
  text(rep(.5, 7), stats2, labels = round(stats2, digits = 4), cex = 0.6)
  text(rep(0.75,2), stats3, labels = c('MINIMO','MAXIMO'))
}

boxplot2(DATA_P1_RENAMED$ventas, 'Diagrama de cajas para la variable VENTAS') #Distribución sesgada. Mediana próxima la limite inferior de la caja. Valores atípicos por encima del bigote superior
boxplot2(DATA_P1_RENAMED$num_empleados, 'Diagrama de cajas para la variable Empleados') #Distribución sesgada. Mediana próxima al limite inferior de la caja. Valores atípicos por encima del bigote superior
boxplot2(DATA_P1_RENAMED$productividad, 'Diagrama de cajas para la variable PRODUCTIVIDAD') 
boxplot2(DATA_P1_RENAMED$edad, 'Diagrama de cajas para la variable edad')


DATOS_1 <- select(DATA_P1_RENAMED, -registro, -provincia) #Reagrupo las variables que necesito para Matriz de Correlaciones(Provincia es una variable cualitativa)
                                                                              
# Correlaciones entre parejas de variables.
cor(DATOS_1) #Matriz de correlación entre todos los pares de variables
ggcorrplot(cor(DATOS_1, use = "complete.obs"), hc.order = TRUE, type = "lower", lab = TRUE) #Gráfico de correlación
p.mat <- cor_pmat(DATOS_1)
ggcorrplot(cor(DATOS_1, use = "complete.obs"), hc.order = TRUE,  #Gráfico de correlación tachando valores insignificantes
           type = "lower", p.mat = p.mat)

#De esta matriz podemos extraer que existe una fuerte relación positiva entre Ventas/Empleados(0.89). Cuando el número de empleados aumenta, las ventas también
#La relación entre Productividad/Empleados es ligeramente negativa(-0.08). Cuando una aumenta la otra disminuye ligeramente. 
#La relación entre Productividad/Ventas es ligeramente positiva(0.08). Cuando una aumenta la otra aumenta muy ligeramente. Podríamos decir que no hay relación lineal entre estas dos variables
#Existe una significante correlación negativa entre "endp" y "rentabieco", por lo que a medida que una crece, la otra decrece


#Creamos intervalos para "Ventas" con rangos homogéneos según la fórmula del enunciado. 

#Definimos el tamaño de cada intervalo según la fórmula descrita en el enunciado

tamaño_intervalo = (max(DATA_P1_RENAMED$ventas)- min(DATA_P1_RENAMED$ventas))/sqrt(4403) 
tamaño_intervalo


#Definimos los límites de cada intervalo

limites_intervalo= seq(min(DATA_P1_RENAMED$ventas),max(DATA_P1_RENAMED$ventas),by=tamaño_intervalo)
limites_intervalo #El primer número es el mínimo valor de ventas, por lo que a partir de ahí empezará el intervalo 1

#Definimos etiquetas para cada Intervalo

intervalo_etiquetas <- paste0("Intervalo ", 1:(length(limites_intervalo)-1 ))
intervalo_etiquetas

#Creamos la variable Rango_Ventas mediante la función Cut(), que  nos asociará un intervalo a cada Venta en la nueva variable

DATA_P1_RENAMED$rango_ventas = cut(DATA_P1_RENAMED$ventas, breaks = limites_intervalo,labels = intervalo_etiquetas, include.lowest = FALSE)



#Creación de la variable Empresa, con la que a partir del número de empleados, las dividimos en varios tipos.

DATA_P1_RENAMED$tipo_empresa <- ifelse(DATA_P1_RENAMED$num_empleados %in% 1:9,                                  "MICROEMPRESA", 
                         
                         ifelse(DATA_P1_RENAMED$num_empleados %in% 10:49,                                       "PEQUEÑA EMPRESA", 
                                
                                ifelse(DATA_P1_RENAMED$num_empleados %in% 50:249,                               "MEDIANA EMPRESA", 
                                       
                                       ifelse(DATA_P1_RENAMED$num_empleados %in% 250:2000,                      "GRAN EMPRESA", 
                                              
                                              "NULL"))))

table(DATA_P1_RENAMED$tipo_empresa, useNA = "always")       #La nueva variable nos indica que cada registro tiene un nuevo tipo de empresa, sin valores nulos
 #Podemos ver el resumen según el tipo de empresa. No hay Grandes Empresas porque el valor maximo de num_empleados es 169


#Tabla de contingencia entre las dos variables
#A simple vista, podemos observar que en pequeños intervalos(menos ventas), más microempresas. Cuando los intervalos aumentan(más ventas), el número de empleados es mayor. Como hemos visto antes, su correlación es cercana a 1.

tabla_contingencia= table(DATA_P1_RENAMED$rango_ventas,DATA_P1_RENAMED$tipo_empresa)
tabla_contingencia
#A continuación, aplicaré el Chisq-t para probar la dependencia entre las dos variables categóricas. 
#Para eso, eliminaré las filas con 0 para no perjudicar el análisis.
tabla_cont_sin_ceros <- table(which(!is.na(tabla_contingencia) & tabla_contingencia != 0, arr.ind = TRUE))
#Crea una nueva tabla de contigncia sin las celdas con 0s, y sin celdas que tengan valores vacíos(no hay)
tabla_cont_sin_ceros

chisq.test(tabla_cont_sin_ceros)
#p-valor<0.05. Rechazo la hipótesis nula de que no hay asociación  entre las dos variables categóricas. Las variables están relacionadas.
#Las dos variables categóricas están muy probablemente relacionadas y la probabilidad de que la asociación observada sea el resultado del azar es extremadamente baja.


#Regresion Lineal SIMPLE entre Ventas y Número de Empleados, que son las que tienen una correlación más alta.

regresion <- lm( ventas ~ num_empleados, data = DATA_P1_RENAMED)
summary(regresion)
 #La recta de regresión es: Ventas = 441.081*empleados + (-557.297)
 #Por cada incremento unitario en la variable independiente (número de empleados), se espera un incremento de 441.081 en la variable dependiente (ventas). 
 #pvalue<0.05. Podemos rechazar la hipótesis nula

 #La pendiente es 441.081: Estimate-Empleados
pendiente<-cov(DATA_P1_RENAMED$ventas,DATA_P1_RENAMED$num_empleados)/var(DATA_P1_RENAMED$num_empleados)
pendiente

# El termino independiente es -557.297: Estimate-(Intercept)
independiente<-mean(DATA_P1_RENAMED$ventas)-pendiente*mean(DATA_P1_RENAMED$num_empleados)
independiente
# El coeficiente de determinacion (que mide la bondad del ajuste de la recta a los datos) 
# es Multiple R-squared: 0,7908. El modelo tiene una buena capacidad para explicar las variaciones en la variable dependiente.
#el 79% de la variabilidad observada en los datos puede ser explicada por el modelo.
coeficiente_R2<-(cov(DATA_P1_RENAMED$num_empleados,DATA_P1_RENAMED$ventas)/(sd(DATA_P1_RENAMED$num_empleados)*sd(DATA_P1_RENAMED$ventas)))^2
coeficiente_R2
# Nube de puntos y recta de minimos cuadrados.
plot(DATA_P1_RENAMED$num_empleados, DATA_P1_RENAMED$ventas, xlab="EMPLEADOS", ylab="VENTAS")
abline(regresion)


#Regresion Lineal Multiple que relaciona Ventas con Empleados y Productividad

#Pvalue<0.05. Rechazamos la Hipótesis Nula de que el modelo no es válido
#r2-ajustado= 0.8143. El 81.43% de la variabilidad de ventas es predicha por nuestro modelo teniendo en cuenta nuestras dos variables predictoras
# La recta de regresión es: Ventas = 447.418*empleados + 85.241*productividad- 2043.824
REGRESION_MULTIPLE_1 <- lm(ventas ~ num_empleados + productividad, data = DATA_P1_RENAMED)
summary(REGRESION_MULTIPLE_1)



#Regresión Multiple Clásica entre la variable predictora Ventas con el resto de variables cuya correlación es medianamente significativa

REGRESION_MULTIPLE_2 <- lm(  ventas ~  num_empleados + coe + numest + fju + edad + liq + productividad + rentabifin + numest , data = DATA_P1_RENAMED)
summary(REGRESION_MULTIPLE_2)
#Ajuste de bondad medianamente alto : 0.8228. p-valor<0.05. Procedemos a descartar la variable liq, cuyo p-valor es muy alto.

REGRESION_MULTIPLE_3<-update(REGRESION_MULTIPLE_2 , .~.-liq)
summary(REGRESION_MULTIPLE_3)
# Descartar la variable liq no representa consecuencias notorias
# el modelo sigue siendo significativo y la bondad del ajuste queda practicamente igual 
# (sube ligeramente, hasta 0,8229).
#Procedemos a descartar la variable rentabifin, cuyo p-valor es alto.

REGRESION_MULTIPLE_4<-update(REGRESION_MULTIPLE_3 , .~.-rentabifin)
summary(REGRESION_MULTIPLE_4)
#El r2 ajustado no se ve afectado. Ahora todas nuestras variables resultan significativas en el modelo.
#Todas las variables tienen un p-valor<0.05.
#Recta de la regresión: Ventas = num_empleados*410.57 + coe*43036 + numest*328.7 + fju*664.19 + edad*(-25.3) + productividad*78.5 - 2277.5

anova(REGRESION_MULTIPLE_2, REGRESION_MULTIPLE_4) #Comparo el primer modelo con el ultimo
#p-valor= 0.81. Eliminar las dos variables no supone efectos relevantes en el modelo.

reg.lm0<-lm(ventas~1, DATA_P1_RENAMED)
slm.forward<-step(reg.lm0, scope=~num_empleados +coe+fju+edad+productividad+rentabifin+numest, direction="forward") #Proceso de Regresión Step hacia adelante nos saca el mismo modelo

# Medida de la bondad del ajuste con RMSPE (Root-Mean-Square Prediction Error).
(relat.imp.RMSPE = hier.part(DATA_P1_RENAMED$ventas, DATA_P1_RENAMED[ , c("num_empleados", "coe","numest","fju","edad","productividad")], family = "gaussian", gof = "RMSPE", barplot = TRUE))
#Gráfico en lo que se observa lo que contribuye cada variable al r2-ajustado de la Regresión.
#Como podemos ver, la mayor parte se la lleva num_empleados, que era con la que tenía mayor correlación al principio


#Para decidir si hay evidencias de que las medias de las dos ciudades se comportan de forma diferente.
#Creo dos dataframes con las ventas de cada ciudad con dplyr.

VENTAS_BARCELONA = DATA_P1_RENAMED %>% 
  filter(provincia=="Barcelona") %>% 
  select(provincia,ventas)
  
VENTAS_MADRID = DATA_P1_RENAMED %>% 
  filter(provincia=="Madrid") %>% 
  select(provincia,ventas)

#H0: Hipótesis nula. Las medias son iguales
t.test(VENTAS_BARCELONA$ventas,VENTAS_MADRID$ventas, var.equal = F) #p-valor= 0.56. No se puede concluír que haya una diferencia signficativa entre las medias de las dos ciudades.
var.test(VENTAS_BARCELONA$ventas,VENTAS_MADRID$ventas) #Las varianzas no son iguales. p-valor<0.05. Rechazamos la hipótesis nula de que las varianzas son iguales

shapiro.test(VENTAS_BARCELONA$ventas) #Los datos no siguen una distribucición normal. p-valor<0.05. Rechazamos H0 de que la muestra sigue una distribución normal.
shapiro.test(VENTAS_MADRID$ventas)    #Los datos no siguen una distribucición normal. p-valor<0.05. Rechazamos H0 de que la muestra sigue una distribución normal.

wilcox.test(VENTAS_BARCELONA$ventas,VENTAS_MADRID$ventas) #p-valor>0.05, No se puede concluír que haya una diferencia significativa entre las medias.


