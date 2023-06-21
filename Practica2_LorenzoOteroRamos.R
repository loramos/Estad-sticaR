                                    #PRÁCTICA 2 LORENZO OTERO RAMOS

rm(list=ls()) #Borrado de variables del espacio de trabajo
#dev.off() Apaga el dispositivo gráfico de prácticas anteriores
options(scipen = 999) #Quitar notacion cientifica

#Librerías que pueden ser importantes para la realización de la práctica

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

function_install_packages(c("dplyr", "ggplot2", "corrplot", "ggcorrplot", "e1071", "tidyverse", "ggpubr", 
                            "base", "car", "MASS","leaps","hier.part","gvlma","lmtest","readxl"))

library(dplyr)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(e1071)
library(GGally)
library(tidyverse)
library(ggpubr)
library(base)
library(car)
library(MASS)
library(leaps)
library(hier.part)
library(gvlma)
library(MASS)
library(lmtest)
library(readxl)

#Cargo el Dataset, pasándolo a formato DataFrame para poder trabajar con él.
DATA_P2 <- as.data.frame(state.x77)

#Modifico el nombre de estas dos variables, para que sea más cómodo tratar con ellas
names(DATA_P2)[6] <- "HS_Grad"
names(DATA_P2)[4] <- "Life_Exp"


#Creo la variable Estado, que tendrá en cada fila el estado correspondiente

DATA_P2$Estado <- row.names(DATA_P2)

#Creo la variable División, donde en función del Estado, cada fila tendrá una División asignada
DATA_P2$Division <- ifelse(DATA_P2$Estado %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", 
                                                 "Vermont", "New Jersey", "New York", "Pennsylvania"),                                                 "NORTHEAST", 
                         
                         ifelse(DATA_P2$Estado %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", 
                                                    "Missouri", "Nebraska", "North Dakota", "South Dakota"),                                            "MIDWEST", 
                                
                                ifelse(DATA_P2$Estado %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", 
                                                           "Virginia", "District of Columbia", "West Virginia", "Alabama", "Kentucky", 
                                                           "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas"),                    "SOUTH", 
                                       
                                       ifelse(DATA_P2$Estado %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", 
                                                                  "Alaska", "California", "Hawaii", "Oregon", "Washington"),                              "WEST", 
                                              
                                              "NULL"))))

table(DATA_P2$Division, useNA = "always")       # Comprobamos que cada estado ha quedado asignado a una única división.
sum(table(DATA_P2$Division)) == nrow(DATA_P2)   #Suma de los estados que hay en cada División. No hay valores nulos.


#Grafico avanzado de estadística descriptiva y correlaciones entre variables
ggpairs(DATA_P2[ , (1:8)], aes(colour = DATA_P2$Division, alpha = 0.5), upper = list(continuous = wrap("cor", alignPercent = 0.5)))


#Crearemos un nuevo Dataframe llamado DATOS_2, con las columnas que nos interesan del DataFrame principal para hacer una matriz de correlaciones lineales.

DATOS_2 <- as.data.frame(DATA_P2[ , c("Murder", "Population", "Illiteracy", "Income", "Frost","Life_Exp","HS_Grad", "Area")])
cor(DATOS_2)  #Matriz de correlaciones lineales entre cada par de variables.

#Gráfico de correlaciones lineales.
ggcorrplot(cor(DATOS_2, use = "complete.obs"), hc.order = TRUE, type = "lower", lab = TRUE) #Gráfico de correlación
#Podemos observar una fuerte correlación positiva entre Murder y Illiteracy, Income/HS Grad y Lif Exp/HS Grad. Cuando una variable aumenta, la otra también
#Por otro lado, hay una fuerte correlación negativa entre Life Exp/ Murder, Frost/Illiteracy y HS Grad/ Illiteracy. Cuando una variable aumenta, la otra decrece
p.mat <- cor_pmat(DATOS_2)
ggcorrplot(cor(DATOS_2, use = "complete.obs"), hc.order = TRUE,  #Gráfico de correlación tachando valores insignificantes
           lab=T,type = "lower", p.mat = p.mat)

# Regresión lineal múltiple que relaciona la esperanza de vida con el resto de las variables
# H0: la variable no contribuye a la predicción en el modelo. En términos matemáticos, el coeficiente que lleva en el modelo es cero.
#Como podemos ver el el summary de la regresión, el p-valor rechaza la hipótesis nula en las variables Murder, Hs Grad y DivisionNorthEast,
#por lo que rechazamos H0 de que el modelo no es válido.
#r2-ajustado= 0.718. El 71% de la variabilidad de esperanza de vida es predicha por nuestro modelo teniendo en cuenta nuestras variables predictoras
REGRESION_MULTIPLE_1 <- lm(Life_Exp ~ Murder + Population + Illiteracy + Frost + Income + HS_Grad + Area + Division, data = DATA_P2)
summary(REGRESION_MULTIPLE_1)
#Hemos hecho una Regresión Lineal Multiple entre la variable dependiente Esperanza de vida con el resto de variables.
#Como podemos ver en el Summary, el r2-ajustado es medianamente elevado, aunque hay variables cuyo p-valor es mayor que 0.05,
#con lo que podríamos descartarlas, pero eso lo haremos en el siguiente paso mediante el proceso Step.


REGRESION_MULTIPLE_2 = (step(REGRESION_MULTIPLE_1, direction = "backward"))
summary(REGRESION_MULTIPLE_2)
#El proceso Step me elimina las variables Illiteracy,Income y Area, cuyo p-valor era bastante elevado
#Con ello, el p-valor de las otras mejora, y el r2 ajustado también mejora. Pasa de 0.718 de la regresión inicial con todas las variables a 0.7304 con el proceso Step


# Intervalos de confianza al 95% para los coeficientes de las variables predictoras mas importantes.
confint(lm(Life_Exp ~ Murder + Population  + Frost + HS_Grad + Division, data = DATA_P2))
#Intervalos pequeños. Buena precision en la estimación.


# Resumen de la evolución sufrida por el R-cuadrado ajustado.
REGRESION_SALTOS <-regsubsets(Life_Exp ~ Murder + Population + Frost + HS_Grad + Division , data = DATA_P2, nbest = 4)
plot(REGRESION_SALTOS, scale = "adjr2")
#El modelo con un r2-ajustado mas alto incluye todas las variables de nuestro modelo final

# Importancia relativa de cada variable (de entre las más representativas): aportación al R-cuadrado ajustado final.

(relat.imp.RMSPE = hier.part(DATA_P2$Life_Exp, DATA_P2[ , c("Murder","HS_Grad","Frost","Division","Population")], family = "gaussian", gof = "RMSPE", barplot = TRUE))


# Diagnóstico del modelo: homocedasticidad -vs- heterocedasticidad, normalidad de residuos y valores anómalos en la predicción.
par(mfrow = c(2,2))
plot(REGRESION_MULTIPLE_2)

# Test de Breusch-Pagan.
bptest(REGRESION_MULTIPLE_2)                    # p-valor > 0.05: no se rechaza la hipotesis nula de homocedasticidad. MODELO HOMOCEDASTICO.

# Test de Shapiro-Wilk.
shapiro.test(REGRESION_MULTIPLE_2$residuals)    # p-valor > 0.05: no se rechaza la hipotesis nula de normalidad. RESIDUOS CON DISTRIBUCI?N NORMAL.
qqPlot(REGRESION_MULTIPLE_2, labels = row.names(DATA_P2), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")

# Test de Durbin-Watson.
dwtest(REGRESION_MULTIPLE_2)                    # p-valor > 0.05: no se rechaza la hipotesis nula de no existencia de autocorrelacion: RESIDUOS INDEPENDIENTES.

gvlma(REGRESION_MULTIPLE_2)         # Cumplimiento de las condiciones del modelo.

outlierTest(REGRESION_MULTIPLE_2)   # Outliers.

vif(REGRESION_MULTIPLE_2)           # Valores por debajo de 5 e incluso cercanos a 1: AUSENCIA DE COLINEALIDAD.




