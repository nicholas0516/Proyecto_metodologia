library(readr)
library(dplyr)
library(TSstudio)
library(readxl)
library(tidyverse)

## Analisis, lmpieza de datos y seleccion de las mejores variables dentro del modelo 

Variaciones_Anuales_IPC <- read_excel("bases_datos/Variaciones_Anuales_IPC.xlsx", 
                                      sheet = "Comparativa con el IPM")
View(Variaciones_Anuales_IPC)


## Graficos

p1 <- ggplot(data = Variaciones_Anuales_IPC,
       aes(x = Año,
           y = ipm)) + 
      geom_area(fill = rgb(0, 0.5, 1, alpha = 0.5)) + 
      ggtitle("Indice de Pobreza Multidimensional Nacional 2010 - 2021") +
      geom_line()

p2 <- ggplot(data = Variaciones_Anuales_IPC,
       aes(x = Año,
           y = ipm_inasis_escolar)) + 
      geom_area(fill = rgb(0, 0.5, 1, alpha = 0.5)) + 
      ggtitle("Indice de Pobreza Multidimensional de la variable de inasistencia escolar 2010 - 2021") +
      geom_line()

p3 <- ggplot(data = Variaciones_Anuales_IPC,
       aes(x = Año,
           y = ipm_Desempleo)) + 
      geom_area(fill = rgb(0, 0.5, 1, alpha = 0.5)) + 
      ggtitle("Indice de Pobreza Multidimensional de la variable desempleo 2010 - 2021") +
      geom_line()

p4 <- ggplot(data = Variaciones_Anuales_IPC,
       aes(x = Año,
           y = ipm_Analfabetismo)) + 
  geom_area(fill = rgb(0, 0.5, 1, alpha = 0.5)) + 
  ggtitle("Indice de Pobreza Multidimensional de la variable Analfabetismo 2010 - 2021") +
  geom_line()

library(patchwork)
(p1 | p3) / p4


library(psych)
multi.hist(x = Variaciones_Anuales_IPC, dcol = c("blue", "red"), dlty = c("dotted", "solid"), density=TRUE, global = FALSE)


library(corrplot)
corrplot(cor(dplyr::select(Variaciones_Anuales_IPC,ipm_Analfabetismo,ipm_Trabajo_infan,ipm_rezago_escolar,ipm_inasis_escolar,
                           ipm_Desempleo, ipm_Analfabetismo, ipm , ipc_Alimentos, ipc_Salud 
                           , ipc_Educación , ipc_Diversión , ipc_Comunicaciones)),
         method = "number", tl.col = "black")

#______________________________________________________________________________#
# Plantear el modelo----
#______________________________________________________________________________#




## Plantear el modelo y evaluar el mejor modelo con los criterios AIC ----

modelo_inicial <- lm(ipm~ipc_Alimentos+
                       +ipc_Vivienda+ipc_Vestuario+ipc_Salud+ipc_Educación+ipc_Diversión+
                       ipc_Transporte+ipc_Comunicaciones
                     ,Variaciones_Anuales_IPC)
step(object = modelo_inicial, direction = "both", trace = 1)

modelo_inicia_1 <- lm(ipm_inasis_escolar~ipc_Alimentos+
                       +ipc_Vivienda+ipc_Vestuario+ipc_Salud+ipc_Educación+ipc_Diversión+
                       ipc_Transporte+ipc_Comunicaciones
                     ,Variaciones_Anuales_IPC)
step(object = modelo_inicia_1, direction = "both", trace = 1)

summary(modelo_inicia_1)

modelo_inicial_2 <- lm(ipm_Desempleo~ipc_Alimentos+
                       +ipc_Vivienda+ipc_Vestuario+ipc_Salud+ipc_Educación+ipc_Diversión+
                       ipc_Transporte+ipc_Comunicaciones
                     ,Variaciones_Anuales_IPC)
step(object = modelo_inicial_2, direction = "both", trace = 1)


## Dado los criterios AIC elegimos el sigueinte modelo final ----

library(coefplot)



modelo_ipm <- lm(formula = ipm ~ ipc_Alimentos + ipc_Vivienda + ipc_Vestuario + 
                   ipc_Educación + ipc_Transporte, data = Variaciones_Anuales_IPC)

summary(modelo_ipm)




modelo_ipm_inasistencia_escolar <-lm(formula = ipm_inasis_escolar ~  ipc_Vestuario + 
                                       ipc_Salud + ipc_Educación  + ipc_Comunicaciones  
                                       , data = Variaciones_Anuales_IPC)

summary(modelo_ipm_inasistencia_escolar)



modelo_ipm_desmpleo <- lm(formula = ipm_Desempleo ~ ipc_Alimentos + ipc_Vivienda + ipc_Educación + 
                            ipc_Transporte, data = Variaciones_Anuales_IPC)


summary(modelo_ipm_desmpleo)



##comparacion modelos ----

multiplot(modelo_ipm_inasistencia_escolar, modelo_ipm,modelo_ipm_desmpleo )


#______________________________________________________________________________#
# MODELO 1 IPM general----
#______________________________________________________________________________#




## Analisis estaditico ----

summary(Variaciones_Anuales_IPC) ## resumen de los datos

library(psych)
multi.hist(x = Variaciones_Anuales_IPC, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")



library(psych)
library(qwraps2)
options(qwraps2_markup = "latex")
Sector.Agro <- Variaciones_Anuales_IPC
summary_statistics <-
  list(
    "ipm" =
      list(
        "Media " = ~mean(Variaciones_Anuales_IPC$ipm),
        "Mediana " = ~median(Variaciones_Anuales_IPC$ipm),
        "Min" = ~min(Variaciones_Anuales_IPC$ipm),
        "Max" = ~max(Variaciones_Anuales_IPC$ipm),
        "Varianza" = ~var(Variaciones_Anuales_IPC$ipm),
        "Desviacion estandar" = ~sd(Variaciones_Anuales_IPC$ipm),
        "coef simetria" = ~skew(Variaciones_Anuales_IPC$ipm),
        "kurtosis" = ~kurtosi(Variaciones_Anuales_IPC$ipm)
      ),
    "ipc_Alimentos" =
      list(
        "Media " = ~mean(Variaciones_Anuales_IPC$ipc_Alimentos),
        "Mediana " = ~median(Variaciones_Anuales_IPC$ipc_Alimentos),
        "Min" = ~min(Variaciones_Anuales_IPC$ipc_Alimentos),
        "Max" = ~max(Variaciones_Anuales_IPC$ipc_Alimentos),
        "Varianza" = ~var(Variaciones_Anuales_IPC$ipc_Alimentos),
        "Desviacion estandar" = ~sd(Variaciones_Anuales_IPC$ipc_Alimentos),
        "coef simetria" = ~skew(Variaciones_Anuales_IPC$ipc_Alimentos),
        "kurtosis" = ~kurtosi(Variaciones_Anuales_IPC$ipc_Alimentos)
      ),
    "ipc_Vivienda" =
      list(
        "Media " = ~mean(Variaciones_Anuales_IPC$ipc_Vivienda),
        "Mediana " = ~median(Variaciones_Anuales_IPC$ipc_Vivienda),
        "Min" = ~min(Variaciones_Anuales_IPC$ipc_Vivienda),
        "Max" = ~max(Variaciones_Anuales_IPC$ipc_Vivienda),
        "Varianza" = ~var(Variaciones_Anuales_IPC$ipc_Vivienda),
        "Desviacion estandar" = ~sd(Variaciones_Anuales_IPC$ipc_Vivienda),
        "coef simetria" = ~skew(Variaciones_Anuales_IPC$ipc_Vivienda),
        "kurtosis" = ~kurtosi(Variaciones_Anuales_IPC$ipc_Vivienda)
      ),
    "ipc_Vestuario" =
      list(
        "Media " = ~mean(Variaciones_Anuales_IPC$ipc_Vestuario),
        "Mediana " = ~median(Variaciones_Anuales_IPC$ipc_Vestuario),
        "Min" = ~min(Variaciones_Anuales_IPC$ipc_Vestuario),
        "Max" = ~max(Variaciones_Anuales_IPC$ipc_Vestuario),
        "Varianza" = ~var(Variaciones_Anuales_IPC$ipc_Vestuario),
        "Desviacion estandar" = ~sd(Variaciones_Anuales_IPC$ipc_Vestuario),
        "coef simetria" = ~skew(Variaciones_Anuales_IPC$ipc_Vestuario),
        "kurtosis" = ~kurtosi(Variaciones_Anuales_IPC$ipc_Vestuario)
      ),
    "ipc_Educación" =
      list(
        "Media " = ~mean(Variaciones_Anuales_IPC$ipc_Educación),
        "Mediana " = ~median(Variaciones_Anuales_IPC$ipc_Educación),
        "Min" = ~min(Variaciones_Anuales_IPC$ipc_Educación),
        "Max" = ~max(Variaciones_Anuales_IPC$ipc_Educación),
        "Varianza" = ~var(Variaciones_Anuales_IPC$ipc_Educación),
        "Desviacion estandar" = ~sd(Variaciones_Anuales_IPC$ipc_Educación),
        "coef simetria" = ~skew(Variaciones_Anuales_IPC$ipc_Educación),
        "kurtosis" = ~kurtosi(Variaciones_Anuales_IPC$ipc_Educación)
      ),
    "ipc_Transporte" =
      list(
        "Media " = ~mean(Variaciones_Anuales_IPC$ipc_Transporte),
        "Mediana " = ~median(Variaciones_Anuales_IPC$ipc_Transporte),
        "Min" = ~min(Variaciones_Anuales_IPC$ipc_Transporte),
        "Max" = ~max(Variaciones_Anuales_IPC$ipc_Transporte),
        "Varianza" = ~var(Variaciones_Anuales_IPC$ipc_Transporte),
        "Desviacion estandar" = ~sd(Variaciones_Anuales_IPC$ipc_Transporte),
        "coef simetria" = ~skew(Variaciones_Anuales_IPC$ipc_Transporte),
        "kurtosis" = ~kurtosi(Variaciones_Anuales_IPC$ipc_Transporte)
      ))
summary_table(Sector.Agro, summary_statistics)



## Tabla resultados modelo para latex ----

library(stargazer)
stargazer(modelo_ipm, type="latex", title="Modelo", report=('vc*p'))



#______________________________________________________________________________#
#  Prueba de significancia individual ----
#______________________________________________________________________________#

summary(modelo_ipm)

#______________________________________________________________________________#
# intervalos de confianza ----
#______________________________________________________________________________#



confint(lm(ipm ~ ipc_Alimentos + ipc_Vivienda + ipc_Vestuario + 
             ipc_Educación + ipc_Transporte, data = Variaciones_Anuales_IPC))



## Observando el resumen del modelo observamos que todas la varibales son significativas
## sin embargo es de resaltar que las variables produccion y tierras_agricolas 
## bajo un alpha del 0.1 % son significativas
## Por otro lado la varible tierras dedicada a cultivo agricolas se considero pertienente
## analizar su significancia individual con un alpha de 0.15

#______________________________________________________________________________#
# Prueba de signficacia global ----
#______________________________________________________________________________#


car::linearHypothesis(modelo_ipm, c("ipc_Alimentos=0", "ipc_Vivienda=0", "ipc_Vestuario=0",
                                      "ipc_Educación=0", "ipc_Transporte=0" ))

#______________________________________________________________________________#
# Cambio estructural- Prueba de Chow ----
#______________________________________________________________________________#


library(strucchange)
sctest(modelo_ipm, type = "Chow")


## Dado que en todas las pruebas el p-valor es mayor  que 0.05 no rechazamos la hipotesis 
## nula y vemos que pasa la prueba de chow por tanto no hay cambio estructural


#___________________________
# Prueba RESET de Ramsey ----                                                  
#___________________________
# Descargamos el paquete para realizar la prueba RESET de Ramsey 


library(lmtest)

resettest(modelo_ipm)

# Ho: Y1=Y2=0 (correcta especificación)
# Ha: al menos un gamma es diferente de 0 
# En este caso 0.05 < p-value = 0.1219 por lo que se acepta Ho, y se concluye que 
# no hay error en la especificación del modelo. 

#______________________________________________________________________________#
# NO MULTICOLINEALIDAD   ----
#______________________________________________________________________________#

#Matriz de correlaciones 
#Multicolinealidad si algún coeficiente es mayor a 0,8.

round(cor(x = Variaciones_Anuales_IPC, method = "pearson"), 3) 


corrplot(cor(dplyr::select(Variaciones_Anuales_IPC, ipm , ipc_Alimentos ,ipc_Vivienda ,ipc_Vestuario,  
                          ipc_Educación , ipc_Transporte)),
         method = "number", tl.col = "black")

library(car)
vif(modelo_ipm)

# Ningun coeficiente es mayor a 0.8 por tanto no hay multicolinealidad, sin embargo 
# se puede considerar que existe cierta autocorrelacion de algunas variables y por
# ello sacamos vif el cual vemos que esta dentro de lo aceptable dado que en todas
# las variabels es menor a 10. 

#______________________________________________________________________________#
# HOMOCEDASTICIDAD   ----
#______________________________________________________________________________#

library(car)     
library(lmtest)


#Prueba Breush-Pagan(Ho:Homocedasticidad)

bptest(modelo_ipm)

## Dado el resultado se tiene y p-value > 0.05 y se concluye que el modelo no 
## tiene problemas de homocedasticidad.

#______________________________________________________________________________#
# NO AUTOCORRELACIÓN:   ----
#______________________________________________________________________________#

#Durbin Watson test (Ho:No autocorrelación de 1er orden)

dwtest(modelo_ipm)
durbinWatsonTest(modelo_ipm)

#Prueba Breush-Godfrey (Ho:No autocorrelación de orden p)

bgtest(modelo_ipm)

## Dado el resultado se tiene y p-value > 0.05 y se concluye que el modelo no 
## tiene problemas de autocorrelacion

#______________________________________________________________________________#
# Normalidad:   ----
#______________________________________________________________________________#


library(tseries)

jarque.bera.test(modelo_ipm$residuals)


## Dado el resultado se tiene y p-value > 0.05 y se concluye que el modelo presenta 
## normalidad en sus datos

## Comprobacion grafica

qqnorm(modelo_ipm$residuals)
qqline(modelo_ipm$residuals)
shapiro.test(modelo_ipm$residuals)




#______________________________________________________________________________#
# MODELO 2 Inasistencia escolar----
#______________________________________________________________________________#



## Tabla resultados modelo para latex ----

library(stargazer)
stargazer(modelo_ipm_inasistencia_escolar, type="latex", title="Modelo", report=('vc*p'))



#______________________________________________________________________________#
#  Prueba de significancia individual ----
#______________________________________________________________________________#

summary(modelo_ipm_inasistencia_escolar)

#______________________________________________________________________________#
# intervalos de confianza ----
#______________________________________________________________________________#



confint(lm(formula = ipm_inasis_escolar ~ ipc_Vestuario + 
             ipc_Salud + ipc_Educación +
             ipc_Comunicaciones, data = Variaciones_Anuales_IPC))


## Observando el resumen del modelo observamos que todas la varibales son significativas
## sin embargo es de resaltar que las variables produccion y tierras_agricolas 
## bajo un alpha del 0.1 % son significativas
## Por otro lado la varible tierras dedicada a cultivo agricolas se considero pertienente
## analizar su significancia individual con un alpha de 0.15

#______________________________________________________________________________#
# Prueba de signficacia global ----
#______________________________________________________________________________#


car::linearHypothesis(modelo_ipm_inasistencia_escolar, c("ipc_Salud=0", "ipc_Vestuario=0",
                                      "ipc_Educación=0", "ipc_Comunicaciones  =0" ))

#______________________________________________________________________________#
# Cambio estructural- Prueba de Chow ----
#______________________________________________________________________________#


library(strucchange)
sctest(modelo_ipm_inasistencia_escolar, type = "Chow")


## Dado que en todas las pruebas el p-valor es mayor  que 0.05 no rechazamos la hipotesis 
## nula y vemos que pasa la prueba de chow por tanto no hay cambio estructural


#___________________________
# Prueba RESET de Ramsey ----                                                  
#___________________________
# Descargamos el paquete para realizar la prueba RESET de Ramsey 


library(lmtest)

resettest(modelo_ipm_inasistencia_escolar)

# Ho: Y1=Y2=0 (correcta especificación)
# Ha: al menos un gamma es diferente de 0 
# En este caso 0.05 < p-value = 0.1219 por lo que se acepta Ho, y se concluye que 
# no hay error en la especificación del modelo. 

#______________________________________________________________________________#
# NO MULTICOLINEALIDAD   ----
#______________________________________________________________________________#

#Matriz de correlaciones 
#Multicolinealidad si algún coeficiente es mayor a 0,8.

round(cor(x = Variaciones_Anuales_IPC, method = "pearson"), 3) 


corrplot(cor(dplyr::select(Variaciones_Anuales_IPC, ipm_inasis_escolar ,   ipc_Vestuario , 
                                         ipc_Salud , ipc_Educación , ipc_Comunicaciones)),
         method = "number", tl.col = "black")

library(car)
vif(modelo_ipm_inasistencia_escolar)

# Ningun coeficiente es mayor a 0.8 por tanto no hay multicolinealidad, sin embargo 
# se puede considerar que existe cierta autocorrelacion de algunas variables y por
# ello sacamos vif el cual vemos que esta dentro de lo aceptable dado que en todas
# las variabels es menor a 10. 

#______________________________________________________________________________#
# HOMOCEDASTICIDAD   ----
#______________________________________________________________________________#

library(car)     
library(lmtest)


#Prueba Breush-Pagan(Ho:Homocedasticidad)

bptest(modelo_ipm_inasistencia_escolar)

## Dado el resultado se tiene y p-value > 0.05 y se concluye que el modelo no 
## tiene problemas de homocedasticidad.

#______________________________________________________________________________#
# NO AUTOCORRELACIÓN:   ----
#______________________________________________________________________________#

#Durbin Watson test (Ho:No autocorrelación de 1er orden)

dwtest(modelo_ipm_inasistencia_escolar)
durbinWatsonTest(modelo_ipm_inasistencia_escolar)

#Prueba Breush-Godfrey (Ho:No autocorrelación de orden p)

bgtest(modelo_ipm_inasistencia_escolar)

## Dado el resultado se tiene y p-value > 0.05 y se concluye que el modelo no 
## tiene problemas de autocorrelacion

#______________________________________________________________________________#
# Normalidad:   ----
#______________________________________________________________________________#


library(tseries)

jarque.bera.test(modelo_ipm_inasistencia_escolar$residuals)


## Dado el resultado se tiene y p-value > 0.05 y se concluye que el modelo presenta 
## normalidad en sus datos

## Comprobacion grafica

qqnorm(modelo_ipm_inasistencia_escolar$residuals)
qqline(modelo_ipm_inasistencia_escolar$residuals)
shapiro.test(modelo_ipm_inasistencia_escolar$residuals)





#______________________________________________________________________________#
# MODELO 3 DESEMPLEO----
#______________________________________________________________________________#



## Tabla resultados modelo para latex ----

library(stargazer)
stargazer(modelo_ipm_desmpleo, type="latex", title="Modelo", report=('vc*p'))



#______________________________________________________________________________#
#  Prueba de significancia individual ----
#______________________________________________________________________________#

summary(modelo_ipm_desmpleo)

#______________________________________________________________________________#
# intervalos de confianza ----
#______________________________________________________________________________#



confint(lm(ipm ~ ipc_Alimentos + ipc_Vivienda  + 
             ipc_Educación + ipc_Transporte, data = Variaciones_Anuales_IPC))


## Observando el resumen del modelo observamos que todas la varibales son significativas
## sin embargo es de resaltar que las variables produccion y tierras_agricolas 
## bajo un alpha del 0.1 % son significativas
## Por otro lado la varible tierras dedicada a cultivo agricolas se considero pertienente
## analizar su significancia individual con un alpha de 0.15

#______________________________________________________________________________#
# Prueba de signficacia global ----
#______________________________________________________________________________#


car::linearHypothesis(modelo_ipm_desmpleo, c("ipc_Alimentos=0", "ipc_Vivienda=0",
                                                         "ipc_Educación=0", "ipc_Transporte=0" ))

#______________________________________________________________________________#
# Cambio estructural- Prueba de Chow ----
#______________________________________________________________________________#


library(strucchange)
sctest(modelo_ipm_desmpleo, type = "Chow")


## Dado que en todas las pruebas el p-valor es mayor  que 0.05 no rechazamos la hipotesis 
## nula y vemos que pasa la prueba de chow por tanto no hay cambio estructural


#___________________________
# Prueba RESET de Ramsey ----                                                  
#___________________________
# Descargamos el paquete para realizar la prueba RESET de Ramsey 


library(lmtest)

resettest(modelo_ipm_desmpleo)

# Ho: Y1=Y2=0 (correcta especificación)
# Ha: al menos un gamma es diferente de 0 
# En este caso 0.05 < p-value = 0.1219 por lo que se acepta Ho, y se concluye que 
# no hay error en la especificación del modelo. nuestros p valores son mayores que la signifi

#______________________________________________________________________________#
# NO MULTICOLINEALIDAD   ----
#______________________________________________________________________________#

#Matriz de correlaciones 
#Multicolinealidad si algún coeficiente es mayor a 0,8.

round(cor(x = Variaciones_Anuales_IPC, method = "pearson"), 3) 

library(corrplot)

corrplot(cor(dplyr::select(Variaciones_Anuales_IPC, ipm_Desempleo  , ipc_Alimentos , ipc_Vivienda , ipc_Educación , 
                             ipc_Transporte)),
         method = "number", tl.col = "black")

library(car)
vif(modelo_ipm_desmpleo)

# Ningun coeficiente es mayor a 0.8 por tanto no hay multicolinealidad, sin embargo 
# se puede considerar que existe cierta autocorrelacion de algunas variables y por
# ello sacamos vif el cual vemos que esta dentro de lo aceptable dado que en todas
# las variabels es menor a 10. 

#______________________________________________________________________________#
# HOMOCEDASTICIDAD   ----
#______________________________________________________________________________#

library(car)     
library(lmtest)


#Prueba Breush-Pagan(Ho:Homocedasticidad)

bptest(modelo_ipm_desmpleo)

## Dado el resultado se tiene y p-value > 0.05 y se concluye que el modelo no 
## tiene problemas de homocedasticidad.

#______________________________________________________________________________#
# NO AUTOCORRELACIÓN:   ----
#______________________________________________________________________________#

#Durbin Watson test (Ho:No autocorrelación de 1er orden)

dwtest(modelo_ipm_desmpleo)
durbinWatsonTest(modelo_ipm_desmpleo)

#Prueba Breush-Godfrey (Ho:No autocorrelación de orden p)

bgtest(modelo_ipm_desmpleo)

## Dado el resultado se tiene y p-value > 0.05 y se concluye que el modelo no 
## tiene problemas de autocorrelacion

#______________________________________________________________________________#
# Normalidad:   ----
#______________________________________________________________________________#


library(tseries)

jarque.bera.test(modelo_ipm_desmpleo$residuals)


## Dado el resultado se tiene y p-value > 0.05 y se concluye que el modelo presenta 
## normalidad en sus datos

## Comprobacion grafica

qqnorm(modelo_ipm_desmpleo$residuals)
n3 <- qqline(modelo_ipm_desmpleo$residuals)
shapiro.test(modelo_ipm_desmpleo$residuals)

## PLOT con todos 

library("car")
qqPlot(modelo_ipm_desmpleo$residuals)
qqPlot(modelo_ipm$residuals)
qqPlot(modelo_ipm_inasistencia_escolar$residuals)
library(patchwork)
(q2 | q1) / q3





