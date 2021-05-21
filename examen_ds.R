setwd('C:/Users/jontx/OneDrive/Escritorio/examen_ds')

library(dplyr)
library(visdat)
library(naniar)
library(VIM)
library(BBmisc)
library(discretization)
library(smoothmest)
library(readxl)
library(ggplot2)
library(readr)
library(dplyr)
library(ggplot2movies)
library(visdat)
library(VIM)
library(BBmisc)
library(naniar)
library(dummies)
install.packages("Dummies")
library(digest)

#1
df <- read_excel("C:/Users/jontx/OneDrive/Escritorio/examen_ds/SBAnational.xlsx")
View(df)
str(df)
df$ChgOffPrinGr<- as.logical(df$ChgOffPrinGr)
df$City<- as.factor(df$City)
df$ApprovalDate<- as.Date(df$ApprovalDate)
df$disbursementdate<- as.Date(df$disbursementdate)
df$ChgOffDate<- as.Date(df$ChgOffDate)
df$ApprovalFY<- as.factor(df$ApprovalDate)
df$RevLineCr<- as.factor(df$RevLineCr)
df$LowDoc<- as.factor(df$LowDoc)
df$NewExist<- as.factor(df$NewExist)

df$UrbanRural<- as.factor(df$UrbanRural)
str(df)

# Sacar un summary (informacion para el data discovery/NA´s..) sobre cada columna de df:
estad_df<- as.data.frame(summary(df[,-c(2,3,4,6,7,9,18,19,20,21,24)]))
estad_df
ciudades<- as.vector(distinct(as.data.frame(df[,c(2,3)])))# Sacar un vector con los ciudades y nombres
ciudades

#2. Anonimiza mediante hasheo el identificador de la persona que pide el préstamo. Justifica la función utilizada
df1<- df%>%
  mutate(var=1, length=df)
df1

Loannr<- df[,1]
Loannr_anonimizar<- as.data.frame(apply(Loannr,1,digest))

df2<- df %>% 
  mutate(Loannr= sapply(Loannr, digest))
#He utilizado la funcion de digest ya que es la mas sencilla para anonimizar
#3
##Trabajamos las ausencias:
#df

# missing implícitos:


miss_case_summary(df)
miss_var_summary(df)
# tenemos que decidir con que completar los missings
gg_miss_var(df, facet=) # Podemos observar los valores ausentes o NA en la variable ChgoffDate
# grafico para ver los missings (libreria naniar creo)
gg_miss_var(df)
# estamos viendo opciones con las que sustituir los missings,
# aqui probamos para ver por pais y por año: (no dicen mucho..)

sum_ausencias_ciudad <- df %>%
  mutate(var=apply(is.na(df)[,-c((2,3,4,6,7,9,18,19,20,21,24)], 1, sum))) %>%
  group_by (ApprovalFY,City) %>%
  summarize(sum(var))

#outliers
boxplot(as.data.frame(df[,-c(1,2,3,4,6,7,9,18,19,20,21,22,24)])) # la DisbursementGross tiene outlier, por lo tanto para el proximo boxplot la quitamos
boxplot(as.data.frame(df[,-c(1,2,3,4,6,7,9,18,19,20,21,22,24,26,27)])) # la columna 26 y 27 tambien contienen sus outliers y en la columna de balance gross hay un outlier de 827875
boxplot(as.data.frame(df[,-c(1,2,3,4,6,7,9,18,19,20,21,22,23,24,26,27)])) # Aqui podemos ver que todos los valores son parecidos

#4
#las ausencias son de tipo MCAR, por lo que imputar es la mejor opción
#en este caso, un sistema de imputación kNN utilizando como variables para
#medir la distancia el pais y el año encaja bien

for(i in (20)) 
{df <- kNN(df, colnames(df)[i], dist_var=c("City", "ApprovalFY"))}
# year y Country_name no tienen ausetntes y los usaremos
vis_miss(WHR) # (en la columna 20 es donde hay ausentes)
miss_var_summary(df)

quitarausencias <- hotdeck(df, variable = c("ChgOffDate"), domain_var="City")
miss_var_summary(quitarausencias)
gg_miss_var(quitarausencias)
#5



#6. Anonimizo consulta
Balancegross_media <- df %>%
  group_by(Zip) %>%
  summarize(BalanceGross=mean(BalanceGross))
Balancegross_media

epsilon=0.1
a<-min(df$BalanceGross)
b<-max(df$BalanceGross)
n<-nrow(df)
gs <- (b-a)/n

BalanceGross_media_anon <- cbind(as.data.frame(Balancegross_media[,c(1)]), round(rdoublex(94765, Balancegross_media[,2], gs/epsilon),2))
BalanceGross_media_anon
#7. Transforma la variable UrbanRural en variables dummies. ¿Cuántas dummies son necesarias para no perder información?
df4<- dummy(df[,17])
df4
df3<- df[,17]
urbanrural<- dummy_cols(df, select_columns= NULL, remove_firs)
#Una dummy es necesaria para no perder informacion el caso de la variable Urbanrural


