# algoritmO
setwd("C:/Users/oscar/Desktop/R")
library(tidyverse)
library(RColorBrewer)
library(scales)
library(readxl)
library(viridis)
library(tseries)
library(urca)
library(forecast)
#Importar la base
base <- read_excel("digitas.xlsx", sheet = "Base Punto 5", col_names = T)
#Loop de prueba
if  (base$Palabras[1] %in% ("halitosis")){
  print("Sí está")
} else { 
  print("No")
}
#Se puede realizar mediante un case when
base <- base %>% mutate(categoria = case_when(
  Palabras %in% ("halitosis") ~ "Halitosis",
  Palabras %in% c("dolor", "sangrado", "sangre", "calculo", "infecciones", "patologias",
                  "hemorragias","hemorragia", "muela", "gingivitis", "gingivits", "rehabilitacion") ~ "Dolores",
  Palabras  %in% c("brakets", "brackets") ~ "Brackets",
  Palabras  %in% c("cuidado","aseo", "encias", "colgate", "higiene", "cepillado", "lavado",
                   "lavar", "limpia", "limpiar", "aliento") ~ "Limpieza",
  Palabras  %in% c("pastas", "tratamientos", "problemas", "causa", "eliminacion", "fortalecer", 
                   "destemplado", "pastas", "pastillas", "destemplar", "calcio", "placa") ~ "Tratamientos",
  Palabras %in% c("blanquear", "blanqueamiento", "bicarbonato", "dientes", "los", "como") ~ "Blanqueamiento",
  Palabras %in% c("sensibilidad", "hipersensibilidad") ~ "Sensibilidad",
  Palabras %in% c("mal", "aliento") ~ "Aliento",
  Palabras %in% c("caries") ~ "Caries",
  Palabras %in% c("vitaminas") ~ "Vitaminas",
  Palabras %in% c("el","sarro") ~ "Sarro",
  T  ~  "Otros"
)
)

#cuantos permanecen en otros
print(paste("La categoria ´Otros´ representa el",
            round((nrow(base %>% filter(categoria == "Otros"))/nrow(base))*100,2),"%"))
#blanqueamiento
print(paste("La categoria ´Blanqueamiento´ representa el",
            round((nrow(base %>% filter(categoria == "Blanqueamiento"))/nrow(base))*100,2),"%"))
#limpieza
print(paste("La categoria ´Limpieza´ representa el",
            round((nrow(base %>% filter(categoria == "Limpieza"))/nrow(base))*100,2),"%"))

#plot
base %>% filter(categoria != "Otros") %>% ggplot(mapping = aes(categoria)) + geom_bar(fill = "slateblue4" ) + theme_classic() +
    labs(title = "Categorias de las busquedas de OralB", x = "",
         subtitle = "La categoria ´Otros´ representa el 35.21 %") + scale_y_continuous(breaks = seq(0,160,20))
#agrupar
base_agrupada <- base %>% group_by(categoria) %>% summarise(mean(`Searches: May 2020`),mean(`Searches: Jun 2020`),mean(`Searches: Jul 2020`),
                                           mean(`Searches: Aug 2020`), mean(`Searches: Sep 2020`), mean(`Searches: Oct 2020`),
                                           mean(`Searches: Nov 2020`), mean(`Searches: Dec 2020`), mean(`Searches: Jan 2021`),
                                           mean(`Searches: Feb 2021`), mean(`Searches: Mar 2021`), mean(`Searches: Apr 2021`))

#Las variables que se utilizaran se ponen en proyecciones
#blanqueamiento
blanq_ts <- as.vector(t(base_agrupada[2,-1]))
blanq_ts <- ts(blanq_ts, start = c(2020,5), end = c(2021,4), frequency = 12)
plot(blanq_ts)
#brackets
brack_ts <- as.vector(t(base_agrupada[3,-1]))
brack_ts <- ts(brack_ts, start = c(2020,5), end = c(2021,4), frequency = 12)
plot(brack_ts)
#limpieza
limpieza_ts <- as.vector(t(base_agrupada[7,-1]))
limpieza_ts <- ts(limpieza_ts, start = c(2020,5), end = c(2021,4), frequency = 12)
plot(limpieza_ts)

#modelo
#Blanqueamiento
modelo_blanq <- auto.arima(blanq_ts, seasonal=T, stepwise=T, approximation=T)
autoplot(forecast(modelo_blanq, h=3)) + labs(title = "Pronóstico categoria Blanqueamiento") +
  theme_light()
summary(modelo_blanq)
#brackets
modelo_brack <- auto.arima(brack_ts, seasonal=T, stepwise=T, approximation=T)
autoplot(forecast(modelo_brack, h=3)) + labs(title = "Pronóstico categoria Brackets") +
  theme_light()
summary(modelo_brack)
# 
modelo_limpieza <- auto.arima(limpieza_ts, seasonal=T, stepwise=T, approximation=T)
autoplot(forecast(modelo_limpieza, h=3)) + labs(title = "Pronóstico categoria Limpieza") +
  theme_light()
summary(modelo_limpieza)
