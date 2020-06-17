# =======================================
# =         Trending analysis           =          
# =           Daniel Jiménez            =
# =       Senior Data Scientist         =
# =            2020-06-17               =
# =======================================

# Objetivo : Presentar el primer paso para desarrollar un análisis de tendencia
# para el desarrollo de campañas digitales
# Paso seguido descubrir los insigths claves que esta data arroja

## librerias -------------------
library(tidyverse); # Análisis y manipulación de datos
library(gtrendsR);# API para comunicar R con Google Trends
library(broom); # Análisis estádisticos de objetos
theme_set(theme_classic())

## Definiendo una busqueda
# Voy a analizar la tendencia del COVID y Anonymous

res<-gtrends(c("coronavirus","anonymus"))

## Explorando un poco la data -------

general<-res$interest_over_time

plot(res)
# Ese gráfico no dice much0o y acá viene el primer insigths 
# Limitar el tiempo


general_2020<-general%>%
  mutate(year=lubridate::year(date))%>%
  filter(year==2020)




general_2020%>%
  mutate(hits=str_replace(hits,"<1","1"))%>%
  mutate(hits=as.numeric(hits))%>%
  ggplot(aes(x=date,y=hits,color=keyword))+
  geom_line()+
  labs(title = 'Relación tendencia entre',
       subtitle = 'Coronavirus y Anonymus')





## Ahora análisis por geo ---------

geo_referencial<-res$interest_by_country
  
geo_referencial%>%
  filter(location=="Colombia")%>%
  ggplot(aes(keyword,hits,fill=keyword))+
  geom_col(show.legend = TRUE)+
  coord_flip()
  
geo_referencial%>%
  mutate(location=factor(location),
         hits=as.numeric(hits),
         keyword=factor(keyword))%>%
  filter(!is.na(hits))%>%
  dplyr::select(location,hits,keyword)%>%
  na.omit()->referenciacion



referenciacion%>%
  filter(hits>=10)%>%
  mutate(location=fct_lump(location,10),
         keyword=fct_lump(keyword,2))%>%
  mutate(keyword=fct_reorder(keyword,hits))%>%
  filter(location!='Other')%>%
  ungroup()%>%
  ggplot(aes(keyword,hits,fill=keyword))+
  geom_bar(stat = 'identity',position = 'dodge')+
  facet_wrap(~location, scales = 'free_x')+
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

  




  

