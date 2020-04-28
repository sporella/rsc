library(tidyverse)
library(lubridate)
library(janitor)

# Cargo datos
datos <- read_csv("data/agrometeorologia-20200427190246.csv", skip = 5)

# Limpio nombres son clean_names() del paquete janitor
datos <- clean_names(datos)

# Visualizo conversión a fecha
datos %>% 
  mutate(fecha = dmy(tiempo_utc_4)) %>% 
  View()

# Quito nas finales por cadenas de texto
datos <- datos %>% 
  mutate(fecha = dmy(tiempo_utc_4)) %>% 
  filter(!is.na(fecha))

# Calculo temperatura media diaria y agregados mensuales
datos_mensuales <- datos %>% 
  mutate(temperatura_media = (temperatura_del_aire_minima_º_c + temperatura_del_aire_maxima_º_c)/2) %>% 
  group_by(month=month(fecha), year=year(fecha)) %>% 
  summarise(max_max=max(temperatura_del_aire_maxima_º_c),
            max_min=max(temperatura_del_aire_minima_º_c),
            media = mean(temperatura_media)) %>% 
  ungroup() 
  
# Ejemplo con pivot wider para tablas "de informe"
pivot_wider(select(datos_mensuales, year, month, media),  names_from = month, values_from = media)

# Ejemplo con ggplot y scale_x_date()
datos_mensuales %>%  
  ggplot()+
  aes(x= ymd(paste0(year,"-", month, "-01")), y=media)+
  geom_line(colour="red2")+
  # en date_labels se puede jugar con el formato de la etiqueta
  # en date_breaks se establece el intervalo de tiempo en "humano"
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months")+
  labs(x="", y="Temperatura media mensual °C", title = "Estación La Platina")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5))