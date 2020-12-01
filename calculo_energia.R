require(stringr)
require(readr)
require(dplyr)
require(tibble)
require(purrr)
require(ggplot2)
require(forcats)
require(gridExtra)
# require(mosaic)
# require(ggformula)

# Sys.setenv("DISPLAY" = ":0") # to make View works on vscode

# work_dir_gen <- "/Users/andres/Google Drive/14_USAL/2018.19. Academic Info/03.TFG/Sergio Gonzalez Moreno/Informacion MotoStudent UR - Falces"
work_dir_gen <- "/Users/sergiogonzalez/Desktop/TFG Eléctrica/Analisis mediante R/Prueba_data_2016"
# Settings
# pattern <- "electric" # Electric bike in 2016
# pattern <- "petrol" # Combustion bike in 2016
pattern <- "log_motostudent_2018_mod" # Electric bike in 2018


if (pattern == "electric") {
  dir_data <- paste(work_dir_gen, "data/2016", sep = "/")
  dir_results <- "/results/2016/"
  num_cols <- 7
} else if (pattern == "petrol") {
  dir_data <- paste(work_dir_gen, "data/2016", sep = "/")
  dir_results <- "/results/2016/"
  num_cols <- 9
}else if (pattern == "log_motostudent_2018_mod") {
  dir_data <- paste(work_dir_gen, "data/2018", sep = "/")
  dir_results <- "/results/2018/"
  num_cols <- 7
}
archivos  <- dir(dir_data)

data_raw  <- read_csv(paste0(dir_data, "/",
                             archivos[which(str_detect(archivos, pattern))]),
                      col_types = cols(
                        x1 = col_double(),
                        x2 = col_double(),
                        x3 = col_double(),
                        x4 = col_double(),
                        x5 = col_double(),
                        x6 = col_double(),
                        x7 = col_double()))

# summary(data_raw)
if (pattern == "log_motostudent_2018_mod") {
  # Remove x7 because it is empty
  data_raw  <- data_raw %>% select(-x7)
}
# Add index to work with the data
data_raw <- data_raw %>% mutate(index = seq_len(n()))

# vars <- paste0("x", "1")
vars <- paste0("x", c(1:6))


#------------------

# aislamiento del caso 547 
caso_547 <- filter(data_raw, x2 == "547")

# sacar los ms del caso 547
ms_caso_547 <- select(caso_547, x1)

# Obtencion final de la corriente del motor AC

# valor la corriente del motor AC (canal 2 del caso 547 ) 
valor_corriente_motor <- select(caso_547, x4)

# unir valor de la corriente del motor AC y los ms para representacion 
representacion_valor_corriente_motor = cbind(ms_caso_547, valor_corriente_motor)

# renombrar la columna x1
representacion_valor_corriente_motor <- rename(representacion_valor_corriente_motor, tiempo=x1)

# renombrar la columna x4
representacion_valor_corriente_motor <- rename(representacion_valor_corriente_motor, corriente=x4)

# limitar a la zona izquierda el valor de la corriente del motor AC
carrera_valor_corriente_motor <- slice(representacion_valor_corriente_motor, 0:200000)

# limitar a la zona izquierda el valor de la corriente del motor AC
carrera_valor_corriente_motor <- filter(representacion_valor_corriente_motor,tiempo <= 3000000 )

# busqueda mas concreta de la carrera a partir del valor de la corriente del motor AC
carrera_2_valor_corriente_motor <- filter(carrera_valor_corriente_motor,tiempo <= 2600000 )

# busqueda 3 de la carrera a partir del valor de la corriente del motor AC
carrera_3_valor_corriente_motor <- filter(carrera_2_valor_corriente_motor,tiempo >= 1896547 )

# busqueda 4 de la carrera a partir del valor de la corriente del motor AC
carrera_4_valor_corriente_motor <- filter(carrera_2_valor_corriente_motor,tiempo >= 1500000 )

# busqueda 5 de la carrera a partir del valor de la corriente del motor AC 
carrera_5_valor_corriente_motor <- filter(carrera_2_valor_corriente_motor,tiempo >= 1000000 )

# busqueda 6 de la carrera a partir del valor de la corriente del motor AC
carrera_6_valor_corriente_motor <- filter(carrera_5_valor_corriente_motor,tiempo <= 2400000 )

# busqueda 7 de la carrera a partir del valor de la corriente del motor AC
carrera_7_valor_corriente_motor <- filter(carrera_5_valor_corriente_motor,tiempo <= 2450000 )

# busqueda 8 de la carrera a partir del valor de la corriente del motor AC
carrera_8_valor_corriente_motor <- filter(carrera_7_valor_corriente_motor,tiempo >= 1700000 )

# busqueda 9 de la carrera a partir del valor de la corriente del motor AC
carrera_9_valor_corriente_motor <- filter(carrera_7_valor_corriente_motor,tiempo >= 1250000 )

# busqueda 10 de la carrera a partir del valor de la corriente del motor AC
carrera_10_valor_corriente_motor <- filter(carrera_7_valor_corriente_motor,tiempo >= 1312500 )

# busqueda vuelta1 de la carrera a partir del valor de la corriente del motor AC
vuelta_1_valor_corriente_motor <- filter(carrera_10_valor_corriente_motor,tiempo <= 1493483 )

# Eje del tiempo en segundos 
vuelta_1_valor_corriente_motor <- vuelta_1_valor_corriente_motor %>% mutate(tiempos = tiempo/1000)

# Representacion de la vuelta 1 de carrera del valor de la corriente del motor AC
grafica_vuelta_1_corriente_motor <- ggplot(vuelta_1_valor_corriente_motor, aes(x = tiempos, y = corriente, )) 
grafica_vuelta_1_corriente_motor + geom_point(color="red", size=0.5) + theme_set(theme_bw()) + ggtitle ("Gráfica corriente del motor AC vuelta 1") + theme(plot.title = element_text(vjust=1)) + labs(x = "Tiempo (s)",y = "Corriente del motor AC (A)") + theme(plot.title = element_text(hjust = 0.5)) 


# busqueda vuelta2 de la carrera a partir de la corriente del motor AC
vuelta_2_inicio_valor_corriente_motor <- filter(carrera_10_valor_corriente_motor,tiempo >= 1493483 )

# busqueda vuelta2 de la carrera a partir de la corriente del motor AC
vuelta_2_valor_corriente_motor <- filter(vuelta_2_inicio_valor_corriente_motor,tiempo <= 1655647 )

# Eje del tiempo en segundos 
vuelta_2_valor_corriente_motor <- vuelta_2_valor_corriente_motor %>% mutate(tiempos = tiempo/1000)

# Representacion de la vuelta 2 de carrera del valor de la corriente del motor AC
grafica_vuelta_2_corriente_motor <- ggplot(vuelta_2_valor_corriente_motor, aes(x = tiempos, y = corriente, )) 
grafica_vuelta_2_corriente_motor + geom_point(color="blue", size=0.5) + theme_set(theme_bw()) + ggtitle ("Gráfica corriente del motor AC vuelta 2") + theme(plot.title = element_text(vjust=1)) + labs(x = "Tiempo (s)",y = "Corriente del motor AC (A)") + theme(plot.title = element_text(hjust = 0.5)) 

# busqueda vuelta3 de la carrera a partir del valor de la corriente del motor AC
vuelta_3_inicio_valor_corriente_motor <- filter(carrera_10_valor_corriente_motor,tiempo >= 1655647 )

# busqueda vuelta3 de la carrera a partir del valor de la corriente del motor AC
vuelta_3_valor_corriente_motor <- filter(vuelta_3_inicio_valor_corriente_motor,tiempo <= 1828336 )

# Eje del tiempo en segundos 
vuelta_3_valor_corriente_motor <- vuelta_3_valor_corriente_motor %>% mutate(tiempos = tiempo/1000)

# Representacion de la vuelta 3 de carrera del valor de la corriente del motor AC
grafica_vuelta_3_corriente_motor <- ggplot(vuelta_3_valor_corriente_motor, aes(x = tiempos, y = corriente, )) 
grafica_vuelta_3_corriente_motor + geom_point(color="green", size=0.5) + theme_set(theme_bw()) + ggtitle ("Gráfica corriente del motor AC vuelta 3") + theme(plot.title = element_text(vjust=1)) + labs(x = "Tiempo (s)",y = "Corriente del motor AC (A)") + theme(plot.title = element_text(hjust = 0.5)) 

# busqueda vuelta4 de la carrera a partir del valor de la corriente del motor AC 
vuelta_4_inicio_valor_corriente_motor <- filter(carrera_10_valor_corriente_motor,tiempo >= 1828336 )

# busqueda vuelta4 de la carrera a partir del valor de la corriente del motor AC
vuelta_4_valor_corriente_motor <- filter(vuelta_4_inicio_valor_corriente_motor,tiempo <= 2005790 )

# Eje del tiempo en segundos 
vuelta_4_valor_corriente_motor <- vuelta_4_valor_corriente_motor %>% mutate(tiempos = tiempo/1000)

# Representacion de la vuelta 4 de carrera de la corriente del motor AC
grafica_vuelta_4_corriente_motor <- ggplot(vuelta_4_valor_corriente_motor, aes(x = tiempos, y = corriente, )) 
grafica_vuelta_4_corriente_motor + geom_point(color="magenta", size=0.5) + theme_set(theme_bw()) + ggtitle ("Gráfica corriente del motor AC vuelta 4") + theme(plot.title = element_text(vjust=1)) + labs(x = "Tiempo (s)",y = "Corriente del motor AC (A)") + theme(plot.title = element_text(hjust = 0.5)) 

# -----------------

# Obtencion tension del motor AC

# valor tension del motor AC (canal 3 del caso 547 ) 
valor_tension_motor <- select(caso_547, x5)

# unir valor real de la tension del motor AC y los ms para representacion 
representacion_valor_tension_motor = cbind(ms_caso_547, valor_tension_motor)

# renombrar la columna x1
representacion_valor_tension_motor <- rename(representacion_valor_tension_motor, tiempo=x1)

# renombrar la columna x5
representacion_valor_tension_motor <- rename(representacion_valor_tension_motor, tension=x5)

# limitar a la zona izquierda el valor de la tension del motor AC
carrera_valor_tension_motor <- slice(representacion_valor_tension_motor, 0:200000)

# limitar a la zona izquierda el valor de la tension del motor AC
carrera_valor_tension_motor <- filter(representacion_valor_tension_motor,tiempo <= 3000000 )

# busqueda mas concreta de la carrera a partir del valor de la tension del motor AC 
carrera_2_valor_tension_motor <- filter(carrera_valor_tension_motor,tiempo <= 2600000 )

# busqueda 3 de la carrera a partir del valor de la tension del motor AC
carrera_3_valor_tension_motor <- filter(carrera_2_valor_tension_motor,tiempo >= 1896547 )

# busqueda 4 de la carrera a partir del valor de la tension del motor AC
carrera_4_valor_tension_motor <- filter(carrera_2_valor_tension_motor,tiempo >= 1500000 )

# busqueda 5 de la carrera a partir del valor de la tension del motor AC
carrera_5_valor_tension_motor <- filter(carrera_2_valor_tension_motor,tiempo >= 1000000 )

# busqueda 6 de la carrera a partir del valor de la tension del motor AC
carrera_6_valor_tension_motor <- filter(carrera_5_valor_tension_motor,tiempo <= 2400000 )

# busqueda 7 de la carrera a partir del valor de la tension del motor AC
carrera_7_valor_tension_motor <- filter(carrera_5_valor_tension_motor,tiempo <= 2450000 )

# busqueda 8 de la carrera a partir del valor de la tension del motor AC
carrera_8_valor_tension_motor <- filter(carrera_7_valor_tension_motor,tiempo >= 1700000 )

# busqueda 9 de la carrera a partir del valor de la tension del motor AC 
carrera_9_valor_tension_motor <- filter(carrera_7_valor_tension_motor,tiempo >= 1250000 )

# busqueda 10 de la carrera a partir del valor de la tension del motor AC 
carrera_10_valor_tension_motor <- filter(carrera_7_valor_tension_motor,tiempo >= 1312500 )

# busqueda vuelta1 de la carrera a partir del valor de la tension del motor AC
vuelta_1_valor_tension_motor <- filter(carrera_10_valor_tension_motor,tiempo <= 1493483 )

# Eje del tiempo en segundos 
vuelta_1_valor_tension_motor <- vuelta_1_valor_tension_motor %>% mutate(tiempos = tiempo/1000)

# Representacion de la vuelta 1 de carrera del valor de la tension del motor AC
grafica_vuelta_1_valor_tension_motor <- ggplot(vuelta_1_valor_tension_motor, aes(x = tiempos, y = tension, )) 
grafica_vuelta_1_valor_tension_motor + geom_point(color="red", size=0.5) + theme_set(theme_bw()) + ggtitle ("Gráfica tensión del motor AC vuelta 1") + theme(plot.title = element_text(vjust=1)) + labs(x = "Tiempo (s)",y = "Tensión del motor AC (V)") + theme(plot.title = element_text(hjust = 0.5)) 

# busqueda vuelta2 de la carrera a partir del valor de la tension del motor AC
vuelta_2_inicio_valor_tension_motor <- filter(carrera_10_valor_tension_motor,tiempo >= 1493483 )

# busqueda vuelta2 de la carrera a partir del valor de la tension del motor AC
vuelta_2_valor_tension_motor <- filter(vuelta_2_inicio_valor_tension_motor,tiempo <= 1655647 )

# Eje del tiempo en segundos 
vuelta_2_valor_tension_motor <- vuelta_2_valor_tension_motor %>% mutate(tiempos = tiempo/1000)

# Representacion de la vuelta 2 de carrera del valor de la tension del motor AC
grafica_vuelta_2_valor_tension_motor <- ggplot(vuelta_2_valor_tension_motor, aes(x = tiempos, y = tension, )) 
grafica_vuelta_2_valor_tension_motor + geom_point(color="blue", size=0.5) + theme_set(theme_bw()) + ggtitle ("Gráfica tensión del motor AC vuelta 2") + theme(plot.title = element_text(vjust=1)) + labs(x = "Tiempo (s)",y = "Tensión del motor AC (V)") + theme(plot.title = element_text(hjust = 0.5)) 

# busqueda vuelta3 de la carrera a partir del valor de la tension del motor AC
vuelta_3_inicio_valor_tension_motor <- filter(carrera_10_valor_tension_motor,tiempo >= 1655647 )

# busqueda vuelta3 de la carrera a partir del valor de la tension del motor AC
vuelta_3_valor_tension_motor <- filter(vuelta_3_inicio_valor_tension_motor,tiempo <= 1828336 )

# Eje del tiempo en segundos 
vuelta_3_valor_tension_motor <- vuelta_3_valor_tension_motor %>% mutate(tiempos = tiempo/1000)

# Representacion de la vuelta 3 de carrera del valor de la tension del motor AC
grafica_vuelta_3_tension_motor <- ggplot(vuelta_3_valor_tension_motor, aes(x = tiempos, y = tension, )) 
grafica_vuelta_3_tension_motor + geom_point(color="green", size=0.5) + theme_set(theme_bw()) + ggtitle ("Gráfica tensión del motor AC vuelta 3") + theme(plot.title = element_text(vjust=1)) + labs(x = "Tiempo (s)",y = "Tensión del motor AC (V)") + theme(plot.title = element_text(hjust = 0.5)) 

# busqueda vuelta4 de la carrera a partir del valor de la tension del motor AC
vuelta_4_inicio_valor_tension_motor <- filter(carrera_10_valor_tension_motor,tiempo >= 1828336 )

# busqueda vuelta4 de la carrera a partir del valor de la tensión del motor AC
vuelta_4_valor_tension_motor <- filter(vuelta_4_inicio_valor_tension_motor,tiempo <= 2005790 )

# Eje del tiempo en segundos 
vuelta_4_valor_tension_motor <- vuelta_4_valor_tension_motor %>% mutate(tiempos = tiempo/1000)

# Representacion de la vuelta 4 de carrera del valor de la tension del motor AC
grafica_vuelta_4_tension_motor <- ggplot(vuelta_4_valor_tension_motor, aes(x = tiempos, y = tension, )) 
grafica_vuelta_4_tension_motor + geom_point(color="magenta", size=0.5) + theme_set(theme_bw()) + ggtitle ("Gráfica tensión del motor AC vuelta 4") + theme(plot.title = element_text(vjust=1)) + labs(x = "Tiempo (s)",y = "Tensión del motor AC (V)") + theme(plot.title = element_text(hjust = 0.5)) 

# -----------------

# Calculo de la potencia en cada instante medido vuelta 3 

# Aislamiento de la tension en la vuelta 3
tension_vuelta_3_potencia <- select(vuelta_3_valor_tension_motor, tension)

# Aislamiento de la corriente en la vuelta 3
corriente_vuelta_3_potencia <- select(vuelta_3_valor_corriente_motor, corriente)

# Formacion dataframe potencia 
potencia_vuelta3 = cbind(corriente_vuelta_3_potencia, tension_vuelta_3_potencia)

# Calculo de la potencia en cada instante
aux <- aux %>% mutate(potencia = corriente*tension)


potencia_vuelta3 %>% select(potencia) %>% slice(., 1:36315) %>% sum()


# Aislamiento del tiempo en la vuelta 3
tiempo_vuelta_3_potencia <- select(vuelta_3_valor_tension_motor, tiempos)

# unir el tiempo de la vuelta 3 y el valor de la potencia
representacion_potencia_vuelta3 = cbind(potencia_vuelta3, tiempo_vuelta_3_potencia)

# Calculo del sumatorio de potencia
aux <- representacion_potencia_vuelta3 %>% mutate(tiempos_prev = lag(tiempos)) %>% slice(-1)
aux <- aux %>% mutate(dif_tiempos = tiempos - tiempos_prev)
aux <- aux %>% mutate(potencia_prev = lag(potencia)) %>% slice(-1)
aux <- aux %>% mutate(potencia_media = (potencia_prev + potencia) / 2)
aux <- aux %>% mutate(Energia = potencia_media * dif_tiempos)

total <- aux %>% select(Energia) %>% slice(., 1:36315) %>% sum()
total * 0.0479691

names(aux) 
head(aux)
tail(aux)
nrow(aux)

