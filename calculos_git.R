rm(list = ls()) # clean variables
graphics.off() # close graphics windows

library(readxl)
library(tidyverse)
library(dplyr)

# Carga de datos iniciales a partir de la interfaz de R
# Datos_iniciales <- read_excel("/Users/sergiogonzalez/Desktop/TFG Eléctrica/calculos_excel/Datos_iniciales.xlsx")
raw_data <- read_excel("/Users/andres/Google Drive/14_USAL/2018.19. Academic Info/03.TFG/Sergio Gonzalez Moreno/Informacion MotoStudent UR - Falces/Datos_iniciales.xlsx")

# Calculo de la velocidad de la rueda (rpm)
data <- raw_data %>% mutate(Vrueda = (1000 * Speed) / (60 * 2 * pi * 0.2964))

# Calculo del ratio
data <- data %>% mutate(Ratio = Eng / Vrueda)

# Calculo de la distancia instantanea (m)
data <- data %>% mutate(Distinst = (Speed / 3.6) * 0.5)

# Calculo de la distancia acumulada (m)
data <- data %>% mutate(Distacum = cumsum(Distinst))

# Calculo de la variacion de altura (m)
data <- data %>% mutate(Az = Distinst * Desnivel)

# Calculo de la altura (m)
data <- data %>% mutate(Z = 12.354 + cumsum(Az))

# Calculo de la altura escalada para su representacion grafica (m)
data <- data %>% mutate(Zescalada = Z * 4)


# Calculo de la aceleracion (m/s^2)
velocidad_aceleracion <- Datos_iniciales$Speed[-nrow(Datos_iniciales)] - Datos_iniciales$Speed[-1]

velocidad_aceleracion2 <- data_frame(velocidad_aceleracion)

velocidad_final <- data_frame(velocidad_aceleracion=c(2))

velocidad_aceleracion3 <- rbind(velocidad_aceleracion2,velocidad_final)

ai <- ((-velocidad_aceleracion3/3.6)/0.5)

data <- mutate(data, a = ai)

# Calculo de la fuerza que se produce (N)
data <- mutate(data, SumF = 148 * a)

# Calculo de la fuerza de desnivel (N)
data <- mutate(data, Fdesniv = -148 * (sin(atan(Desnivel))) * 9.8)

# Calculo del coeficiente de rodadura 
data <- mutate(data, CoefRod = 0.001 * (1 + (Speed/160)))

# Calculo de la fuerza de rodadura (N)
data <- mutate(data, FRod = -(1450.4 * CoefRod))

# Calculo del drag o resistencia aerodinámica (N)
data <- mutate(data, Fdrag = -0.5 * 0.43 * 0.5884 * 1.00 * ((Speed/3.6)^2))

# Calculo de la suma de fuerza motor mas fuerza de freno (N)
data <- mutate(data, FmotorFfreno = SumF - Fdesniv - FRod - Fdrag)

# Calculo de la suma del par motor mas el par de freno (Nm)
data <- mutate(data, TmotorTfreno = 0.2964 * FmotorFfreno)

# Calculo del par de freno (Nm)
data <- mutate(data, Tfreno = ifelse(TmotorTfreno<0, TmotorTfreno, 0))

# Calculo del par motor en la rueda (Nm)
data <- mutate(data, Tmotor = ifelse(TmotorTfreno>0, TmotorTfreno, 0))

# Calculo de la potencia en la rueda en kw (kw)
data <- mutate(data, Pruedakw = ((Tmotor*Vrueda*2*pi)/60)/1000)

# Calculo de la potencia en la rueda en cv (cv)
data <- mutate(data, Pruedacv = Pruedakw/0.735)

# Calculo de la potencia en el motor en kw (kw)
data <- mutate(data, Pmotorkw = Pruedakw/80.00)

# Calculo de la potencia en el motor en cv (cv)
data <- mutate(data, Pmotorcv = Pmotorkw/0.735)

# Calculo de la energia mecanica (wh)
data <- mutate(data, Emecanica = (Pmotorkw * 0.5)/3.6)

# Calculo de energia de combustible (wh)
data <- mutate(data, Ecombustible = Emecanica/20.00)

# Calculo de la potencia de frenado (kw)
data <- mutate(data, Pfrenado = ((Tfreno*Vrueda*2*pi)/60)/1000)

# Calculo de la energia de frenado (wh)
data <- mutate(data, Efrenado = (Pfrenado*0.5)/3.6)


