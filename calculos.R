library(readxl)
library(tidyverse)
library(dplyr)

# Carga de datos iniciales a partir de la interfaz de R

# Calculo de la velocidad de la rueda (rpm)
calculos <- mutate(Datos_iniciales, Vrueda = (1000*Speed)/(60*2*pi*0.2964))

# Calculo del ratio 
calculos <- mutate(calculos, Ratio = Eng/Vrueda)

# Calculo de la distancia instantanea (m)
calculos <- mutate(calculos, Distinst = (Speed/3.6)*0.5)

# Calculo de la distancia acumulada (m)
calculos <- mutate(calculos, Distacum = cumsum(Distinst))

# Calculo de la variacion de altura (m)
calculos <- mutate(calculos, Az = Distinst*Desnivel)

# Calculo de la altura (m)
calculos <- mutate(calculos, Z = 12.354+cumsum(Az))

# Calculo de la altura escalada para su representacion grafica (m)
calculos <- mutate(calculos, Zescalada = Z*4)

# Calculo de la aceleracion (m/s^2)
velocidad_aceleracion <- Datos_iniciales$Speed[-nrow(Datos_iniciales)] - Datos_iniciales$Speed[-1]
velocidad_aceleracion2 <- data_frame(velocidad_aceleracion)
velocidad_final <- data_frame(velocidad_aceleracion=c(2))
velocidad_aceleracion3 <- rbind(velocidad_aceleracion2,velocidad_final)
ai <- ((-velocidad_aceleracion3/3.6)/0.5)
calculos <- mutate(calculos, a=ai)

# Calculo de la fuerza que se produce (N)
calculos <- mutate(calculos, SumF = 148*a)

# Calculo de la fuerza de desnivel (N)
calculos <- mutate(calculos, Fdesniv = -148 * (sin(atan(Desnivel))) * 9.8)

# Calculo del coeficiente de rodadura 
calculos <- mutate(calculos, CoefRod = 0.001 * (1 + (Speed/160)))

# Calculo de la fuerza de rodadura (N)
calculos <- mutate(calculos, FRod = -(1450.4 * CoefRod))

# Calculo del drag o resistencia aerodinámica (N)
calculos <- mutate(calculos, Fdrag = -0.5 * 0.43 * 0.5884 * 1.00 * ((Speed/3.6)^2))

# Calculo de la suma de fuerza motor mas fuerza de freno (N)
calculos <- mutate(calculos, FmotorFfreno = SumF - Fdesniv - FRod - Fdrag)

# Calculo de la suma del par motor mas el par de freno (Nm)
calculos <- mutate(calculos, TmotorTfreno = 0.2964 * FmotorFfreno)

# Calculo del par de freno (Nm)
calculos <- mutate(calculos, Tfreno = ifelse(TmotorTfreno<0, TmotorTfreno, 0))
  
# Calculo del par motor en la rueda (Nm)
calculos <- mutate(calculos, Tmotor = ifelse(TmotorTfreno>0, TmotorTfreno, 0))

# Calculo de la potencia en la rueda en kw (kw)
calculos <- mutate(calculos, Pruedakw = ((Tmotor*Vrueda*2*pi)/60)/1000)

# Calculo de la potencia en la rueda en cv (cv)
calculos <- mutate(calculos, Pruedacv = Pruedakw/0.735)

# Calculo de la potencia en el motor en kw (kw)
calculos <- mutate(calculos, Pmotorkw = Pruedakw/80.00)

# Calculo de la potencia en el motor en cv (cv)
calculos <- mutate(calculos, Pmotorcv = Pmotorkw/0.735)

# Calculo de la energia mecanica (wh)
calculos <- mutate(calculos, Emecanica = (Pmotorkw * 0.5)/3.6)

# Calculo de energia de combustible (wh)
calculos <- mutate(calculos, Ecombustible = Emecanica/20.00)

# Calculo de la potencia de frenado (kw)
calculos <- mutate(calculos, Pfrenado = ((Tfreno*Vrueda*2*pi)/60)/1000)

# Calculo de la energia de frenado (wh)
calculos <- mutate(calculos, Efrenado = (Pfrenado*0.5)/3.6)
