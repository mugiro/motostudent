rm(list = ls()) # clean variables
graphics.off() # close graphics windows

library(readxl)
# library(tidyverse)  
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
data <- data %>% mutate(Speed_pre = lag(Speed)) %>% slice(-1)
data <- data %>% mutate(Aceleracion = (((Speed - Speed_pre) / 3.6) / 0.5))

# Calculo de la fuerza que se produce (N)
data <- data %>% mutate(Force = 148 * Aceleracion)

# Calculo de la fuerza de desnivel (N)
data <- data %>% mutate(Force_desnivel = -148 * (sin(atan(Desnivel))) * 9.8)

# Calculo del coeficiente de rodadura y la fuerza de rodadura (N)
data <- data %>% mutate(Coef_Rod = 0.001 * (1 + (Speed / 160)))
data <- data %>% mutate(Force_Rod = - (1450.4 * Coef_Rod))

# Calculo del drag o resistencia aerodinámica (N)
data <- data %>% mutate(Force_drag = -0.5 * 0.43 * 0.5884 * 1.00 * ((Speed / 3.6)^2))

# Calculo de la suma de fuerza motor mas fuerza de freno (N)
data <- data %>% mutate(Force_motor_freno = Force - Force_desnivel - Force_Rod - Force_drag)

# Calculo de la suma del par motor mas el par de freno (Nm)
data <- data %>% mutate(Torque_motor_freno = 0.2964 * Force_motor_freno)

# Calculo del par de freno (Nm)
data <- data %>% mutate(Torque_freno = ifelse(Torque_motor_freno < 0, Torque_motor_freno, 0),
                        Torque_motor = ifelse(Torque_motor_freno > 0, Torque_motor_freno, 0))

# Calculo de la potencia en la rueda en kw (kw)
data <- data %>% mutate(Power_rueda_kw = ((Torque_motor * Vrueda * 2 * pi) / 60) / 1000)

# Calculo de la potencia en la rueda en cv (cv) y potencia en el motor en kw (kw)
data <- data %>% mutate(Power_rueda_cv = Power_rueda_kw / 0.735,
                        Power_motor_kw = Power_rueda_kw / 80.00)

# Calculo de la potencia en el motor en cv (cv)
data <- data %>% mutate(Power_motor_cv = Power_motor_kw / 0.735,
                        E_mecanica = (Power_motor_kw * 0.5) / 3.6)

# Calculo de energia de combustible (Wh)
data <- data %>% mutate(E_combustible = E_mecanica / 20.00)

# Calculo de la potencia de frenado (kW) y la energia de frenado (Wh)
data <- data %>% mutate(Power_frenado = ((Torque_freno * Vrueda * 2 * pi) / 60) / 1000)
data <- data %>% mutate(E_frenado = (Power_frenado * 0.5) / 3.6)

View(data)
