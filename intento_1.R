library(readxl)
library(tidyverse)
library(dplyr)

# Carga de datos de 2018 
Datos_iniciales <- read_csv("/Users/sergiogonzalez/Desktop/To_csv_log_motostudent_2018_mod.csv")

# Filtro del caso 500
Caso_500 <- Datos_iniciales %>%
  dplyr::filter(x1 == 500, x2 == 500, x3 == 500, x4 == 500, x5 == 500, x6 == 500, x7 == 500 )

# Filtro del caso 501
Caso_501 <- Datos_iniciales %>%
  dplyr::filter(x1 == 501, x2 == 501, x3 == 501, x4 == 501, x5 == 501, x6 == 501, x7 == 501 )

# Filtro del caso 502
Caso_502 <- Datos_iniciales %>%
  dplyr::filter(x1 == 502, x2 == 502, x3 == 502, x4 == 502, x5 == 502, x6 == 502, x7 == 502 )

# Filtro del caso 503
Caso_503 <- Datos_iniciales %>%
  dplyr::filter(x1 == 503, x2 == 503, x3 == 503, x4 == 503, x5 == 503, x6 == 503, x7 == 503 )

# Filtro del caso 504
Caso_504 <- Datos_iniciales %>%
  dplyr::filter(x1 == 504, x2 == 504, x3 == 504, x4 == 504, x5 == 504, x6 == 504, x7 == 504 )

# Filtro del caso 505
Caso_505 <- Datos_iniciales %>%
  dplyr::filter(x1 == 505, x2 == 505, x3 == 505, x4 == 505, x5 == 505, x6 == 505, x7 == 505 )


