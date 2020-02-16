# Paquetes -------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(readxl)
library(gganimate)

# Lectura y limpieza de datos ----- ------------------------------------------------

## data de hombres ---------------------------------------------
hombres <- read_excel(
  "data/proyeccion_poblacion_edad.xlsx",
  skip = 4,
  sheet = "Hombres ") %>%
  clean_names()

## limpieza y creación de data en grupos quinquenales
hombres <- 
  hombres %>%
  # Quitando las notas al final de la tabla
  slice(
    1:which(str_detect(grupo_de_edad, "Nota")) - 1
  ) %>%
  # eliminando información redundante
  filter(
    # quitar filas en blanco
    !is.na(grupo_de_edad),
    #quita los totales
    str_detect(grupo_de_edad, "Total", negate = TRUE)
    ) %>%
  # agrega la variable edad quinquenal
  mutate(
      edad_quinquenal = ifelse(str_detect(grupo_de_edad, "[-y]"), grupo_de_edad, NA)
    ) %>%
  # rellena la variable edad_quinquenal
  fill(edad_quinquenal) %>%
  # quita los totales por edad quinquenal
  filter(str_detect(grupo_de_edad, "[-]", negate = TRUE)) %>%
  # reorganiza los resultados
  select(edad = grupo_de_edad, edad_quinquenal, everything()) 

## data edades simples en formato tidy
hombres_long <- hombres %>%
  gather(year, poblacion, -edad, -edad_quinquenal) %>%
  mutate(
    year = parse_number(year),
    sexo = "Hombres")

# data de mujeres ----------------------------------------------
mujeres <- read_excel(
  "data/proyeccion_poblacion_edad.xlsx",
  skip = 4,
  sheet = "Mujeres ") %>%
  clean_names() 

## limpieza de data con edades simples 
mujeres <-
  mujeres %>%
  # Quitando las notas al final de la tabla
  slice(
    1:which(str_detect(grupo_de_edad, "Nota")) - 1
  ) %>%
  # eliminando información redundante
  filter(
    # quitar filas en blanco
    !is.na(grupo_de_edad),
    #quita los totales
    str_detect(grupo_de_edad, "Total", negate = TRUE)
    ) %>%
  # agrega la variable edad quinquenal
  mutate(
      edad_quinquenal = ifelse(str_detect(grupo_de_edad, "[-y]"), grupo_de_edad, NA)
    ) %>%
  # rellena la variable edad_quinquenal
  fill(edad_quinquenal) %>%
  # quita los totales por edad quinquenal
  filter(str_detect(grupo_de_edad, "[-]", negate = TRUE)) %>%
  # reorganiza los resultados
  select(edad = grupo_de_edad, edad_quinquenal, everything()) 

## data de edad en grupos quinquenales tidy
mujeres_long <- mujeres %>%
  gather(year, poblacion, -edad, -edad_quinquenal) %>%
  mutate(
    year = parse_number(year),
    sexo = "Mujeres")

## Data población total ----------------------------------------

## combinando la data de mujeres y de hombres
poblacion <-
  bind_rows(hombres_long, mujeres_long) %>%
    group_by(year) %>%
    mutate(
      total = sum(poblacion),
      porcentaje_total = poblacion/total*100,
      edad = parse_number(edad),
    edad_quinquenal = fct_inorder(edad_quinquenal)
    ) %>%
    ungroup()

## Guardando los archivos --------------------------------------
saveRDS(poblacion, "data/poblacion.RDS")

