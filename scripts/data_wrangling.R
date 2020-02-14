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
hombres_quinquenal <- hombres %>%
  # Quitando las notas al final de la tabla
  slice(
    1:which(str_detect(grupo_de_edad, "Nota")) - 1
  ) %>%
  # eliminando información redundante
  filter(
    # quitar filas en blanco
    !is.na(grupo_de_edad),
    # quita grupos quinquenales de edad y total
    str_detect(grupo_de_edad, "-"),
    str_detect(grupo_de_edad, "Total", negate = TRUE))

## limpieza y creacion de data en edades simples
hombres <- hombres %>%
  # Quitando las notas al final de la tabla
  slice(
    1:which(str_detect(grupo_de_edad, "Nota")) - 1
  ) %>%
  # eliminando información redundante
  filter(
    # quitar filas en blanco
    !is.na(grupo_de_edad),
    # quita grupos quinquenales de edad y total
    str_detect(grupo_de_edad, "-|Total", negate = TRUE))

## data con edad quinquenal en formato tidy
hombres_qlong <- hombres_quinquenal %>%
   gather(year, poblacion, -grupo_de_edad) %>%
  rename(edad = grupo_de_edad) %>%
  mutate(
    year = parse_number(year),
    sexo = "Hombres")

## data edades simples en formato tidy
hombres_long <- hombres %>%
  gather(year, poblacion, -grupo_de_edad) %>%
  rename(edad = grupo_de_edad) %>%
  mutate(
    year = parse_number(year),
    sexo = "Hombres")


# data de mujeres ----------------------------------------------
mujeres <- read_excel(
  "data/proyeccion_poblacion_edad.xlsx",
  skip = 4,
  sheet = "Mujeres ") %>%
  clean_names() 

## limpieza de data con edad en grupos quinquenales
mujeres_quinquenal <- mujeres %>%
  # Quitando las notas al final de la tabla
  slice(
    1:which(str_detect(grupo_de_edad, "Nota")) - 1
  ) %>%
  # eliminando información redundante
  filter(
    # quitar filas en blanco
    !is.na(grupo_de_edad),
    # quita grupos quinquenales de edad y total
    str_detect(grupo_de_edad, "-"),
    str_detect(grupo_de_edad, "Total", negate = TRUE))


## limpieza de data con edades simples 
mujeres <- mujeres %>%
  # Quitando las notas al final de la tabla
  slice(
    1:which(str_detect(grupo_de_edad, "Nota")) - 1
  ) %>%
  # eliminando información redundante
  filter(
    # quitar filas en blanco
    !is.na(grupo_de_edad),
    # quita grupos quinquenales de edad y total
    str_detect(grupo_de_edad, "-|Total", negate = TRUE))

## data de edad en grupos quinquenales tidy
mujeres_qlong <- mujeres_quinquenal %>%
  gather(year, poblacion, -grupo_de_edad) %>%
  rename(edad = grupo_de_edad) %>%
  mutate(
    year = parse_number(year),
    sexo = "Mujeres")

# data de edades simples en formato tidy
mujeres_long <- mujeres %>%
  gather(year, poblacion, -grupo_de_edad) %>%
  rename(edad = grupo_de_edad) %>%
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
      base = 0,
      edad = parse_number(edad)
    ) %>%
    ungroup()

## data con edades en grupos quinquenales
poblacion_qlong <- bind_rows(hombres_qlong, mujeres_qlong) %>%
  group_by(year) %>%
    mutate(
      total = sum(poblacion),
      porcentaje_total = poblacion/total*100,
      base = 0,
      edad = fct_inorder(edad)
    ) %>%
    ungroup()

## Guardando los archivos --------------------------------------

saveRDS()

