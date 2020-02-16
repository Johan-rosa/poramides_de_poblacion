p <- poblacion_piramide %>%
  filter(edad < 80) %>%
  ggplot(
    aes(
    x = edad, ymin = base,
    ymax = porcentaje_total,
    fill = sexo)
  ) + 
  geom_ribbon(alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c("midnightblue", "darkred")) +
  theme(legend.position = "bottom") +
  gganimate::transition_time(year) +
  labs(
    title = 'Distribución de la población por edad, año: {as.integer(frame_time)}',
    x = "Edad",
    y ='Porcentaje de la población total')

animate(p)

animate(p, nframes = 40, renderer = gifski_renderer("visualizaciones/piramide_animation.gif"))

# Paquetes necesarios para hacer animaciones 
install.packages("transformr")
install.packages("gifski")


poblacion_piramide %>%
  filter(year == 2020) %>% 
  ggplot(aes(x = edad, y = porcentaje_total, fill = sexo)) +
  geom_area() +
  coord_flip()
