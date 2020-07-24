heatmap <- heatmap %>%
  mutate(air_yards = ifelse(air_yards > 25, 25, air_yards)) %>%
  mutate(air_yards = ifelse(air_yards < -5, -5, air_yards))

qbs <- f19 %>%
  filter(!is.na(passer_player_name)) %>%
  filter(play_type == "pass") %>%
  group_by(passer_player_name) %>%
  summarise(passes = n()) %>%
  filter(passes > 200)  %>%
  select(passer_player_name)

qbs <- unique(qbs$passer_player_name)

plot_vec <- list()

for(i in seq_along(qbs)){
  plot <- ggplot(subset(heatmap, heatmap$passer_player_name==qbs[i]), aes(x = pass_location, y = air_yards)) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_fill_distiller(palette= "Spectral", direction=-1, na.value = "white") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(
      legend.position='none',
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y =element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) + labs(title = qbs[i])
  
  plot_vec[[i]] = plot
}