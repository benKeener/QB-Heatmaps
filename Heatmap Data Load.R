f19 <- readRDS(
  url(
    glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds")
  )
)

heatmap <- f19 %>%
  mutate(passer_player_name = ifelse(passer_player_name == "G.Minshew", "G.Minshew II", passer_player_name)) %>%
  filter(!is.na(passer_player_name)) %>%
  filter(!is.na(pass_location)) %>%
  group_by(passer_player_id, passer_player_name) %>%
  select(air_yards, pass_location) %>%
  mutate(pass_location = ifelse(pass_location == "left", -1, 
                                ifelse(pass_location == "middle", 0, 1)))