



ggplot(data = eksport, aes(
  x = eksport$uke, 
  y = eksport$`Eksport av oppalen laks, etter varegruppe, uke og statistikkvariabel`)
  ) + 
  geom_col() +
  facet_grid(statistikkvariabel ~ varegruppe)
