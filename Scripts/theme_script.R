# theme for the ggplots - controls colours and fonts of titles and axes labels

plain_theme <- function() {
  theme(
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    legend.text = element_text(family = "Avenir", colour = "black", size = 7),
    legend.title = element_text(family = "Avenir", colour = "black", size = 7),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.text = element_text(colour = "black", family = "Avenir", face = "bold"),
    plot.title = element_text(colour = "black", face = "bold", size = 10, 
                              vjust = 1, hjust = 0.5, family = "Avenir"),
    axis.title = element_text(colour = "black", face = "bold", size = 10, 
                              family = "Avenir"),
    panel.grid.major.y = element_line(colour = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Avenir", colour = "black"),
    strip.background = element_rect(fill = "grey"),
    axis.ticks = element_line(colour = "white")
  )
}

