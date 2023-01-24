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

BES_theme_draft <- function() {
  theme(
    legend.background = element_rect(fill = "#7B9FBD"),
    legend.key = element_rect(fill = "#7B9FBD"),
    legend.text = element_text(family = "Avenir", colour = "#FFFFFF", size = 12),
    legend.title = element_text(family = "Avenir", colour = "#FFFFFF", size = 12),
    plot.background = element_rect(fill = "#7B9FBD", colour = NA),
    panel.background = element_rect(fill = "#7B9FBD",colour = NA),
    axis.text = element_text(colour = "#FFFFFF", family = "Avenir", face = "bold",
                             size = 12),
    plot.title = element_text(colour = "#FFFFFF", face = "bold", size = 15, 
                              vjust = 1, hjust = 0.5, family = "Avenir"),
    axis.title = element_text(colour = "#FFFFFF", face = "bold", size = 15, 
                              family = "Avenir"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Avenir", colour = "#FEFF54"),
    strip.background = element_rect(fill = "#7B9FBD"),
    axis.ticks = element_line(colour = "white")
  )
}

BES_theme <- function() {
  theme(
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    legend.text = element_text(family = "Avenir", colour = "#FFFFFF", 
                               size = 15),
    legend.title = element_text(family = "Avenir", colour = "#FFFFFF", 
                                size = 15),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.text = element_text(colour = "#FFFFFF", family = "Avenir", face = "bold",
                             size = 15),
    plot.title = element_text(colour = "#FFFFFF", face = "bold", size = 20, 
                              vjust = 1, hjust = 0.5, family = "Avenir"),
    axis.title = element_text(colour = "#FFFFFF", face = "bold", size = 20, 
                              family = "Avenir"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Avenir", colour = "#FEFF54"),
    strip.background = element_rect(fill = "transparent"),
    axis.ticks = element_line(colour = "white")
  )
}


