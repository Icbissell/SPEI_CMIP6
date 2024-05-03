library(ggmap)
library(maps)
library(ggplot2)
library(usmap)


register_google(key = "your_key")

# Define bounding box coordinates
lon <- c(-75.25, -69.75)
lat <- c(40, 43.25)

# Get a map
map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 6,
               maptype = "satellite", source = "google")

a <- ggmap(map) +
  geom_rect(xmin = min(lon), ymin = min(lat),
            xmax = max(lon), ymax = max(lat),
            fill = NA, colour = "red", linewidth = 0.6, lty = 1) +
  annotation_map(
    map_data("state"),
    fill = NA, colour = "darkgrey", linewidth = 0.2
  ) +
  labs(x = "Latitude", y = "Longitude") +
  ylim(c(38, 45))

a

ggsave("plots/Figure1.pdf", plot = a, width = 8, height = 6)
ggsave("plots/Figure1.png", plot = a, width = 8, height = 6)

