## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE
)

## ----setup, echo=FALSE--------------------------------------------------------
library(sugarglider)
library(knitr)
library(ggplot2)
library(sf)
library(tidyverse)
library(grid)
library(viridis)
library(gridExtra)
library(ozmaps)
library(ggthemes)

## -----------------------------------------------------------------------------
vic_temp <- aus_temp |>
  filter(id %in% c("ASN00026021", "ASN00085291", "ASN00084143"))

p1 <- vic_temp |>
   ggplot(aes(x_major = long, y_major = lat,
              x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "Victoria"),
         color = grey(0.7), fill = NA, inherit.aes = FALSE)  +
  # Customize the size of each glyph box using the width and height parameters.
  add_glyph_boxes(width = rel(2.5), height = rel(1.5)) +
  add_ref_lines(width = rel(2.5), height = rel(1.5)) +
  geom_glyph_ribbon(width = rel(2.5), height = rel(1.5)) +
  theme_map() +
  labs(title = "geom_glyph_ribbon()") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

p2 <- vic_temp |>
   ggplot(aes(x_major = long, y_major = lat,
              x_minor = month, y_minor = tmin, yend_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "Victoria"),
          color = grey(0.7), fill = NA, inherit.aes = FALSE)  +
  # Customize the size of each glyph box using the width and height parameters.
   add_glyph_boxes(width = rel(2.5), height = rel(1.5)) +
   add_ref_lines(width = rel(2.5), height = rel(1.5)) +
  geom_glyph_segment(width = rel(2.5), height = rel(1.5)) +
  theme_map() +
  labs(title = "geom_glyph_segment()") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

grid.arrange(p1, p2, ncol = 2) 

## ----eval=FALSE---------------------------------------------------------------
#  # Download the development version from GitHub:
#  devtools::install_github("maliny12/ribbon")

## -----------------------------------------------------------------------------
aus_temp

## -----------------------------------------------------------------------------
aus_temp |>
  ggplot(aes(
    x_major = long, 
    y_major = lat, 
    x_minor = month, 
    y_minor = tmin, 
    yend_minor = tmax)) +
  geom_sf(data = abs_ste, color = grey(0.7),
          fill = NA, inherit.aes = FALSE) +
  coord_sf(xlim = c(110,155)) +
  # Add glyph box to each glyph
  add_glyph_boxes(
    width = 4,
    height = 3) +
  geom_point(aes(x = long, y = lat,
                 color = "Weather Station")) +
  geom_glyph_segment(
    # Customize the size of each glyph box using the width and height parameters.
    width = 4, 
    height = 3,
    aes(color = "Temperature")) +
  ggthemes::theme_map() +
  scale_color_manual(
    values = c("Weather Station" = "firebrick",
               "Temperature" = "black")) +
  labs(color = "Data")  +
  theme(legend.position.inside = c(.2,0), 
        legend.direction = "horizontal")


## -----------------------------------------------------------------------------
# Global rescale
p1 <- aus_temp |>
  ggplot(aes(
    x_major = long, 
    y_major = lat, 
    x_minor = month, 
    y_minor = tmin, 
    yend_minor = tmax)) +
  geom_sf(data = abs_ste, color = grey(0.7),
          fill = NA, inherit.aes = FALSE) +
  coord_sf(xlim = c(110,155)) +
  # Add glyph box to each glyph
  add_glyph_boxes() +
  # Add reference lines to each glyph
  add_ref_lines() +
  geom_glyph_segment(global_rescale = TRUE) +
  ggthemes::theme_map() +
  labs(title = "Global Rescale") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
  
# Local Rescale
p2 <- aus_temp |>
  ggplot(aes(
    x_major = long, 
    y_major = lat, 
    x_minor = month, 
    y_minor = tmin, 
    yend_minor = tmax)) +
  geom_sf(data = abs_ste, color = grey(0.7),
          fill = NA, inherit.aes = FALSE) +
  coord_sf(xlim = c(110,155)) +
  # Add glyph box to each glyph
  add_glyph_boxes() +
  # Add reference lines to each glyph
  add_ref_lines() +
  geom_glyph_segment(global_rescale = FALSE) +
  ggthemes::theme_map() +
  labs(title = "Local Rescale") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

grid.arrange(p1, p2, ncol = 2) 

## -----------------------------------------------------------------------------

prcp <- aus_temp |>
   group_by(id) |>
   mutate(prcp = mean(prcp, na.rm = TRUE)) |>
   ggplot(aes(x_major = long, y_major = lat, x_minor = month,
              ymin_minor = tmin, ymax_minor = tmax,
              fill = prcp, color = prcp)) +
  geom_sf(data = abs_ste, color = grey(0.7), fill = NA,
          inherit.aes = FALSE)  +
   add_glyph_boxes() +
   add_ref_lines() +
   geom_glyph_ribbon() +
   coord_sf(xlim = c(112,155)) +
  theme_map() +
  theme(legend.position.inside = c(.2,0), 
        legend.direction = "horizontal",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_fill_gradientn(colors = c("#ADD8E6", "#2b5e82", "dodgerblue4")) +
  scale_color_gradientn(colors = c( "#ADD8E6", "#2b5e82", "dodgerblue4")) +
  labs(fill = "Percepitation", color = "Percepitation")
  

grid.arrange(prcp, ncol = 1) 

## -----------------------------------------------------------------------------

fact <- historical_temp |> 
  filter(id %in% c("ASN00026021", "ASN00085291", "ASN00084143")) |>
   ggplot(aes(color = factor(year), fill = factor(year),
              group = interaction(year,id),
              x_major = long, y_major = lat,
              x_minor = month, ymin_minor = tmin,
              ymax_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "Victoria"),
           fill = NA, color = grey(0.7),
          inherit.aes = FALSE)  +
   add_glyph_boxes(width = rel(2),
                   height = rel(1.5)) +
   add_ref_lines(width = rel(2),
                 height = rel(1.5)) +
   geom_glyph_ribbon(alpha = 0.5,
                     width = rel(2),
                     height = rel(1.5)) +
  labs(x = "Longitude", y = "Latitude",
       color = "year", fill = "year") +
  theme_map() +
  theme(legend.position.inside = c(.3,0.9), 
        legend.direction = "horizontal",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_colour_wsj("colors6") +
  scale_fill_wsj("colors6") 


grid.arrange(fact, ncol = 1) 

## -----------------------------------------------------------------------------
legend <- aus_temp |>
   ggplot(aes(x_major = long, y_major = lat,
              x_minor = month, ymin_minor = tmin,
              ymax_minor = tmax)) +
  geom_sf(data = abs_ste ,
           fill = NA, color = grey(0.7),
           inherit.aes = FALSE)  +
  add_glyph_boxes() +
  add_ref_lines() +
  add_glyph_legend() + 
  geom_glyph_ribbon() + 
  theme(legend.position.inside = c(.3,0.9), 
        legend.direction = "horizontal",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
  theme_map() 


grid.arrange(legend, ncol = 1) 

