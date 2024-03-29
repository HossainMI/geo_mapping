setwd("E:...")

# libraries

library(sf)

library(rayshader)

library(ggplot2)

library(ggspatial)

library(dplyr)

# Shape file import

shape = st_read("ABC.shp",
                stringsAsFactors = FALSE)

# Data file import

data = read.csv("ABC-Map.csv")

# Shape file editing

shape %>% 
  select(Division = ADM1_EN,
         geometry) -> edited_shape_df

data %>% 
  count(Division,
        wt = value,
        sort = T,
        name = "Confirmed") -> data_summary_df

shape_file_for_plotting = sf::st_as_sf(merge(data_summary_df,
                                              edited_shape_df,
                                              by = c("Division")))

shape_file_for_plotting$Confirmed1 = 
  cut(shape_file_for_plotting$Confirmed,
      breaks = c(0,0.314, 0.356, 0.365, 0.366,
                 0.376, 0.394, 0.404, 0.432, Inf),
      lables=c("Mymensingh", "Dhaka", "Sylhet",
               "Rajshahi", "Rangpur", "Khulna",
               "Chittagong", "Barisal"))

GM = ggplot(shape_file_for_plotting)+
  geom_sf(aes(fill = Confirmed1))+
  geom_sf_text(aes(label = dncd$Percentage),
               color = "black",
               size = 2.75,
               fontface = "bold",
               check_overlap = TRUE)

MM = gg1+
  scale_fill_manual(label = c("Mymensingh", "Dhaka", "Sylhet",
                              "Rajshahi", "Rangpur", "Khulna",
                              "Chittagong", "Barisal"),
                    values = c("#3BB9FF", "#00FF00", "#FFF380","#FFCBA4",
                               "#F88017","#F535AA","#FF0000","#6AFB92"),
                    name = "TITLE",
                    guide = guide_legend(
                      direction = "vertical",
                      nrow = 4, ncol = 2,
                      title.position = "top",
                      title.hjust = .5,
                      label.hjust = .5,
                      label.position = "right",
                      keyheight = 1))+
  labs(x= "Longitude",
       y= "Latitude")+
  theme(title = element_text(face = "bold"),
        legend.position = c(.97,.98),
        legend.justification = c("right","top"),
        legend.box.background = element_rect(colour = "black",
                                             size = 0.5),
        legend.title = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0.5),
        legend.box.margin = margin(1,1,1,1),
        plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.title = element_text(size = 10,
                                  face = "bold.italic"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "#E5E4E2"),
        panel.border = element_rect(fill = NA,
                                    "black",
                                    size = 0.8),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))
MM

ggsave("MAP.png",
       units = "in",
       width = 5.5,
       height = 7,
       dpi = 320)
