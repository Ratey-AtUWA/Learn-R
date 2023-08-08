library(ggmap)
testStamen <- get_stamenmap(bbox=c(left=115.89, bottom = -31.947,
                                   right = 115.99, top = -31.897),
                            zoom=13, # seems to be max zoom
                            maptype = "terrain", color = "color")
ggmap(testStamen) + 
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") + 
  geom_rect(aes(xmin=115.9401, xmax=115.9497, ymin=-31.922,ymax=-31.9155),
               colour="red3", fill=NA, size=1.5) +
  geom_text(aes(x = 115.939, y = -31.91875, label = "Ashfield\nFlats",
                fontface = "bold", family="sans"), 
            size = 6.5, vjust = 0.5, hjust = 1, color="red3", lineheight=0.8) +
  geom_text(aes(x = 115.926, y = -31.9325, label = "Swan River",
                fontface = "italic", family="sans"), angle=4,
            size = 4, vjust = 0, hjust = 0.5, color="steelblue4") +
  theme(axis.text=element_text(size=9, color="black"),
        axis.title=element_text(size=11,face="bold")) + 
  theme_bw()

testStamen2 <- get_stamenmap(bbox=c(left=115.8175, bottom = -31.9736,
                                   right = 115.8851, top = -31.9501),
                            zoom=15, # seems to be best zoom; zoom=17 works
                            maptype = "watercolor", # "watercolor" also works
                            color = "color") 
ggmap(testStamen2)

# register_google(key = "AIzaSyDU7QiTWE4RGFFQNmhWy51n7e4RBeHKjc0")

