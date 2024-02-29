ggmap(afr.gg) + 
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") + 
  geom_text(aes(x = 115.944, y = -31.918, label = "Ashfield\nFlats",
                fontface = "italic", family="sans"), 
            size = 4, color="gray65", lineheight=0.8) +
  geom_text(aes(x = 115.942, y = -31.9199, label = "Swan River",
                fontface="italic", family="sans"), size=4, color="gray65") +
  geom_path(aes(x = drain_lon, y=drain_lat), data=afr_map, 
            color = "slategray2", size = 1.25) +
  geom_polygon(aes(x=wetland_lon, y=wetland_lat), data = afr_map,
               color = "slategray", fill="azure3") + 
  geom_sf(data = afs21ll, aes(bg=Zn, size=Zn), shape=21, inherit.aes = FALSE) +
  scale_x_continuous(label = abs) +
  scale_y_continuous(label = abs) +
  scale_fill_viridis_c(alpha = 0.7) + 
  scale_size_area(breaks = c(30,100,300,1000,3000,5000), max_size = 16) +
  theme(axis.text=element_text(size=9, color="black"),
        axis.title = element_text(size = 12, face = "bold"))

#```{r map-code-hidden, echo=FALSE, fig.align='center', fig.height=4.2, fig.width=9, message=FALSE, warning=FALSE, out.width='80%', results='hold'}
# make Rottnest rotto map

rotto_ext <- st_as_sf(data.frame(x=c(115.44, 115.561),y=c(-32.03,-31.985)),
                      coords = c("x","y"), crs=st_crs(4326))
require(maptiles)
rotto <- get_tiles(rotto_ext, provider = "Thunderforest.Outdoors",
                   zoom=14, apikey = secret[1,1], crop=TRUE)
plot_tiles(rotto)
text(115.45,-31.995,pos=4, cex=1.5, col="#004000", font=3,
     labels="Thunderforest ‘Outdoors’ map\nof Rottnest Island")
