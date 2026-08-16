p <- ggplot(results, aes(x = dev_value, y = p_cutoff, fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  theme_void() +
  theme(
    text = element_text(family="serif"),
    axis.title.x = element_text(color = "black", size = 20, margin = margin(t=-10, b=5)),
    axis.title.y = element_text(color = "black", size = 20, margin = margin(l=5, r=-15), angle = 90)
  ) +
  labs(x = "Expressiveness", y = "Selectivity")



## get colors for my example
library(sf)

country_outline <- map %>% st_union()

filtered <- results %>% filter(dev_value > 99.9)
table(filtered$color)

genmap <- function(bin=NA, int = NA, listcol=NA, color, border) {
  if(!identical(listcol, NA)) {
    ROI <- listcol
  } else if(!is.na(int)) {
    ROI <- task_id_to_binary(int)
  } else if(!is.na(bin)) {
    ROIint <- strtoi(bin,base=2)
    ROI <- task_id_to_binary(ROIint)
  } else {
    return("no data given")
  }
  map$color <- ifelse(ROI==1, color, "white")
  plotmap <- ggplot(data=map, aes(fill=color)) + 
    geom_sf(data=country_outline, fill=NA, color=border,linewidth=1.8, inherit.aes = FALSE) +
    geom_sf(color=rgb(0.2,0.2,0.2),linewidth=0.6) + 
    scale_fill_identity() +
    theme_void()
  return(plotmap)
}

basefrench <- "11111011000000000000000000"
fullfrench <- "10100011000000000000000000"
nongerman <-  "11111011000000000100000001" ->  #61D941

genf <- "00100000000000000000000000" # "#D94165" -> "#1A6B4E"

notveryg <-  "11101011000000000000000001" # "#41D9A2" -> "#6B1A35"




makepng <- function(filename, ROI, maincol, border) {
  map <- genmap (bin = ROI, color=maincol, border = border)
  ggsave(filename=filename,
         plot=map, bg="transparent", width=10, height=8, dpi=300)
}

table(results$color)

makepng("genf.png", ROI="00100000000000000000000000", maincol="#D94165", border="white")
makepng("notveryg.png", ROI="11101011000000000000000001", maincol="#41D9A2", border="black")
makepng("basefrench.png", ROI= "11111011000000000000000000", maincol="#4D41D9", border="white")
makepng("fullfrench.png", ROI= "10100011000000000000000000", maincol="#D96541", border="black")

makepng("nongerman.png", ROI = "11111011000000000100000001", maincol="#BA41D9", border="black")



  