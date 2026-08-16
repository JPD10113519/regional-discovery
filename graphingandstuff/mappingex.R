library(BFS)
map <- bfs_get_base_maps(geom="kant")
## fix map ordering
canton_order <- c("Vaud", "Valais", "Genève", "Bern", "Fribourg", "Solothurn", 
                  "Neuchâtel", "Jura", "Basel-Stadt", "Basel-Landschaft", "Aargau", 
                  "Zürich", "Glarus", "Schaffhausen", "Appenzell Ausserrhoden", 
                  "Appenzell Innerrhoden", "St. Gallen", "Graubünden", "Thurgau", 
                  "Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden", "Zug", "Tessin")

map <- map[match(canton_order, map$name), ]

source("code_v2/model_functions.R")
source("code_v2/PREDSload.R")

genmap <- function(bin=NA, int = NA, listcol=NA) {
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
  map$color <- rgb(r = (26 + 188*ROI), g = (95 - 27*ROI), b = (168-46*ROI), maxColorValue=255)
  plotmap <- ggplot(data=map, aes(fill=color)) + geom_sf(color=rgb(0.2,0.2,0.2),linewidth=0.6) + scale_fill_identity() +
    theme_void()
  print(plotmap)
}

set.seed(234234)
int <- runif(1, min=0, max = 2^26-1)
genmap(int = int)

ROI <- task_id_to_binary(int)

target_dev <- 95
p_cutoff <- 0.51

test <- run_iteration(target_dev = target_dev, p_cutoff = p_cutoff, ROI = ROI)

genmap(listcol=test$final_roi)

step1 <- run_iteration(target_dev = target_dev, p_cutoff = p_cutoff, ROI = ROI, max_iterations = 1)

featuresdf1 <- data.frame(
  PctCatholic = PREDS[,1],
  PctGerman = PREDS[,2],
  colorsinit = rgb(r = (26 + 188*step1$init_roi), g = (95 - 27*step1$init_roi), b = (168-46*step1$init_roi), maxColorValue=255),
  colorsfinal =rgb(r = (26 + 188*step1$final_roi), g = (95 - 27*step1$final_roi), b = (168-46*step1$final_roi), maxColorValue=255),
  size <- step1$laststep_preds
)

plot1 <- ggplot(data=featuresdf1, aes(x=PctCatholic, y=PctGerman, color=colorsinit)) + 
  geom_point()
plot2 <- ggplot(data=featuresdf1, aes(x=PctCatholic, y=PctGerman, color=colorsfinal, size=s1^2)) + 
  geom_point()


step2 <- run_iteration(target_dev = target_dev, p_cutoff = p_cutoff, ROI = step1$final_roi, max_iterations = 1)

featuresdf2 <- data.frame(
  PctCatholic = PREDS[,1],
  PctGerman = PREDS[,2],
  colorsinit = rgb(r = (26 + 188*step2$init_roi), g = (95 - 27*step2$init_roi), b = (168-46*step2$init_roi), maxColorValue=255),
  colorsfinal =rgb(r = (26 + 188*step2$final_roi), g = (95 - 27*step2$final_roi), b = (168-46*step2$final_roi), maxColorValue=255),
  size <- step2$laststep_preds
)

plot3 <- ggplot(data=featuresdf2, aes(x=PctCatholic, y=PctGerman, color=colorsinit, size=s1^2)) + 
  geom_point()
plot4 <- ggplot(data=featuresdf2, aes(x=PctCatholic, y=PctGerman, color=colorsfinal, size=s1^2)) + 
  geom_point()

plot1
plot2
plot3
plot4


##### NEW EXAMPLE -- FOR SLIDE

### animation

library(av, lib.loc="~/R/library")
library(gganimate, lib.loc="~/R/library")

# Interpolate from uniform size to actual s1 values across N states
n_states <- 20  # more states = smoother animation

df_anim <- lapply(seq(0, 1, length.out = n_states), function(t) {
  featuresdf1 %>%
    mutate(size_anim = mean(s1) + t * (s1 - mean(s1)),
           t = t)
}) %>%
  bind_rows()

plot_anim <- ggplot(df_anim, aes(x = PctCatholic, y = PctGerman,
                                 color = colorsinit, size = size_anim)) +
  geom_point() +
  transition_time(t) +
  ease_aes('cubic-in-out') +
  guides(size = "none")  # hide the size legend if you want


step1 <- ggplot(data=featuresdf1, aes(x=PctCatholic, y=PctGerman, color=colorsinit)) + 
  scale_color_identity() +
  geom_point(size=3.5) +
  theme_minimal() + theme(legend.position="none")

step2 <- ggplot(data=featuresdf1, aes(x=PctCatholic, y=PctGerman, color=colorsinit, size=s1^3)) + 
  scale_color_identity() +
  scale_size_continuous(range = c(1, 8)) +
  geom_point() + theme_minimal() + theme(legend.position="none")

step3 <- ggplot(data=featuresdf1, aes(x=PctCatholic, y=PctGerman, color=colorsfinal, size=s1^3)) + 
  scale_color_identity() +
  scale_size_continuous(range = c(1, 8)) +
  geom_point() + theme_minimal() + theme(legend.position="none")


map1 <- genmap(listcol=step1$init_roi)
map2 <- genmap(listcol=step1$final_roi)

plots <- list(step1 = step1, step2 = step2, step3 = step3, map1 = map1, map2 = map2)

for (name in names(plots)) {
  ggsave(paste0(name, ".png"), plot = plots[[name]], width = 8, height = 6)
}

step2 <- run_iteration(target_dev = target_dev, p_cutoff = p_cutoff, ROI = step1$final_roi, max_iterations = 1)

featuresdf2 <- data.frame(
  PctCatholic = PREDS[,1],
  PctGerman = PREDS[,2],
  colorsinit = rgb(r = (26 + 188*step2$init_roi), g = (95 - 27*step2$init_roi), b = (168-46*step2$init_roi), maxColorValue=255),
  colorsfinal =rgb(r = (26 + 188*step2$final_roi), g = (95 - 27*step2$final_roi), b = (168-46*step2$final_roi), maxColorValue=255),
  size <- step2$laststep_preds
)

step1 <- ggplot(data=featuresdf2, aes(x=PctCatholic, y=PctGerman, color=colorsinit)) + 
  scale_color_identity() +
  geom_point(size=3.5) +
  theme_minimal() + theme(legend.position="none")

step2 <- ggplot(data=featuresdf2, aes(x=PctCatholic, y=PctGerman, color=colorsinit, size=s1^3)) + 
  scale_color_identity() +
  scale_size_continuous(range = c(1, 8)) +
  geom_point() + theme_minimal() + theme(legend.position="none")

step3 <- ggplot(data=featuresdf2, aes(x=PctCatholic, y=PctGerman, color=colorsfinal, size=s1^3)) + 
  scale_color_identity() +
  scale_size_continuous(range = c(1, 8)) +
  geom_point() + theme_minimal() + theme(legend.position="none")



