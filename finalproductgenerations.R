## code imported from mappingex.R


run1 <- run_iteration(target_dev = target_dev, p_cutoff = p_cutoff, ROI = ROI, max_iterations = 1)

featuresdf1 <- data.frame(
  PctCatholic = PREDS[,1],
  PctGerman = PREDS[,2],
  colorsinit = rgb(r = (26 + 188*run1$init_roi), g = (95 - 27*run1$init_roi), b = (168-46*run1$init_roi), maxColorValue=255),
  colorsfinal =rgb(r = (26 + 188*run1$final_roi), g = (95 - 27*run1$final_roi), b = (168-46*run1$final_roi), maxColorValue=255),
  size <- run1$laststep_preds
)

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


map1 <- genmap(listcol=run1$init_roi)
map2 <- genmap(listcol=run1$final_roi)

plots <- list(step1 = step1, step2 = step2, step3 = step3, map1 = map1, map2 = map2)

for (name in names(plots)) {
  ggsave(paste0(name, ".png"), plot = plots[[name]], width = 8, height = 6)
}

run2 <- run_iteration(target_dev = target_dev, p_cutoff = p_cutoff, ROI = run1$final_roi, max_iterations = 1)

featuresdf2 <- data.frame(
  PctCatholic = PREDS[,1],
  PctGerman = PREDS[,2],
  colorsinit = rgb(r = (26 + 188*run2$init_roi), g = (95 - 27*run2$init_roi), b = (168-46*run2$init_roi), maxColorValue=255),
  colorsfinal =rgb(r = (26 + 188*run2$final_roi), g = (95 - 27*run2$final_roi), b = (168-46*run2$final_roi), maxColorValue=255),
  size <- run2$laststep_preds
)

step4 <- ggplot(data=featuresdf2, aes(x=PctCatholic, y=PctGerman, color=colorsinit)) + 
  scale_color_identity() +
  geom_point(size=3.5) +
  theme_minimal() + theme(legend.position="none")

step5 <- ggplot(data=featuresdf2, aes(x=PctCatholic, y=PctGerman, color=colorsinit, size=s1^3)) + 
  scale_color_identity() +
  scale_size_continuous(range = c(1, 8)) +
  geom_point() + theme_minimal() + theme(legend.position="none")

step6 <- ggplot(data=featuresdf2, aes(x=PctCatholic, y=PctGerman, color=colorsfinal, size=s1^3)) + 
  scale_color_identity() +
  scale_size_continuous(range = c(1, 8)) +
  geom_point() + theme_minimal() + theme(legend.position="none")

map3 <- genmap(listcol=run2$init_roi)
map4 <- genmap(listcol=run2$final_roi)

map3
map4


plots <- list(step4=step4, step5=step5)

for (name in names(plots)) {
  ggsave(paste0(name, ".png"), plot = plots[[name]], width = 8, height = 6)
}





########## next steps: french iterations

basefrench <- "11111011000000000000000000"
fullfrench <- "10100011000000000000000000"
nongerman <-  "11111011000000000100000001"

basefrench <- genmap(basefrench)
fullfrench <- genmap(fullfrench)
nongerman <- genmap(nongerman)

plots <- list(basefrench=basefrench, fullfrench=fullfrench, nongerman=nongerman)

for(name in names(plots)) {
  ggsave(paste0(name, "_contrastplot.png"), plot = plots[[name]], width=8, height=6)
}

basefrench <- "11111011000000000000000000"
fullfrench <- "10100011000000000000000000"
nongerman <-  "11111011000000000100000001"

lcbf <- as.integer(unlist(strsplit(basefrench, split=NULL)))
colorsbf <- rgb(r = (26 + 188*lcbf), g = (95 - 27*lcbf), b = (168-46*lcbf), maxColorValue=255)

lcff <- as.integer(unlist(strsplit(fullfrench, split=NULL)))
colorsff <- rgb(r = (26 + 188*lcff), g = (95 - 27*lcff), b = (168-46*lcff), maxColorValue=255)

lcng <- as.integer(unlist(strsplit(nongerman, split=NULL)))
colorsng <- rgb(r = (26 + 188*lcng), g = (95 - 27*lcng), b = (168-46*lcng), maxColorValue=255)

run <- run_iteration(target_dev = 95, p_cutoff = p_cutoff, ROI = lcbf, max_iterations = 1)

featdfgen <- data.frame(
  PctCatholic = PREDS[,1],
  PctGerman = PREDS[,2],
  size <- run$laststep_preds
)

featsbf <- ggplot(data=featdfgen, aes(x=PctCatholic, y=PctGerman, color=colorsbf, size=s1)) + 
  scale_color_identity() +
  geom_point() +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() + theme(legend.position="none")

featsff <- ggplot(data=featdfgen, aes(x=PctCatholic, y=PctGerman, color=colorsff, size=s1)) + 
  scale_color_identity() +
  geom_point() +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() + theme(legend.position="none")

featsng <- ggplot(data=featdfgen, aes(x=PctCatholic, y=PctGerman, color=colorsng, size=s1)) + 
  scale_color_identity() +
  geom_point() +
  scale_size_continuous(range = c(1, 8)) +
  theme_minimal() + theme(legend.position="none")

plots <- list(featsbf = featsbf, featsff = featsff, featsng = featsng)


for(name in names(plots)) {
  ggsave(paste0(name, ".png"), plot = plots[[name]], width=8, height=6)
}


