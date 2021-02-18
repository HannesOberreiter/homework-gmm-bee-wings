# Constants ---------------------------------------------------------------
sampleNames <- c("Graz 2011", "Graz 2009", "Wien 2009", "Frohnleiten 2009")

# Read Data ---------------------------------------------------------------
# TPS from tpsDig
fwRaw <- readland.tps("./data/ForeWing.TPS", specID = "imageID")
bwRaw <- readland.tps("./data/BackWing.TPS", specID = "imageID")
# Our Links
fwLink <- as.matrix(read.table("./data/ForeWingLink.NTS", sep = " ", skip = 2))
bwLink <- as.matrix(read.table("./data/BackWingLink.NTS", sep = " ", skip = 2))
# check or set links
#define.links(bwRaw[,,1], links = bwLink)
#define.links(fwRaw[,,1], links = fwLink)

# Procrustes calculation -----------------------------------------------------
fwSlider = rbind(
  define.sliders(c(1, 20:23)),
  define.sliders(c(23:36, 12))
)
fwProc <- gpagen(fwRaw, curves = fwSlider)
bwSlider = rbind(
  define.sliders(c(1, 9:11, 8)),
  define.sliders(c(8, 12:23, 4))
)
bwProc <- gpagen(bwRaw, curves = bwSlider)
rm(bwSlider, fwSlider, bwRaw, fwRaw)
# Generate Tibble ---------------------------------------------------------
# simply dont want to work with matrices
#fwDF <- geomorph.data.frame(fwProc)
#dim(fwProc$coords)
wTibble <- bind_rows(
  tibble(
    id = rep(colnames(fwProc$coords[,1,]), each = length(rownames(fwProc$coords[,1,]))),
    landmark = rep(rownames(fwProc$coords[,1,]),length(colnames(fwProc$coords[,1,]))),
    x = c(fwProc$coords[,1,]),
    y = c(fwProc$coords[,2,]),
    csize = rep(fwProc$Csize,each = length(rownames(fwProc$coords[,1,]))),
    wing = "ForeWing"
  ),
  tibble(
    id = rep(colnames(bwProc$coords[,1,]), each = length(rownames(bwProc$coords[,1,]))),
    landmark = rep(rownames(bwProc$coords[,1,]),length(colnames(bwProc$coords[,1,]))),
    x = c(bwProc$coords[,1,]),
    y = c(bwProc$coords[,2,]),
    csize = rep(bwProc$Csize,each = length(rownames(bwProc$coords[,1,]))),
    wing = "BackWing"
  )
)

# add sample names to our ids
wTibble <- wTibble %>% 
  mutate(
    sample = case_when(
      str_detect(id, "eb") ~ sampleNames[1],
      str_detect(id, "g") ~ sampleNames[2],
      str_detect(id, "w") ~ sampleNames[3],
      str_detect(id, "f") ~ sampleNames[4]
    ),
    sample = fct_relevel(sample, sampleNames),
    wing = fct_relevel(wing, c("ForeWing", "BackWing"))
  )

# Centroid Size Plots -----------------------------------------------------
wCSsummary <- wTibble %>% 
  filter(landmark == 1) %>% # we only need one entry for each specimen
  group_by(sample, wing) %>% 
  summarise(
    mean = format(round(mean(csize),1), nsmall = 1),
    median = format(round(median(csize),1), nsmall = 1),
    sd = format(round(sd(csize),2), nsmall = 2),
    n = n()
  )

csPlot <- ggplot(wTibble %>% filter(landmark == 1), aes(x = sample, y = csize, color = sample)) +
  geom_boxplot() +
  geom_point() + 
  geom_text(
    data = wCSsummary, 
    aes(
      x = sample, 
      y = ifelse(wing == "ForeWing", 13.5, 8.5), 
      label = paste0(
        "Mean = ", mean, "\n", "SD = ", sd, "\n", "Median = ", median, "\n", "n = ", n
        )
      ),
    hjust = 0,
    nudge_x = -0.4,
    vjust = "inward"
  ) +
  facet_wrap(~ wing, scales = "free") +
  ylab("Centroid Size") + xlab("") +
  labs(color='Sample') +
  #ylim(13, NA) +
  scale_color_manual(aesthetics = "color", values = colorBlindBlack8) +
  theme_classic()
ggsave("images/csPlot.pdf", csPlot, width = 12, height = 6)

rm(csPlot, wCSsummary)


# Procuster Point Plot -------------------------------------------------------
# Calculate Mean Position
wPcSummary <- wTibble %>% 
  group_by(landmark, wing) %>% 
  summarise(
    mean_x = mean(x),
    mean_y = mean(y)
  ) %>% 
  ungroup() %>% 
  mutate(
    wing = fct_relevel(wing, c("ForeWing", "BackWing"))
  )
# Generate a Link Tibble
linkTibble <- bind_rows(
    tibble(
      L1 = rep(fwLink[,1], each = 2),
      L2 = rep(fwLink[,2], each = 2),
      x = 0, y = 0,
      wing = "ForeWing",
      grp = rep(paste0(fwLink[,1], fwLink[,2]), each = 2)
    ),
    tibble(
      L1 = rep(bwLink[,1], each = 2),
      L2 = rep(bwLink[,2], each = 2),
      x = 0, y = 0,
      wing = "BackWing",
      grp = rep(paste0(bwLink[,1], bwLink[,2]), each = 2)
    )
  ) %>% 
  mutate(
    wing = fct_relevel(wing, c("ForeWing", "BackWing"))
  )

# We need to extract for each link the corresponding x, y coordinates
for(i in seq_along(1:nrow(linkTibble))){
  wing <- linkTibble$wing[i]
  # check if first or second coordinate
  L <- ifelse(i %% 2, linkTibble$L1[i], linkTibble$L2[i])
  linkTibble$x[i] <- wPcSummary %>% filter(landmark == .env$L & wing == .env$wing) %>% dplyr::select(mean_x) %>% pull()
  linkTibble$y[i] <- wPcSummary %>% filter(landmark == .env$L & wing == .env$wing) %>% dplyr::select(mean_y) %>% pull()
}
rm(i, wing, L)

pcPlot <- ggplot(wTibble, aes(x = x, y = y, color = sample)) +
  geom_line(data = linkTibble, aes(x = x, y = y, group = grp, color = NA), color = colorBlindBlack8[7], show.legend = F) +
  geom_point(alpha = 0.5) + 
  ylab("Y") + xlab("X") + 
  facet_wrap(~ wing) +
  labs(color='Sample') +
  scale_color_manual(values = colorBlindBlack8, aesthetics = "color") +
  theme_classic() + coord_equal()

ggsave("images/pcPlot.pdf", pcPlot, width = 12, height = 3)

# PCA ---------------------------------------------------------------------
# generate long format of coords, not needed as we use our tibble
# x <- two.d.array(fwProc$coords)
widerTibble <- wTibble %>% 
  pivot_wider(names_from = landmark, values_from = c(x, y))


# FW PCA ------------------------------------------------------------------
fwWideLandmarks <- widerTibble %>% 
  filter(wing == "ForeWing") %>% 
  dplyr::select(starts_with("x_") | starts_with("y_"))

# Calculate PCA
fwPCA <- prcomp(fwWideLandmarks, scale = F)
# Extract Data
fwPCAValues<- tidy(fwPCA, matrix = "u")
# Combine with Sample
fwPCAValues$sample <- rep(
  widerTibble$sample[widerTibble$wing == "ForeWing"], each = max(fwPCAValues$PC)
  )
# Summary Stats
fwPCASummary <- tidy(fwPCA, matrix = "d")
# Generate Tibble for Plotting
fwPC12 <- fwPCAValues %>% 
  filter(between(PC, 1,2)) %>% 
  pivot_wider(names_from = PC, values_from = value, names_prefix = "PC")
# Convex Hull
fwPCAhull <- fwPC12 %>%
  group_by(sample) %>%
  slice(chull(PC1, PC2))

fwPCAplot <- ggplot(fwPC12) + 
  geom_hline(aes(yintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_point(
    aes(PC1, PC2, colour = sample, shape = sample), 
    size = 2.5
  ) + 
  geom_polygon(
    data = fwPCAhull, 
    aes(x = PC1, y = PC2, color = sample, fill = sample), 
    alpha = 0.1
  ) +
  labs(
    x = paste("PC1 (", round(fwPCASummary$percent[1]*100, 0), "%)", sep=""),
    y = paste("PC2 (", round(fwPCASummary$percent[2]*100, 0), "%)", sep="")
  ) +
  ggtitle("PCA - ForeWing") + labs(color="", shape="", fill="") +
  scale_color_manual(values = colorBlindBlack8, aesthetics = "color") +
  scale_color_manual(values = colorBlindBlack8, aesthetics = "fill") +
  theme_light() +
  scale_x_continuous(breaks = seq(-10, 10, by = 0.01)) +
  scale_y_continuous(breaks = seq(-10, 10, by = 0.01)) +
  coord_equal() +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank()
  )

ggsave("images/fwPCAplot.pdf", fwPCAplot, width = 5, height = 5)

fwTemplatePCAplot <- 
  fwPCAplot <- ggplot(fwPC12) + 
  geom_hline(aes(yintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  labs(
    x = paste("PC1 (", round(fwPCASummary$percent[1]*100, 0), "%)", sep=""),
    y = paste("PC2 (", round(fwPCASummary$percent[2]*100, 0), "%)", sep="")
  ) +
  ggtitle("TPS - ForeWing - Extrapolated to 0.15") + labs(color="", shape="", fill="") +
  theme_light() +
  scale_x_continuous(breaks = c(-0.15, 0.15), limit = c(-0.15, 0.15)) +
  scale_y_continuous(breaks = c(-0.15, 0.15), limit = c(-0.15, 0.15)) +
  coord_equal() +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank()
  )

ggsave("images/fwtemplatePCAplot.pdf", fwTemplatePCAplot, width = 7, height = 7)

# Use first and second Dimension
PC <- fwPCA$x[,1:2]
fwPreds <- shape.predictor(
  fwProc$coords, x = PC, 
  pred1 = c(-0.15,0), pred2 = c(0.15,0),
  pred3 = c(0,-0.15), pred4 = c(0,0.15)
  )
fwMshape <- mshape(fwProc$coords)

pdf(file="images/fw_pc1_negative.pdf")
plotRefToTarget(fwMshape, fwPreds$pred1, links = fwLink)
dev.off()
pdf(file="images/fw_pc1_positive.pdf")
plotRefToTarget(fwMshape, fwPreds$pred2, links = fwLink)
dev.off()
pdf(file="images/fw_pc2_negative.pdf")
plotRefToTarget(fwMshape, fwPreds$pred3, links = fwLink)
dev.off()
pdf(file="images/fw_pc2_positive.pdf")
plotRefToTarget(fwMshape, fwPreds$pred4, links = fwLink)
dev.off()

# BW PCA ------------------------------------------------------------------
bwWideLandmarks <- widerTibble %>% 
  filter(wing == "BackWing") %>% 
  dplyr::select(starts_with("x_") | starts_with("y_")) %>% 
  select_if(~sum(!is.na(.)) > 0)


bwPCA <- prcomp(bwWideLandmarks, scale = F)
# Extract Data
bwPCAValues<- tidy(bwPCA, matrix = "u")
# Combine with Sample
bwPCAValues$sample <- rep(
  widerTibble$sample[widerTibble$wing == "BackWing"], each = max(bwPCAValues$PC)
)
# Summary Stats
bwPCASummary <- tidy(bwPCA, matrix = "d")
# Generate Tibble for Plotting
bwPC12 <- bwPCAValues %>% 
  filter(between(PC, 1,2)) %>% 
  pivot_wider(names_from = PC, values_from = value, names_prefix = "PC")
# Convex Hull
bwPCAhull <- bwPC12 %>%
  group_by(sample) %>%
  slice(chull(PC1, PC2))

bwPCAplot <- ggplot(bwPC12) + 
  geom_hline(aes(yintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_point(
    aes(PC1, PC2, colour = sample, shape = sample), 
    size = 2.5
  ) + 
  geom_polygon(
    data = bwPCAhull, 
    aes(x = PC1, y = PC2, color = sample, fill = sample), 
    alpha = 0.1
  ) +
  labs(
    x = paste("PC1 (", round(bwPCASummary$percent[1]*100, 0), "%)", sep=""),
    y = paste("PC2 (", round(bwPCASummary$percent[2]*100, 0), "%)", sep="")
  ) +
  ggtitle("PCA - BackWing") + labs(color="", shape="", fill="") +
  scale_color_manual(values = colorBlindBlack8[-1], aesthetics = "color") +
  scale_color_manual(values = colorBlindBlack8[-1], aesthetics = "fill") +
  theme_light() +
  scale_x_continuous(breaks = seq(-10, 10, by = 0.01)) +
  scale_y_continuous(breaks = seq(-10, 10, by = 0.01)) +
  coord_equal() +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank()
  )

ggsave("images/bwPCAplot.pdf", bwPCAplot, width = 5, height = 5)

bwTemplatePCAplot <- 
  fwPCAplot <- ggplot(bwPC12) + 
  geom_hline(aes(yintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  labs(
    x = paste("PC1 (", round(bwPCASummary$percent[1]*100, 0), "%)", sep=""),
    y = paste("PC2 (", round(bwPCASummary$percent[2]*100, 0), "%)", sep="")
  ) +
  ggtitle("TPS - BackWing - Extrapolated to 0.15") + labs(color="", shape="", fill="") +
  theme_light() +
  scale_x_continuous(breaks = c(-0.15, 0.15), limit = c(-0.15, 0.15)) +
  scale_y_continuous(breaks = c(-0.15, 0.15), limit = c(-0.15, 0.15)) +
  coord_equal() +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank()
  )

ggsave("images/bwtemplatePCAplot.pdf", bwTemplatePCAplot, width = 7, height = 7)

# Use first and second Dimension
PC <- bwPCA$x[,1:2]
bwPreds <- shape.predictor(
  bwProc$coords, x = PC, 
  pred1 = c(-0.15,0), pred2 = c(0.15,0),
  pred3 = c(0,-0.15), pred4 = c(0,0.15)
)
bwMshape <- mshape(bwProc$coords)

pdf(file="images/bw_pc1_negative.pdf")
plotRefToTarget(bwMshape, bwPreds$pred1, links = bwLink)
dev.off()
pdf(file="images/bw_pc1_positive.pdf")
plotRefToTarget(bwMshape, bwPreds$pred2, links = bwLink)
dev.off()
pdf(file="images/bw_pc2_negative.pdf")
plotRefToTarget(bwMshape, bwPreds$pred3, links = bwLink)
dev.off()
pdf(file="images/bw_pc2_positive.pdf")
plotRefToTarget(bwMshape, bwPreds$pred4, links = bwLink)
dev.off()

# traditional cubital index -----------------------------------------------

# LM 15-17 = a, LM 17-18 = b
point_a  <- wTibble %>% filter(landmark == 15 & wing == "ForeWing") %>% dplyr::select(x,y)
point_ab <- wTibble %>% filter(landmark == 17 & wing == "ForeWing") %>% dplyr::select(x,y)
point_b  <- wTibble %>% filter(landmark == 18 & wing == "ForeWing") %>% dplyr::select(x,y)
point_id <- wTibble %>% filter(landmark == 18 & wing == "ForeWing") %>% dplyr::select(id)
# point distance on flat surface
distance_a <- raster::pointDistance(point_a, point_ab, lonlat=FALSE)
distance_b <- raster::pointDistance(point_ab, point_b, lonlat=FALSE)
# cubital index
distance_ab <- distance_a / distance_b
point_id$cubital = distance_ab
# add to our tibble
wTibble <- left_join(wTibble, point_id, by = c("id" = "id"))

wCIsummary <- wTibble %>% 
  filter(wing == "ForeWing" & landmark == 1) %>% 
  group_by(sample) %>% 
  summarise(
    mean = format(round(mean(cubital),1), nsmall = 1),
    median = format(round(median(cubital),1), nsmall = 1),
    sd = format(round(sd(cubital),2), nsmall = 2),
    n = n()
  )

ciPlot <- wTibble %>% 
  filter(wing == "ForeWing" & landmark == 1) %>% 
  ggplot(., aes(x = sample, y = cubital, color = sample)) +
  geom_boxplot() +
  geom_point() + 
  geom_text(
    data = wCIsummary, 
    aes(
      x = sample, 
      y = 1, 
      label = paste0(
        "Mean = ", mean, "\n", "SD = ", sd, "\n", "Median = ", median, "\n", "n = ", n
      )
    ),
    hjust = 0,
    nudge_x = -0.4,
    vjust = "inward"
  ) +
  ylab("Cubital Index") + xlab("") +
  labs(color='Sample') +
  #ylim(13, NA) +
  scale_color_manual(aesthetics = "color", values = colorBlindBlack8) +
  theme_classic()

ggsave("images/ciPlot.pdf", ciPlot, width = 7, height = 4)


# FW CVA ---------------------------------------------------------------------

# create wide format for linear discriminant analysis
# we use the PCA dimensions 1:10
fwWideSampleLM <- bind_cols(
    as_tibble(fwPCA$x[,1:10]), 
    widerTibble %>% filter(wing == "ForeWing") %>% dplyr::select(sample)
  )

fwLda  <- MASS::lda(sample ~ ., data = fwWideSampleLM, CV = F)
fwPred <- predict(fwLda, fwWideSampleLM)
# Generate tibble for plotting
fwLdaTibble <- tibble(
  sample = fwWideSampleLM %>% pull(sample),
  LD1 = fwPred$x[,1],
  LD2 = fwPred$x[,2]
  )
# Between Group Variation
fwLdaProp <- fwLda$svd^2/sum(fwLda$svd^2)

# Density Plot
# ggplot(fwLdaTibble, aes(x = LD1)) +
#   geom_density(aes(color=sample)) +
#   labs(x = "CV1")

# Convex Hull
fwCVhull <- fwLdaTibble %>%
  group_by(sample) %>%
  slice(chull(LD1, LD2))

fwLdaPlot <- ggplot(fwLdaTibble) + 
  geom_hline(aes(yintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_point(
    aes(LD1, LD2, colour = sample, shape = sample), 
    size = 2.5
  ) + 
  geom_polygon(
    data = fwCVhull, 
    aes(x = LD1, y = LD2, color = sample, fill = sample), 
    alpha = 0.1
    ) +
  labs(
    x = paste("CV1 (", round(fwLdaProp[1]*100, 0), "%)", sep=""),
    y = paste("CV2 (", round(fwLdaProp[2]*100, 0), "%)", sep="")
    ) +
  ggtitle("CVA - ForeWing") + labs(color="", shape="", fill="") +
  scale_color_manual(values = colorBlindBlack8, aesthetics = "color") +
  scale_color_manual(values = colorBlindBlack8, aesthetics = "fill") +
  theme_light() +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  scale_y_continuous(breaks = seq(-10, 10, by = 1)) +
  coord_equal() +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank()
    )

# Confusion Matrix with Jacknife
fwJackLda       <- MASS::lda(sample ~ ., data = fwWideSampleLM, CV = T)
fwConfJack      <- table(fwWideSampleLM$sample, fwJackLda$class)
fwJackExplained <- sum(fwConfJack[row(fwConfJack) == col(fwConfJack)]) / sum(fwConfJack)
fwJackExplained
fwConfJack

# BW CVA ---------------------------------------------------------------------

# create wide format for linear discriminant analysis
# we use the PCA dimensions 1:10
bwWideSampleLM <- bind_cols(
  as_tibble(bwPCA$x[,1:10]), 
  widerTibble %>% filter(wing == "BackWing") %>% dplyr::select(sample)
) %>% mutate(
  sample = fct_drop(sample)
)

bwLda  <- MASS::lda(sample ~ ., data = bwWideSampleLM, CV = F)
bwPred <- predict(bwLda, bwWideSampleLM)
# Generate tibble for plotting
bwLdaTibble <- tibble(
  sample = bwWideSampleLM %>% pull(sample),
  LD1 = bwPred$x[,1],
  LD2 = bwPred$x[,2]
)
# Between Group Variation
bwLdaProp <- bwLda$svd^2/sum(bwLda$svd^2)

# Density Plot
# ggplot(fwLdaTibble, aes(x = LD1)) +
#   geom_density(aes(color=sample)) +
#   labs(x = "CV1")

# Convex Hull
bwCVhull <- bwLdaTibble %>%
  group_by(sample) %>%
  slice(chull(LD1, LD2))

bwLdaPlot <- ggplot(bwLdaTibble) + 
  geom_hline(aes(yintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.8, linetype="dashed") +
  geom_point(
    aes(LD1, LD2, colour = sample, shape = sample), 
    size = 2.5
  ) + 
  geom_polygon(
    data = bwCVhull, 
    aes(x = LD1, y = LD2, color = sample, fill = sample), 
    alpha = 0.1
  ) +
  labs(
    x = paste("CV1 (", round(bwLdaProp[1]*100, 0), "%)", sep=""),
    y = paste("CV2 (", round(bwLdaProp[2]*100, 0), "%)", sep="")
  ) +
  ggtitle("CVA - BackWing") + labs(color="", shape="", fill="") +
  scale_color_manual(values = colorBlindBlack8[-1], aesthetics = "color") +
  scale_color_manual(values = colorBlindBlack8[-1], aesthetics = "fill") +
  theme_light() +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  scale_y_continuous(breaks = seq(-10, 10, by = 1)) +
  coord_equal() +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank()
  )

# Confusion Matrix with Jacknife
bwJackLda       <- MASS::lda(sample ~ ., data = bwWideSampleLM, CV = T)
bwConfJack      <- table(bwWideSampleLM$sample, bwJackLda$class)
bwJackExplained <- sum(bwConfJack[row(bwConfJack) == col(bwConfJack)]) / sum(bwConfJack)
bwJackExplained
bwConfJack


ggsave("images/cvaPlot.pdf", fwLdaPlot | bwLdaPlot, width = 12, height = 6)

# CVA Freq ----------------------------------------------------------------

fwDens <- ggplot(fwLdaTibble, aes(x = LD1, color = sample, fill = sample)) +
  geom_density(alpha = 0.3, show.legend = F) +
  ggtitle("Distribution Density - BackWing") + labs(color="", shape="", fill="") +
  labs(
    x = paste("CV1 (", round(fwLdaProp[1]*100, 0), "%)", sep="")
  ) +
  ylab("Sample Density") +
  scale_color_manual(values = colorBlindBlack8, aesthetics = "color") +
  scale_color_manual(values = colorBlindBlack8, aesthetics = "fill") +
  theme_classic()

bwDens <- ggplot(bwLdaTibble, aes(x = LD1, color = sample, fill = sample)) +
  geom_density(alpha = 0.3) +
  ggtitle("Distribution Density - BackWing") + labs(color="", shape="", fill="") +
  labs(
    x = paste("CV1 (", round(bwLdaProp[1]*100, 0), "%)", sep="")
  ) +
  ylab("Sample Density") +
  scale_color_manual(values = colorBlindBlack8[-1], aesthetics = "color") +
  scale_color_manual(values = colorBlindBlack8[-1], aesthetics = "fill") +
  theme_classic()

fwDens | bwDens

ggsave("images/densPlot.pdf", fwDens | bwDens, width = 12, height = 6)
