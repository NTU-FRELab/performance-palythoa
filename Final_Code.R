# Core data manipulation
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(plyr)
library(stringr)
# Plotting
library(ggplot2)
library(ggpubr)
library(patchwork)
library(gridExtra)
library(paletteer)
# Statistics and modeling
library(funspace)
library(vegan)
library(mFD)
library(ade4)
library(mvabund)
library(rcompanion)
library(coin)
library(Hotelling)
library(rjags)
library(nlme)
library(minpack.lm)
library(FSA)
library(car)
library(broom)
library(IDPmisc)
# Specialized packages
library(SIBER)
setwd("D:/Projet/These_FRE_Lab/Axe2/Stat")

######ENVIRONMENTAL CONDITIONS######
data_env <- read.table("data_env.txt", header = TRUE, sep = "\t", dec = ".")
row.names(data_env)=data_env$sample_no
data_env$region <- as.factor(data_env$region)

#temp
perm_test_temp<- wilcox_test(temp ~ region, 
                             data = data_env, 
                             distribution = approximate(nresample = 999))
p_value_temp <- pvalue(perm_test_temp)
a=ggplot(data_env, aes(x = region, y = temp, fill = region)) +
  geom_boxplot(na.rm = TRUE) +
  scale_fill_manual(values = c("North Taiwan" = "#00008B", "Green Island" = "#8B0000")) +
  labs(x = "", y = "°C", title = "Temperature") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(data_env$temp, na.rm = TRUE) + 0, label =  if (p_value_temp <= 0.001) {"***"} 
                else if (p_value_temp <= 0.01) {"**"} 
                else if (p_value_temp <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#o2_con
perm_test_o2_con<- wilcox_test(o2_con ~ region, 
                               data = data_env, 
                               distribution = approximate(nresample = 999))
p_value_o2_con <- pvalue(perm_test_o2_con)
b=ggplot(data_env, aes(x = region, y = o2_con, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("North Taiwan" = "#00008B", "Green Island" = "#8B0000")) +
  labs(x = "", y = "mg/L", title = "O2 Concentration") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(o2_con,na.rm = TRUE) + 0, label =  if (p_value_o2_con <= 0.001) {"***"} 
                else if (p_value_o2_con <= 0.01) {"**"} 
                else if (p_value_o2_con <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#turbi
perm_test_turbi<- wilcox_test(turbi ~ region, 
                              data = data_env, 
                              distribution = approximate(nresample = 999))
p_value_turbi<- pvalue(perm_test_turbi)
c=ggplot(data_env, aes(x = region, y = turbi, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("North Taiwan" = "#00008B", "Green Island" = "#8B0000")) +
  labs(x = "", y = "NTU", title = "Turbidity") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(turbi, na.rm = TRUE) + 0, label =  if (p_value_turbi <= 0.001) {"***"} 
                else if (p_value_turbi <= 0.01) {"**"} 
                else if (p_value_turbi <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#chla
perm_test_chla<- wilcox_test(chla ~ region, 
                             data = data_env, 
                             distribution = approximate(nresample = 999))
p_value_chla<- pvalue(perm_test_chla)
d=ggplot(data_env, aes(x = region, y = chla, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("North Taiwan" = "#00008B", "Green Island" = "#8B0000")) +
  labs(x = "", y = "ppb", title = "Chlorophyll-a") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(chla, na.rm = TRUE) + 0, label =  if (p_value_chla <= 0.001) {"***"} 
                else if (p_value_chla <= 0.01) {"**"} 
                else if (p_value_chla <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#PAR
perm_test_PAR<- wilcox_test(PAR ~ region, 
                            data = data_env, 
                            distribution = approximate(nresample = 999))
p_value_PAR<- pvalue(perm_test_PAR)
e=ggplot(data_env, aes(x = region, y = PAR, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("North Taiwan" = "#00008B", "Green Island" = "#8B0000")) + 
  labs(x = "", y = "μmol photons m⁻² s⁻¹", title = "PAR") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(PAR, na.rm = TRUE) + 0, label =  if (p_value_PAR <= 0.001) {"***"} 
                else if (p_value_PAR <= 0.01) {"**"} 
                else if (p_value_PAR <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")
grid.arrange(a,b,c,d,e, ncol=2, nrow = 3)


summary_table <- data_env %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    sd_temp = sd(temp, na.rm = TRUE),
    mean_o2_con = mean(o2_con, na.rm = TRUE),
    sd_o2_con = sd(o2_con, na.rm = TRUE),
    mean_turbi = mean(turbi, na.rm = TRUE),
    sd_turbi = sd(turbi, na.rm = TRUE),
    mean_chla = mean(chla, na.rm = TRUE),
    sd_chla = sd(chla, na.rm = TRUE),
    mean_PAR = mean(PAR, na.rm = TRUE),
    sd_PAR = sd(PAR, na.rm = TRUE))
print(summary_table)

######TRAIT SPACE######
data <- read.table("data_total.txt", header = TRUE, sep = "\t", dec = ".")
row.names(data)=data$sample_no
#Global trait space
data_global=data[,1:17]

funspaceDim(data_global[,6:17]) #retain the first 4 components
pca.trait <- princomp(data_global[, 6:17], cor = TRUE)
trait_space_ord <- funspace(
  x = pca.trait,
  PCs = c(1, 2),#Change the PC to plot the other dimensions
  group.vec = data$Order,
  n_divisions = 300)
zoa_idx <- data$Order == "Zoantharia"
point_colors_zoa <- ifelse(
  data$species[zoa_idx] == "Palythoa_tuberculosa2",
  "navyblue",
  "grey")

plot(
  x = trait_space_ord,
  type = "groups",       # one panel per taxonomic order
  which = "Zoantharia",  # select the Zoantharia panel
  quant.plot = TRUE,
  globalContour = TRUE,
  pnt = TRUE,
  pnt.cex = 0.9,
  pnt.col = point_colors_zoa,
  axis.title.line = 2,
  repel = TRUE,
  main = NULL,
  arrows = T,
  arrows.length = 0.9,
  col=brewer.pal(n = 3, name = "Spectral"))

#Global functional diversity indices
traits <- data_global[1:312, 6:17]
order <- data_global[1:312,]$Order
fspace <- tr.cont.fspace(
  sp_tr        = traits, 
  pca          = TRUE, 
  nb_dim       = 10, 
  scaling      = 'scale_center',
  compute_corr = 'pearson')

unique_order<- unique(order)
abundance_matrix <- matrix(0, nrow = length(unique_order), ncol = nrow(traits))
rownames(abundance_matrix) <- unique_order
colnames(abundance_matrix) <- rownames(traits)

for (i in 1:length(unique_order)) {
  reg <- unique_order[i]
  abundance_matrix[i, order == reg] <- 1}

abundance_matrix <- as.numeric(abundance_matrix)
dim(abundance_matrix) <- c(length(unique_order), nrow(traits))
rownames(abundance_matrix) <- unique_order
colnames(abundance_matrix) <- rownames(traits)

sp_faxes_coord <- fspace$sp_faxes_coord
alpha_index=alpha.fd.multidim(sp_faxes_coord   = sp_faxes_coord[, c('PC1', 'PC2','PC3','PC4')],#4 components previously retained
                              asb_sp_w         = abundance_matrix, 
                              ind_vect         = c('feve', 'fric', 'fdis'),
                              scaling          = TRUE, 
                              check_input      = TRUE, 
                              details_returned = TRUE)
fd_ind_values <- alpha_index$functional_diversity_indices
fd_ind_values

#P.tuberculosa trait space
data_PT=data[313:332,c(-15,-17,-18)]

funspaceDim(data_PT[,6:20]) #retain the first 2 components
pca.trait=princomp(data_PT[,6:20], cor = TRUE)
trait_space_global=funspace(x = pca.trait, PCs = c(1,2), n_divisions = 300)
plot(x = trait_space_global,
     type = "global",
     pnt = T,
     pnt.cex = 0.9,
     quant.plot = TRUE, 
     arrows = T, 
     arrows.length = 0.9,
     pnt.col = "navyblue",
     col=brewer.pal(n = 3, name = "Spectral"))

trait_space_region= funspace(pca.trait, PCs = c(1,2), group.vec = data_PT$region, n_divisions = 300)
plot(x = trait_space_region,
     type = "groups",
     quant.plot = TRUE,
     globalContour = T,
     pnt = T,
     pnt.cex = 0.8,
     pnt.col = "black",
     col=brewer.pal(n = 3, name = "Blues"))

#Test difference of distribution
scaled_data <- scale(data_PT[,6:20])
dist_matrix <- vegdist(scaled_data, method = "euclidean")
adonis_result <- adonis2(dist_matrix ~ region, data = data_PT)
print(adonis_result)#0.001***

#Global functional diversity indices
traits <- data_PT[,6:20]
region <- data_PT$region
fspace <- tr.cont.fspace(
  sp_tr        = traits, 
  pca          = TRUE, 
  nb_dim       = 10, 
  scaling      = 'scale_center',
  compute_corr = 'pearson')

unique_region<- unique(region)
abundance_matrix <- matrix(0, nrow = length(unique_region), ncol = nrow(traits))
rownames(abundance_matrix) <- unique_region
colnames(abundance_matrix) <- rownames(traits)

for (i in 1:length(unique_region)) {
  reg <- unique_region[i]
  abundance_matrix[i, region == reg] <- 1}

abundance_matrix <- as.numeric(abundance_matrix)
dim(abundance_matrix) <- c(length(unique_region), nrow(traits))
rownames(abundance_matrix) <- unique_region
colnames(abundance_matrix) <- rownames(traits)

sp_faxes_coord <- fspace$sp_faxes_coord
alpha_index=alpha.fd.multidim(sp_faxes_coord   = sp_faxes_coord[, c('PC1', 'PC2')],#4 components previously retained
                              asb_sp_w         = abundance_matrix, 
                              ind_vect         = c('feve', 'fric', 'fdis'),
                              scaling          = TRUE, 
                              check_input      = TRUE, 
                              details_returned = TRUE)
fd_ind_values <- alpha_index$functional_diversity_indices
fd_ind_values

#Trait space by convex hull
pca_res <- prcomp(traits, center = TRUE, scale. = TRUE)
pca_scores <- as.data.frame(pca_res$x[, 1:2])
pca_scores$Region <- data_PT$region   # add region info
var_expl <- (pca_res$sdev^2 / sum(pca_res$sdev^2)) * 100
pca_scores$PC1 <- pca_scores$PC1
pca_scores$PC2 <- -pca_scores$PC2
hulls <- pca_scores %>%
  group_by(Region) %>%
  slice(chull(PC1, PC2))
ggplot(pca_scores, aes(PC1, PC2, color = Region)) +
  geom_polygon(
    data = hulls,
    aes(fill = Region),
    alpha = 0.2,
    color = NA) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Tropical" = "#8B0000",
               "Subtropical" = "#00008B")) +
  scale_fill_manual(
    values = c(
      "Tropical" = "#8B0000",
      "Subtropical" = "#00008B")) +
  theme_classic() +
  labs(
    x = paste0("PC1 (", round(var_expl[1], 1), "%)"),
    y = paste0("PC2 (", round(var_expl[2], 1), "%)"),
    title = "") +
  guides(fill = "none") +
  theme(legend.position = "none")

######ISOTOPIC ANALYSIS######
#Test Hotelling
data_iso <- data.frame(
  delta13C_host = data_PT$H.Delta13C,
  delta15N_host = data_PT$H.Delta15N,
  delta13C_symb = data_PT$Z.Delta13C,
  delta15N_symb = data_PT$Z.Delta15N,
  region = data_PT$region)

hostGI <- data_iso[c(1:10), c("delta13C_host", "delta15N_host")]
symbGI <- data_iso[c(1:10), c("delta13C_symb", "delta15N_symb")]
resultGI <- hotelling.test(hostGI, symbGI, paired = TRUE)
print(resultGI)#0.1153 

hostNT <- data_iso[c(11:20), c("delta13C_host", "delta15N_host")]
symbNT <- data_iso[c(11:20), c("delta13C_symb", "delta15N_symb")]
resultNT <- hotelling.test(hostNT, symbNT, paired = TRUE)
print(resultNT)#0.000822 

#Siber analysis
data_siber <- read.table("data_siber.txt", header = TRUE, sep = "\t", dec = ".")
row.names(data_siber)=data_siber$sample
data_siber$sample=NULL
data_siber$group[1:10] <- "Coral_TRO"
data_siber$group[11:20] <- "Coral_SUB"
data_siber$group[21:30] <- "Zoox_TRO"
data_siber$group[31:40] <- "Zoox_SUB"

#Plot
data_siber_Obj <- createSiberObject(data_siber)
community.hulls.args <- list(col = 3, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.40, lty = 1, lwd = 2)
group.hulls.args     <- list(lty = 2, col = "grey20")
palette(c("#00008B","#8B0000","#6699FF","#FF6666"))
layout(matrix(c(2,0,1,3), 2, 2, byrow = TRUE), widths=c(4,1), heights=c(1,4))
#dev.off()
par(mar = c(5,5,1,1))
plotSiberObject(data_siber_Obj,
                ax.pad = 2, 
                hulls = T, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'permille'),
                ylab = expression({delta}^15*N~'permille'),
                x.limits = c(-19,-12),
                y.limits = c(-2,8))
par(mar = c(0,5,1,1))
boxplot(iso1 ~ group, data = data_siber,
        horizontal = TRUE, axes = FALSE, col = c("#00008B","#8B0000","#6699FF","#FF6666"),
        ylim = c(-19,-12), xlab = "", ylab = "")
par(mar = c(5,0,1,1))
boxplot(iso2 ~ group, data = data_siber,
        horizontal = FALSE, axes = F, col = c("#00008B","#8B0000","#6699FF","#FF6666"),
        ylim = c(-2,8),   xlab = "", ylab = "")

#Siber ellipse
tropical_df <- data_siber %>% filter(community == "Tropical")
subtropical_df <- data_siber %>% filter(community == "Subtropical")
tropical_siber <- createSiberObject(tropical_df)
subtropical_siber <- createSiberObject(subtropical_df)
#Overlap
ellipse1SUB <- "Coral_SUB"
ellipse2SUB <- "Zoox_SUB"
sea.overlapSUB <- maxLikOverlap(ellipse1SUB, ellipse2SUB, subtropical_siber, 
                                p.interval = NULL, n = 100)
sea.overlapSUB

ellipse1TRO <- "Coral_TRO"
ellipse2TRO <- "Zoox_TRO"
sea.overlapTRO <- maxLikOverlap(ellipse1TRO, ellipse2TRO, tropical_siber, 
                                p.interval = NULL, n = 100)
sea.overlapTRO

#SEAc
group.ML.TRO <- groupMetricsML(tropical_siber)
print(group.ML.TRO)
group.ML.SUB <- groupMetricsML(subtropical_siber)
print(group.ML.SUB)
#SEAb
parms <- list()
parms$n.iter <- 2 * 10^4
parms$n.burnin <- 1 * 10^3
parms$n.thin <- 10
parms$n.chains <- 2
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3
SEAb_siber <- createSiberObject(data_siber)
ellipses_posterior <- siberMVN(SEAb_siber, parms, priors)
SEAb <- siberEllipses(ellipses_posterior)
apply(SEAb, 2, mean)

#######TRAIT BY TRAIT#######
data_PT$region <- as.factor(data_PT$region)
                            
#H.Delta15N
perm_test_H_Delta15N <- wilcox_test(H.Delta15N ~ region, 
                                    data = data_PT, 
                                    distribution = approximate(nresample = 999))
p_value_H_Delta15N <- pvalue(perm_test_H_Delta15N)
a=ggplot(data_PT, aes(x = region, y = H.Delta15N, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = "‰", title = expression(bold(paste("Host ", delta^{15}, "N")))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(H.Delta15N) + 0.2, label =  if (p_value_H_Delta15N <= 0.001) {"***"} 
                else if (p_value_H_Delta15N <= 0.01) {"**"} 
                else if (p_value_H_Delta15N <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#H.Delta13C
perm_test_H_Delta13C <- wilcox_test(H.Delta13C ~ region, 
                                    data = data_PT, 
                                    distribution = approximate(nresample = 999))
p_value_H_Delta13C <- pvalue(perm_test_H_Delta13C)
b=ggplot(data_PT, aes(x = region, y = H.Delta13C, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = "‰", title = expression(bold(paste("Host ", delta^{13}, "C")))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(H.Delta13C) + 0.2, label =  if (p_value_H_Delta13C <= 0.001) {"***"} 
                else if (p_value_H_Delta13C <= 0.01) {"**"} 
                else if (p_value_H_Delta13C <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#Z.Delta15N
perm_test_Z_Delta15N <- wilcox_test(Z.Delta15N ~ region, 
                                    data = data_PT, 
                                    distribution = approximate(nresample = 999))
p_value_Z_Delta15N <- pvalue(perm_test_Z_Delta15N)
c=ggplot(data_PT, aes(x = region, y = Z.Delta15N, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = "‰", title = expression(bold(paste("Zooxanthellae ", delta^{15}, "N")))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(Z.Delta15N) + 0.2, label =  if (p_value_Z_Delta15N <= 0.001) {"***"} 
                else if (p_value_Z_Delta15N <= 0.01) {"**"} 
                else if (p_value_Z_Delta15N <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#Z.Delta13C
perm_test_Z_Delta13C <- wilcox_test(Z.Delta13C ~ region, 
                                    data = data_PT, 
                                    distribution = approximate(nresample = 999))
p_value_Z_Delta13C <- pvalue(perm_test_Z_Delta13C)
d=ggplot(data_PT, aes(x = region, y = Z.Delta13C, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = "‰", title = expression(bold(paste("Zooxanthellae ", delta^{13}, "C")))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(Z.Delta13C) + 0.2, label =  if (p_value_Z_Delta13C <= 0.001) {"***"} 
                else if (p_value_Z_Delta13C <= 0.01) {"**"} 
                else if (p_value_Z_Delta13C <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")
#H.ratio.C.N
perm_test_H.ratio.C.N <- wilcox_test(H.ratio.C.N ~ region, 
                                     data = data_PT, 
                                     distribution = approximate(nresample = 999))
p_value_H.ratio.CN <- pvalue(perm_test_H.ratio.C.N)
e=ggplot(data_PT, aes(x = region, y = H.ratio.C.N, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = "", title = "Host C/N ratio") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(H.ratio.C.N) + 0.2, label =  if (p_value_H.ratio.CN <= 0.001) {"***"} 
                else if (p_value_H.ratio.CN <= 0.01) {"**"} 
                else if (p_value_H.ratio.CN <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#Z.ratio.C.N
perm_test_Z.ratio.C.N <- wilcox_test(Z.ratio.C.N ~ region, 
                                     data = data_PT, 
                                     distribution = approximate(nresample = 999))
p_value_Z.ratio.C.N <- pvalue(perm_test_Z.ratio.C.N)
f=ggplot(data_PT, aes(x = region, y = Z.ratio.C.N, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = "", title = "Zooxanthellae C/N ratio") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(Z.ratio.C.N) + 0.2, label =  if (p_value_Z.ratio.C.N <= 0.001) {"***"} 
                else if (p_value_Z.ratio.C.N <= 0.01) {"**"} 
                else if (p_value_Z.ratio.C.N <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#Zdens
perm_test_Zdens <- wilcox_test(Z.dens ~ region, 
                               data = data_PT, 
                               distribution = approximate(nresample = 999))
p_value_Zdens <- pvalue(perm_test_Zdens)
g=ggplot(data_PT, aes(x = region, y = Z.dens, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = expression(paste("cells mgAFDW"^{-1})), title = "Zooxanthellae Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(Z.dens) + 0.2, label =  if (p_value_Zdens <= 0.001) {"***"} 
                else if (p_value_Zdens <= 0.01) {"**"} 
                else if (p_value_Zdens <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#Cdens
perm_test_C.dens <- wilcox_test(C.dens ~ region, 
                                data = data_PT, 
                                distribution = approximate(nresample = 999))
p_value_C.dens <- pvalue(perm_test_C.dens)
h=ggplot(data_PT, aes(x = region, y = C.dens, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = expression(paste("cells mgAFDW"^{-1})), title = "Cnidocyte Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(C.dens) + 0.2, label =  if (p_value_C.dens <= 0.001) {"***"} 
                else if (p_value_C.dens <= 0.01) {"**"} 
                else if (p_value_C.dens <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")


#Chla
perm_test_chla <- wilcox_test(chla ~ region, 
                              data = data_PT, 
                              distribution = approximate(nresample = 999))
p_value_chla <- pvalue(perm_test_chla)
i=ggplot(data_PT, aes(x = region, y = chla, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = expression(paste("μg mgAFDW"^{-1})), title = expression(bold(paste("Chlorophyll-", italic(a), " concentration")))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(chla) + 0.2, label =  if (p_value_chla <= 0.001) {"***"} 
                else if (p_value_chla <= 0.01) {"**"} 
                else if (p_value_chla <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")+
  coord_cartesian(ylim = c(0, 150))

#Chlb
perm_test_chlb <- wilcox_test(chlb ~ region, 
                              data = data_PT, 
                              distribution = approximate(nresample = 999))
p_value_chlb <- pvalue(perm_test_chlb)
j=ggplot(data_PT, aes(x = region, y = chlb, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = expression(paste("μg mgAFDW"^{-1})), title = expression(bold(paste("Chlorophyll-", italic(b), " concentration")))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(chlb) + 1, label =  if (p_value_chlb <= 0.001) {"***"} 
                else if (p_value_chlb <= 0.01) {"**"} 
                else if (p_value_chlb <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")+
  coord_cartesian(ylim = c(0, 2.5)) 

j
#Chlc
perm_test_chlc <- wilcox_test(chlc ~ region, 
                              data = data_PT, 
                              distribution = approximate(nresample = 999))
p_value_chlc <- pvalue(perm_test_chlc)
k=ggplot(data_PT, aes(x = region, y = chlc, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = expression(paste("μg mgAFDW"^{-1})), title = expression(bold(paste("Chlorophyll-", italic(c), " concentration")))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(chlc) + 15, label =  if (p_value_chlc <= 0.001) {"***"} 
                else if (p_value_chlc <= 0.01) {"**"} 
                else if (p_value_chlc <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")+
  coord_cartesian(ylim = c(0, 50)) 

#Chlca
perm_test_chlca <- wilcox_test(chlca ~ region, 
                               data = data_PT, 
                               distribution = approximate(nresample = 999))
p_value_chlca <- pvalue(perm_test_chlca)
l=ggplot(data_PT, aes(x = region, y = chlca, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = "ratio", title = expression(bold(paste("Ratio chlorophyll ", italic(c/a))))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(chlca) + 0.2, label =  if (p_value_chlc <= 0.001) {"***"} 
                else if (p_value_chlca <= 0.01) {"**"} 
                else if (p_value_chlca <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#Tot.pro.cont
perm_test_Tot.pro.cont<- wilcox_test(Tot.pro.cont ~ region, 
                                     data = data_PT, 
                                     distribution = approximate(nresample = 999))
p_value_Tot.pro.cont <- pvalue(perm_test_Tot.pro.cont)
m=ggplot(data_PT, aes(x = region, y = Tot.pro.cont, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = expression(paste("μg mgAFDW"^{-1})), title = "Total protein content") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(Tot.pro.cont) + 0.2, label =  if (p_value_Tot.pro.cont <= 0.001) {"***"} 
                else if (p_value_Tot.pro.cont <= 0.01) {"**"} 
                else if (p_value_Tot.pro.cont <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#Tot.Lip.cont
perm_test_Tot.Lip.cont <- wilcox_test(Tot.Lip.cont ~ region, 
                                      data = data_PT, 
                                      distribution = approximate(nresample = 999))
p_value_Tot.Lip.cont <- pvalue(perm_test_Tot.Lip.cont)
n=ggplot(data_PT, aes(x = region, y = Tot.Lip.cont, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = expression(paste("μg mgAFDW"^{-1})), title = "Total lipid content") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(Tot.Lip.cont) + 0.2, label =  if (p_value_Tot.Lip.cont <= 0.001) {"***"} 
                else if (p_value_Tot.Lip.cont <= 0.01) {"**"} 
                else if (p_value_Tot.Lip.cont <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

#In.cont
perm_test_In.cont <- wilcox_test(In.cont ~ region, 
                                 data = data_PT, 
                                 distribution = approximate(nresample = 999))
p_value_In.cont <- pvalue(perm_test_In.cont)
o=ggplot(data_PT, aes(x = region, y = In.cont, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Subtropical" = "#00008B", "Tropical" = "#8B0000")) +
  labs(x = "", y = expression(paste("mg mgAFDW"^{-1})), title = "Inorganic content") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none")+
  geom_text(aes(x = 1.5, y = max(In.cont) + 0.2, label =  if (p_value_In.cont <= 0.001) {"***"} 
                else if (p_value_In.cont <= 0.01) {"**"} 
                else if (p_value_In.cont <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")

grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o, ncol=3, nrow = 5)
dev.off()

######PAM ANALYSIS#######
mydata <- read.table("data_pam.txt", header = TRUE, sep = "", dec = ".")
mydata$PAR <- as.numeric(as.character(mydata$PAR))
mydata[, 2:11] <- lapply(mydata[, 2:11], function(x)
  as.numeric(as.character(x)))

mydata.long <- mydata %>%
  pivot_longer(
    -c(disc, PAR),
    names_to = "rep",
    values_to = "meas"
  ) %>%
  arrange(rep) %>%
  filter(complete.cases(.))

mydata.long$id <- as.factor(paste(mydata.long$disc, mydata.long$rep, sep = ""))
mydata.long$rep <- as.factor(mydata.long$rep)

#rETR calculation
mydata.long$rETR <- mydata.long$PAR * mydata.long$meas
mydata.long$PAR  <- ifelse(mydata.long$PAR <= 0, 0.1, mydata.long$PAR)
mydata.long$rETR <- ifelse(mydata.long$rETR <= 0, 0.01, mydata.long$rETR)

#Self start Platt model
SSPlatt.my <- stats::selfStart(
  ~ Pmax*(1-exp(-alpha*I/Pmax))*exp(-beta*I/Pmax),
  function(mCall, data, LHS, ...) {
    
    ## Extract x and y
    x <- mCall[["I"]]
    y <- LHS
    
    if (is.language(x) || ((length(x) == 1L) && is.character(x)))
      x <- eval(asOneSidedFormula(x)[[2L]], data)
    x <- as.numeric(x)
    
    if (is.language(y) || ((length(y) == 1L) && is.character(y)))
      y <- eval(asOneSidedFormula(y)[[2L]], data)
    y <- as.numeric(y)
    
    keep <- !is.na(x) & !is.na(y)
    x <- x[keep]
    y <- y[keep]
    
    if (length(unique(x)) < 6)
      stop("Too few distinct x values to fit a Platt model")
    
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
    
    ## Initial beta estimate
    ks <- (length(x)+1) - seq_len(
      max(4, which(rev(y) >= 0.9 * max(y))[1])
    )
    
    beta <- -lsfit(
      x[ks],
      log(pmax(y[ks] - min(y), 1e-8))
    )$coef[2]
    beta <- max(beta, 1e-8)
    
    ## First partial linear fit
    fit <- nls(
      y ~ exp(-x*exp(b)) - exp(-x*exp(a)),
      data = list(y = y, x = x, b = log(beta)),
      start = list(a = log(beta) + 3),
      control = nls.control(maxiter = 100, minFactor = 1/4096),
      algorithm = "plinear"
    )
    
    cf <- coef(fit)
    cf <- if (cf[2] > 0)
      c(cf[1], log(beta), cf[2:3])
    else
      c(log(beta), cf[1], -cf[2:3])
    
    ## Second refinement
    fit2 <- tryCatch(
      nls(
        y ~ exp(-x*exp(b)) - exp(-x*exp(a)),
        data = list(y = y, x = x),
        start = list(a = cf[1], b = cf[2]),
        control = nls.control(maxiter = 100, minFactor = 1/4096),
        algorithm = "plinear"
      ),
      error = function(e) NULL
    )
    
    if (!is.null(fit2)) {
      cf2 <- coef(fit2)
      cf2 <- if (cf2[3] > 0) cf2 else c(cf2[2:1], -cf2[3:4])
      cf <- if (cf2[1] < cf2[2]) cf else cf2
    }
    
    setNames(
      c((exp(cf[1]) - exp(cf[2])) * cf[3],
        exp(cf[2]) * cf[3],
        -cf[3:4]),
      c("alpha", "beta", "Pmax")
    )
  },
  c("alpha", "beta", "Pmax")
)

#Initial parameter estimation
starts <- mydata.long %>%
  group_by(id) %>%
  do(broom::tidy(
    getInitial(
      rETR ~ SSPlatt.my(PAR, alpha, beta, Ys),
      data = .))) %>%
  pivot_wider(names_from = names, values_from = x)
colnames(starts) <- c("id", "alpha.s", "beta.s", "Pmax.s")
starts <- starts[,-5]

#Onlinear model fit
test <- mydata.long %>%
  right_join(starts, by = "id") %>%
  group_by(id) %>%
  do(model = try(
    broom::tidy(
      nlsLM(
        rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*exp(-beta*PAR/Pmax),
        start = list(
          Pmax  = mean(.$Pmax.s),
          alpha = mean(.$alpha.s),
          beta  = mean(.$beta.s) ),data = .)),
    silent = TRUE))

#Parameter extraction
unest.test <- test %>% unnest(model)
df.param <- unest.test %>%
  select(id, term, estimate)
dat_wide <- df.param %>%
  pivot_wider(names_from = term, values_from = estimate)

#Derived photosynthetic parameters
dat_wide$ETRm <- dat_wide$Pmax *
  (dat_wide$alpha / (dat_wide$alpha + dat_wide$beta)) *
  ((dat_wide$beta / (dat_wide$alpha + dat_wide$beta)))^(dat_wide$beta / dat_wide$alpha)
dat_wide$Ek <- dat_wide$ETRm / dat_wide$alpha
dat_wide$Em <- (dat_wide$Pmax / dat_wide$alpha) *
  log((dat_wide$alpha + dat_wide$beta) / dat_wide$beta)
curve_parameter <- dat_wide %>%
  mutate(
    region = case_when(
      grepl("^GIPT", id) ~ "Tropical",
      grepl("^NOPT", id) ~ "Subtropical",
      TRUE              ~ NA_character_))

#Statistical analysis of parameters
curve_parameter=curve_parameter[c(-1,-11),]#remove mean per region

#alpha
curve_parameter$alpha <- as.numeric(as.character(curve_parameter$alpha))
curve_parameter$region <- as.factor(curve_parameter$region)
perm_test_alpha <- wilcox_test(alpha ~ region, 
                               data = curve_parameter, 
                               distribution = approximate(nresample = 999))
p_value_alpha <- pvalue(perm_test_alpha)
p=ggplot(curve_parameter, aes(x = region, y = alpha, fill = region)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("Subtropical" = "#0000B3", "Tropical" = "#8B0000")) +
  labs(x = "", y = "Alpha", title = "") +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none",  axis.text.x = element_blank(), panel.grid = element_blank(),            # remove grid lines
  )+
  geom_text(aes(x = 1.5, y = max(alpha) + 0.2, label =  if (p_value_alpha <= 0.001) {"***"} 
                else if (p_value_alpha <= 0.01) {"**"} 
                else if (p_value_alpha <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")
aggregate(alpha ~ region, data = curve_parameter, FUN = mean)


#beta
curve_parameter$beta <- as.numeric(as.character(curve_parameter$beta))
curve_parameter$region <- as.factor(curve_parameter$region)
perm_test_beta <- wilcox_test(beta ~ region, 
                              data = curve_parameter, 
                              distribution = approximate(nresample = 999))
p_value_beta <- pvalue(perm_test_beta)
q=ggplot(curve_parameter, aes(x = region, y = beta, fill = region)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("Subtropical" = "#0000B3", "Tropical" = "#8B0000")) +
  labs(x = "", y = "Beta", title = "") +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none",  axis.text.x = element_blank(),panel.grid = element_blank())+
  geom_text(aes(x = 1.5, y = max(curve_parameter$beta, na.rm = TRUE)+4, label =  if (p_value_beta <= 0.001) {"***"} 
                else if (p_value_beta <= 0.01) {"**"} 
                else if (p_value_beta <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")
aggregate(beta ~ region, data = curve_parameter, FUN = mean)

#ETRm
curve_parameter$ETRm <- as.numeric(as.character(curve_parameter$ETRm))
curve_parameter$region <- as.factor(curve_parameter$region)
perm_test_ETRm <- wilcox_test(ETRm ~ region, 
                              data = curve_parameter, 
                              distribution = approximate(nresample = 999))
p_value_ETRm <- pvalue(perm_test_ETRm)
r=ggplot(curve_parameter, aes(x = region, y = ETRm, fill = region)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("Subtropical" = "#0000B3", "Tropical" = "#8B0000")) +
  labs(x = "", y = "rETRm", title = "") +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none",  axis.text.x = element_blank(),panel.grid = element_blank())+
  geom_text(aes(x = 1.5, y = max(curve_parameter$ETRm, na.rm = TRUE)+20, label =  if (p_value_ETRm <= 0.001) {"***"} 
                else if (p_value_ETRm <= 0.01) {"**"} 
                else if (p_value_ETRm <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")
aggregate(ETRm ~ region, data = curve_parameter, FUN = mean)

#Ek
curve_parameter$Ek <- as.numeric(as.character(curve_parameter$Ek))
curve_parameter$region <- as.factor(curve_parameter$region)
perm_test_Ek <- wilcox_test(Ek ~ region, 
                            data = curve_parameter, 
                            distribution = approximate(nresample = 999))
p_value_Ek <- pvalue(perm_test_Ek)
s = ggplot(curve_parameter, aes(x = region, y = Ek, fill = region)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("Subtropical" = "#0000B3", "Tropical" = "#8B0000")) +
  labs(x = "", y = "Ek", title = "") +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none",  axis.text.x = element_blank(), panel.grid = element_blank())+
  geom_text(aes(x = 1.5, y = max(curve_parameter$Ek, na.rm = TRUE), label =  if (p_value_Ek <= 0.001) {"***"} 
                else if (p_value_Ek <= 0.01) {"**"} 
                else if (p_value_Ek <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")
aggregate(Ek ~ region, data = curve_parameter, FUN = mean)

#Em
curve_parameter$Em <- as.numeric(as.character(curve_parameter$Em))
curve_parameter$region <- as.factor(curve_parameter$region)
perm_test_Em <- wilcox_test(Em ~ region, 
                            data = curve_parameter, 
                            distribution = approximate(nresample = 999))
p_value_Em<- pvalue(perm_test_Em)
t = ggplot(curve_parameter, aes(x = region, y = Em, fill = region)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("Subtropical" = "#0000B3", "Tropical" = "#8B0000")) +
  labs(x = "", y = "Em", title = "") +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold", color = "black"),
    legend.position = "none",  axis.text.x = element_blank(), panel.grid = element_blank())+
  geom_text(aes(x = 1.5, y = max(curve_parameter$Em, na.rm = TRUE), label =  if (p_value_Em <= 0.001) {"***"} 
                else if (p_value_Em <= 0.01) {"**"} 
                else if (p_value_Em <= 0.05) {"*"} 
                else {"NS"}), size = 4, color = "black")
aggregate(Em ~ region, data = curve_parameter, FUN = mean)

#model-derived prediction of rETR
fit <- mydata.long %>%
  right_join(starts, by="id") %>%
  group_by(id) %>%
  do(model = try(nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*exp(-beta*PAR/Pmax),
                       start=list(Pmax=mean(.$Pmax.s), alpha=mean(.$alpha.s), beta=mean(.$beta.s)),
                       data=.),
                 silent=TRUE))

df.x <- data.frame(PAR = seq(0.1,1500,length=100))
pred.list <- lapply(1:nrow(fit), function(i) try(predict(fit$model[[i]], newdata=df.x)))
derived_data <- data.frame(t(do.call(rbind,pred.list)))
colnames(derived_data) <- as.character(fit$id)
derived_data[] <- lapply(derived_data, as.numeric)
derived_data$PAR <- df.x$PAR

derived_data <- derived_data %>%
  pivot_longer(-PAR, names_to="id", values_to="rETR") %>%
  arrange(id)

#Plot Light curve
#Tropical
derived_data_TRO <- derived_data %>%
  filter(grepl("GI", id))
derived_data_TRO <- derived_data_TRO[101:1000,]
u=ggplot(derived_data_TRO, aes(x = PAR, y = rETR)) +
  geom_line(aes(group = id),
            color = "grey70",
            alpha = 0.5,
            linewidth = 0.6) +
    geom_smooth(method = "loess",
              se = TRUE,
              color = "black",
              fill = "#8B0000",
              linewidth = 1.2,
              alpha = 0.3) +
  
  theme_classic() +
  labs(x = "PAR",
       y = "rETR")

#Subtropical
derived_data_SUB <- derived_data %>%
  filter(grepl("NO", id))
derived_data_SUB <- derived_data_SUB[101:600,]
v=ggplot(derived_data_SUB, aes(x = PAR, y = rETR)) +
  geom_line(aes(group = id),
            color = "grey70",
            alpha = 0.5,
            linewidth = 0.6) +
  geom_smooth(method = "loess",
              se = TRUE,
              color = "black",
              fill = "#00008B",
              linewidth = 1.2,
              alpha = 0.3) +
  
  theme_classic() +
  labs(x = "PAR",
       y = "rETR")

Top_row <- grid.arrange(u, v, ncol = 2)
bottom_row  <- grid.arrange(p, q, r, s, t, ncol = 5)
grid.arrange(Top_row,bottom_row,ncol =1)

#######SYMBIONT COMPOSITION#######
#Read data
data_symbiont <- read.table(
  "data_symbiont.txt",
  header = TRUE, sep = "\t", dec = ".")

data_long <- data_symbiont %>%
  pivot_longer(
    cols = -sample_name,
    names_to = "haplotype",
    values_to = "abundance") %>%
  mutate(region = ifelse(str_starts(toupper(sample_name), "TRO"), "TRO", "SUB"))

#Identify Top 10 haplotypes per region 
top10_region <- data_long %>%
  group_by(region, haplotype) %>%
  summarise(mean_abund = mean(abundance, na.rm = TRUE), .groups = "drop") %>%
  group_by(region) %>%
  slice_max(mean_abund, n = 10, with_ties = FALSE) %>%
  mutate(is_top10 = TRUE) %>%
  select(region, haplotype, is_top10)

#Collapse others per region
data_long <- data_long %>%
  left_join(top10_region, by = c("region", "haplotype")) %>%
  mutate(
    haplotype_grp = ifelse(is_top10 %in% TRUE, haplotype, "Others")
  ) %>%
  select(-is_top10)

#Sample level composition
data_sample <- data_long %>%
  group_by(sample_name, region, haplotype_grp) %>%
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
  group_by(sample_name) %>%
  mutate(percent = abundance / sum(abundance) * 100) %>%
  ungroup()

#Region level composition
data_region <- data_sample %>%
  group_by(region, haplotype_grp) %>%
  summarise(
    percent = mean(percent, na.rm = TRUE),
    .groups = "drop")

#Color
haplotypes <- sort(unique(data_long$haplotype_grp))
haplotypes_no_others <- setdiff(haplotypes, "Others")

palette <- c(
  setNames(
    colorRampPalette(brewer.pal(12, "Set3"))(length(haplotypes_no_others)),
    haplotypes_no_others),
  Others = "grey70")


#Plots
#Tropical
ggplot(
  filter(data_region, region == "TRO"),
  aes(x = "", y = percent, fill = haplotype_grp)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = palette) +
  labs(title = "TRO Region", fill = "Haplotype")

#Sample tropical
ggplot(
  filter(data_sample, region == "TRO"),
  aes(x = "", y = percent, fill = haplotype_grp)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ sample_name, ncol = 5) +
  theme_void() +
  scale_fill_manual(values = palette) +
  theme(legend.position = "bottom")

#Subtropical
ggplot(
  filter(data_region, region == "SUB"),
  aes(x = "", y = percent, fill = haplotype_grp)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = palette) +
  labs(title = "SUB Region", fill = "Haplotype")

#Sample Subtropical
ggplot(
  filter(data_sample, region == "SUB"),
  aes(x = "", y = percent, fill = haplotype_grp)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ sample_name, ncol = 5) +
  theme_void() +
  scale_fill_manual(values = palette) +
  theme(legend.position = "bottom")

