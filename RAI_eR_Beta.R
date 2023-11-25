##################################################
# RAI_eR:an R package to calculate relative abundance index (RAI) and encounter rates (eR) from camera traps
# Development by: Salvador Mandujano R
# Last modification: November 16, 2023
##################################################

##################################################
# Function of general RAIeR model
##################################################

RAIeR_gral <- function(new.mat) {
  
  cameras <- with(new.mat, tapply(Camera, Species, length)) 
  days <- with(new.mat, tapply(Effort, Species, sum)) 
  n <- with(new.mat, tapply(Events, Species, sum))    
  RAIeR_gral <- round(n/days*100, 2) 
  
  # Naive occupation:
  occ <- subset(new.mat, new.mat$Events > 0)
  OccNaive <- as.data.frame(round(table(occ$Species)/cameras,2))
  
  # Relative percent (RAIeR_pct)
  RAIeR_pct <- with(new.mat, round(n/sum(Events)*100,2))
  
  # Results:
  table1 <- cbind(cameras, days, n, RAIeR_gral, RAIeR_pct, OccNaive = OccNaive[,2])
  table1 <- table1[order(RAIeR_gral),]
  View(table1)
  write.csv(table1, "Results/Table_RAIeR_gral.csv") 
  
  # RAIeR-naive occupation graph:
  jpeg(filename = "Results/Occ_RAIeR.jpg", width = 7000, height = 5000, units = "px", res = 1200)
    r2 <- round(cor(RAIeR_gral, OccNaive[,2]),2)
    plot(OccNaive[,2], RAIeR_gral, xlab= "Distribution naive", ylab = "RAIeR", main = paste("r2 =", r2), frame.plot = F, las = 1, pch = 16, col = "skyblue", cex = 2)
    abline(lm(RAIeR_gral ~ OccNaive[,2]), col = "red", lwd = 2)
    text(OccNaive[,2], RAIeR_gral, OccNaive[,1], cex = 0.5)
    dev.off()
  
  # ------
  r2 <- round(cor(RAIeR_gral, OccNaive[,2]),2)
  plot(OccNaive[,2], RAIeR_gral, xlab= "Distribution naive", ylab = "RAIeR", main = paste("r2 =", r2), frame.plot = F, las = 1, pch = 16, col = "skyblue", cex = 2)
  abline(lm(RAIeR_gral ~ OccNaive[,2]), col = "red", lwd = 2)
  text(OccNaive[,2], RAIeR_gral, OccNaive[,1], cex = 0.5)
  
} # end function


##################################################
# Function of RAIeR per camera traps model
##################################################

RAIeR_ct <- function(new.mat, Ymax) {
  
  require(akima)
  require(MASS)
  require(RColorBrewer)
  
  RAIeRalt <- with(new.mat, round((Events/Effort)*100, 2))
  table2 <- cbind(new.mat, RAIeRalt)
  View(table2)
  write.csv(table2, "Results/Table_RAIeR_ct.csv")

  # Boxplot graph:
  jpeg(filename = "Results/RAIeR_ct.jpg", width = 7000, height = 5000, units = "px", res = 1200)
  par(mfcol = c(1,1), mar = c(5,5,5,5))
  boxplot(RAIeRalt ~ Species, data = new.mat, ylab = "RAIeR", ylim = c(0, Ymax), xlab = "", varwidth = F, outline = F, cex = 3.4, las = 2, frame.plot = F, cex.axis = 0.7, col = "skyblue")
  dev.off()
  
  # --------
  par(mfcol = c(1,1), mar = c(5,5,5,5))
  boxplot(RAIeRalt ~ Species, data = new.mat, ylab = "RAIeR", ylim = c(0, Ymax), xlab = "", varwidth = F, outline = F, cex = 3.4, las = 2, frame.plot = F, cex.axis = 0.7, col = "skyblue")
  
# interpolation:
  RAIeR_mean <- with(table2, round(tapply(RAIeRalt, Species, mean),2))
  RAIeR_sd <- with(table2, round(tapply(RAIeRalt, Species, sd),2))
  occ <- subset(table2, table2$Events > 0)
  cameras <- with(new.mat, tapply(Camera, Species, length))
  n <- with(new.mat, tapply(Events, Species, sum)) 
  OccNaive <- round(table(occ$Species)/cameras,2)
  
  g <- interp(RAIeR_mean, OccNaive, n, duplicate = T)
  
  jpeg(filename= "Results/Interpolation.jpg", width = 7000, height = 5000, units = "px", res = 1200)
  par(mfrow = c(1,1))
  image(g, col= topo.colors(12), cex.lab = 1.0, frame = F, xlab = "RAIeR", ylab = "Naive occupation", xlim = c(0, max(RAIeR_mean + 2)), ylim = c(0, 1.1))
  contour(g, add = T)
  text(RAIeR_mean, jitter(OccNaive,3), labels = unique(sort(table2$Species)), cex = 1, pos = 4, col = "red")
  dev.off()

} # end function

  
##################################################
# Function to statistical tests
##################################################

  RAIeR_test <- function(Ymax) {
    
    require(xtable)
    require(agricolae)
    
    table2 <- read.csv("Results/Table_RAIeR_ct.csv", header = T)
    RAIeRaov <- aov(RAIeRalt ~ Species-1, data = table2)
    cat("----- \n RAIeR comparasion among species \n")
    print(summary(RAIeRaov))
    mod <- summary(RAIeRaov)
    anova <- xtable(mod)
    View(anova)
    write.csv(anova, "Results/Table_ANOVA.csv")
    
    # Posterior comparison:
    P <- as.numeric(unlist(summary.aov(RAIeRaov)[[1]][5]))[1]
    ifelse(P < 0.05, { # if P significative then:
      
    # Tukey test:
      jpeg(filename = "Results/Tukey_test.jpg", width = 7000, height = 10000, units = "px", res = 1200)
      par(mfcol = c(1,1))
      plot(TukeyHSD(RAIeRaov), cex.axis = 0.5, las = 1)
      dev.off()
      
    # ---------
      par(mfcol = c(1,1))
      plot(TukeyHSD(RAIeRaov), cex.axis = 0.5, las = 1)
     
    # HSD test:
      out_1 <- HSD.test(RAIeRaov, "Species")
      out2 <- out_1$groups
      
      jpeg(filename = "Results/HSD_test.jpg", width = 7000, height = 5000, units = "px", res = 1200)
      par(mfcol = c(1,1))
      bar.group(out_1$groups, horiz = F, las = 2, cex.main = 2, font = 3, cex.axis = 0.9, plot = T, col = "lightblue", ylim = c(0, Ymax),   names.arg = out_1$trt, ylab = "RAIeR")
      dev.off()
      
      # ---------
      par(mfcol = c(1,1))
      bar.group(out_1$groups, horiz = F, las = 2, cex.main = 2, font = 3, cex.axis = 0.9, plot = T, col = "lightblue", ylim = c(0, Ymax),   names.arg = out_1$trt, ylab = "RAIeR")
      
    }, NA) # if P not significant
    
} # end function

##################################################
# Function to create tables of Species-RAIeR, and Species-Events
##################################################

  RAIeR_matx <- function() {
    
    require(dplyr)
    require(tidyr)
    
    table2 <- read.csv("Results/Table_RAIeR_ct.csv", header = T)
    
    # matrix RAIeR per Species:
    SppRAIeR <- table2 %>% dplyr::select(Camera, Species, RAIeRalt)
    SppRAIeR <- arrange(SppRAIeR, Species)
    Spp_RAIeR <- pivot_wider(SppRAIeR, names_from = "Species", values_from =  "RAIeRalt")
    View(Spp_RAIeR)
    write.csv(Spp_RAIeR, "Results/Matrix_Spp-RAIeR.csv")   
    
    # matrix Events per Species:
    SppEvents <- table2 %>% dplyr::select(Camera, Species, Events)
    SppEvents <- arrange(SppEvents, Species)
    Spp_Event <- pivot_wider(SppEvents, names_from = "Species", values_from =  "Events")
    View(Spp_Event)
    write.csv(Spp_Event, "Results/Matrix_Spp-Events.csv") 
    
} # end function

##################################################
# Function RAIeR GLM-Poisson regression
##################################################

RAIeR_glm <- function(new.mat, family) {
  RAIeRglm <- glm(Events ~ Species-1, data = new.mat, offset = log(Effort), family = family)
  cat("----- \n GLM-Poisson: \n")
  print(summary(RAIeRglm))
  modglm <- summary(RAIeRglm)
  
  cat("----- \n Estimation per species: \n")
  RAIeRpoisson <- cbind(RAIeRpoisson = round(exp(RAIeRglm$coefficients)*100, 2))
  RAIeRpoisson <- RAIeRpoisson[order(RAIeRpoisson),]
  View(RAIeRpoisson)
  
  require(xtable)
  table_glm <- xtable(modglm)
  mlg <- cbind(table_glm, RAIeRpoisson)
  write.csv(mlg, "Results/Table_RAIeR_GLM.csv")
  
} # end function

##################################################
# Function to create final Table
##################################################

RAIeR_final <- function(new.mat) {
  
  t1 <- read.csv("Results/Table_RAIeR_gral.csv", header = T)
  t2 <- read.csv("Results/Table_RAIeR_ct.csv", header = T)
  t3 <- read.csv("Results/Table_RAIeR_GLM.csv", header = T)
  
  RAIeR_ct.Mean <-  with(t2, round(tapply(RAIeRalt, Species, mean), 2))
  RAIeR_ct.SD <- with(t2, round(tapply(RAIeRalt, Species, sd), 2))
  alt <- cbind(RAIeR_ct.Mean, RAIeR_ct.SD)
  alt2 <- alt[order(RAIeR_ct.Mean),]
  
  table3 <- with(c(t1,t3), cbind(cameras, days, n, RAIeR_gral, alt2, RAIeRpoisson, RAIeR_pct, OccNaive))
  #table3 <- table3[order(RAIeR_gral),]
  View(table3)
  write.csv(table3, "Results/Table_Final.csv") 
  
} # end function

##################################################
# Function to GLM-Poisson for selected species and covariates
##################################################

RAIeR_glmSp <- function(Species, covs, family) {
  
  require(tidyverse)
  require(xtable)
  
  # Select species:
  new.mat <- read.csv("Results/Table_RAIeR_ct.csv", header = T)
  Sp <- new.mat[new.mat$Species== Species,]
  SpCovs <- Sp %>% dplyr::select(covs, Species, Events, Effort, RAIeRalt)
  View(SpCovs)
  
  # Models:
  if (length(covs)==1) {  
    glm1 <- glm(Events ~ SpCovs[,1], offset = log(Effort), family, data = SpCovs)
    cat("----------------------------")
    cat("----- \n RAIeR GLM-Poisson with covariates: \n")
    print(summary(glm1))
    cat("----------------------------")
    cat("----- \n RAIeR estimates: \n")
    print(exp(glm1$coefficients)*100)
    table_glm <- xtable(glm1)
    View(table_glm)
    write.csv(table_glm, "Results/Table_GLM_Sp_1covs.csv")
    
    jpeg(filename = "Results/GLM_Sp_1covs.jpg", width = 7000, height = 5000, units = "px", res = 1200)
    par(mfrow = c(1,1), mar = c(5,5,1,1))
    plot(RAIeRalt ~ SpCovs[,1], data = Sp, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[1]), main = unique(SpCovs$Species))
    dev.off()
    
    # -----------
    par(mfrow = c(1,1), mar = c(5,5,1,1))
    plot(RAIeRalt ~ SpCovs[,1], data = Sp, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[1]), main = unique(SpCovs$Species))
  }
  if (length(covs)==2) {  
    glm2 <- glm(Events ~ SpCovs[,1] + SpCovs[,2], offset = log(Effort), family, data = SpCovs)
    summary(glm2)
    cat("----- \n RAIeR GLM-Poisson with covariates: \n")
    print(summary(glm2))
    cat("----- \n RAIeR estimates: \n")
    print(exp(glm2$coefficients)*100)
    table_glm <- xtable(glm2)
    View(table_glm)
    write.csv(table_glm, "Results/Table_GLM_Sp_2covs.csv")
    
    jpeg(filename = "Results/GLM_Sp_2covs.jpg", width = 7000, height = 5000, units = "px", res = 1200)
    par(mfrow = c(1,2), mar = c(5,5,1,1))
    plot(RAIeRalt ~ SpCovs[,1], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(Sp[1]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,2], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[2]), main = unique(SpCovs$Species))
    dev.off()
    
    # -----------
    plot(RAIeRalt ~ SpCovs[,1], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(Sp[1]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,2], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[2]), main = unique(SpCovs$Species))
  }
  
  if (length(covs)==3)  {  
    glm3 <- glm(Events ~ SpCovs[,1] + SpCovs[,2] + SpCovs[,3], offset = log(Effort), family, data = SpCovs)
    cat("----- \n RAIeR GLM-Poisson with covariates: \n")
    print(summary(glm3))
    cat("----- \n RAIeR estimates: \n")
    print(exp(glm3$coefficients)*100)
    table_glm <- xtable(glm3)
    View(table_glm)
    write.csv(table_glm, "Results/Table_GLM_Sp_3covs.csv")
    
    jpeg(filename = "Results/GLM_Sp_3covs.jpg", width = 7000, height = 5000, units = "px", res = 1200)
    par(mfrow = c(2,2), mar = c(5,5,1,1))
    plot(RAIeRalt ~ SpCovs[,1], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[1]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,2], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[2]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,3], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[3]), main = unique(SpCovs$Species))
    dev.off()
    
    # -----------
    par(mfrow = c(2,2), mar = c(5,5,1,1))
    plot(RAIeRalt ~ SpCovs[,1], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[1]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,2], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[2]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,3], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[3]), main = unique(SpCovs$Species))
  }
  
  if (length(covs)==4)  {  
    glm4 <- glm(Events ~ SpCovs[,1] + SpCovs[,2] + SpCovs[,3] + SpCovs[,4], offset = log(Effort), family, data = SpCovs)
    cat("----- \n RAIeR GLM-Poisson with covariates: \n")
    print(summary(glm4))
    cat("----- \n RAIeR estimates: \n")
    print(exp(glm4$coefficients)*100)
    table_glm <- xtable(glm4)
    View(table_glm)
    write.csv(table_glm, "Results/Table_GLM_Sp_4covs.csv")
    
    jpeg(filename = "Results/GLM_Sp_4covs.jpg", width = 7000, height = 5000, units = "px", res = 1200)
    par(mfrow = c(2,2), mar = c(3,3,1,1))
    plot(RAIeRalt ~ SpCovs[,1], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[1]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,2], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[2]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,3], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[3]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,4], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[4]), main = unique(SpCovs$Species))
    dev.off()
    
    # -----------
    par(mfrow = c(2,2), mar = c(3,3,1,1))
    plot(RAIeRalt ~ SpCovs[,1], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[1]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,2], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[2]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,3], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[3]), main = unique(SpCovs$Species))
    plot(RAIeRalt ~ SpCovs[,4], data = SpCovs, frame.plot = F, col = "skyblue", pch = 16, xlab = names(SpCovs[4]), main = unique(SpCovs$Species))
  }
  
} # end function

#####################
# MAPS
##################################################
# Function for graph events per CT for selected species
##################################################

Event_CT <- function(new.mat, especie, parRow, parCol, pointSize, labels) {
  require(stringr)
  
  jpeg(filename= "Results/Event_CT.jpg", width= 7000, height= 5000, units= "px", res=1200)
  
  par(mfcol = c(parRow, parCol), mar = c(3,1,1,1))
  
  for (i in 1:length(especie)) {
    sp  <- subset(new.mat, Species == especie[i])
    
    plot(sp$X, sp$Y, xlab="", ylab="", frame.plot = F, cex.axis = 1,  main = unique(sp$Species), type = "n", labels = labels, cex.main = 0.9) 
    points(sp$X, sp$Y, pch = 16, col = "skyblue", cex = sp$Events*pointSize)
    text(sp$X, sp$Y, sp$Events, cex = 0.7) 
  }
  dev.off()
  
  # ------------------
  par(mfcol = c(parRow, parCol), mar = c(3,1,1,1))
  
  for (i in 1:length(especie)) {
    sp  <- subset(new.mat, Species == especie[i])
    
    plot(sp$X, sp$Y, xlab="", ylab="", frame.plot = F, cex.axis = 1,  main = unique(sp$Species), type = "n", labels = labels, cex.main = 0.9) 
    points(sp$X, sp$Y, pch = 16, col = "skyblue", cex = sp$Events*pointSize)
    text(sp$X, sp$Y, sp$Events, cex = 0.7) 
  }
  
} # end function

##################################################
# Function to project the Study area and CTs
##################################################

Map_CTs <- function(map, my_colors) {
  
  require(raster)
  require(prettymapr)
  
  jpeg(filename = "Results/My_MapCTs.jpg", width = 8000, height = 7000, units = "px", res = 1200)
  
  par(mfrow=c(1,1), mar = c(1,1,1,1))
  plot(map, col = my_colors, fill = T, lty = 0)
  points(habitat.data$X, habitat.data$Y, pch = 16, cex = 1.5, col = "black")
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()
  legend("bottomleft", map$tipo_Suelo, col = my_colors, pch = 15, cex = 0.7, pt.cex = 1.5)
  dev.off() 
  
  # -------------
  par(mfrow=c(1,1), mar = c(1,1,1,1))
  plot(map, col = my_colors, fill = T, lty = 0)
  points(habitat.data$X, habitat.data$Y, pch = 16, cex = 1.5, col = "black")
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()

} # end function

##################################################
# Function to project CTs on Esri.WorldImagery
##################################################

Esri_CTs <- function(CT_table, proje2) {
  
  require(leaflet)
  
  CT_points <- SpatialPoints(cbind(habitat.data$X, habitat.data$Y), proj4string = proje2) 
  
  CT_points <- spTransform(CT_points, "+proj=longlat +datum=WGS84")
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Base") %>% 
    addCircleMarkers(lng = sp::coordinates(CT_points)[,1], lat = sp::coordinates(CT_points)[,2], popup = paste(CT_table$Camera), color = "skyblue", weight = 1, radius = 7, opacity = 0.5, fill = T,  fillOpacity = 0.75) %>%
    addLayersControl(baseGroups = c("Satellite", "Base"), options = layersControlOptions(collapsed = FALSE))
  
} # end function


##################################################
# Function to map the RAI of selected species
##################################################

RAIeR_mapSp <- function(species, pointSize, mycolors) { 
  
  require(stringr)
  require(prettymapr)
  
  new.mat <- read.csv("Results/Table_RAIeR_ct.csv", header = T)
  sp  <- subset(new.mat, Species == species)
  
  jpeg(filename= paste("Results/Map_", str_to_title(species), ".jpg", sep = ""), width= 7000, height= 5000, units= "px", res=1200)
  par(mfrow = c(1,1), mar = c(1,1,1,1))
  plot(map, col = mycolors, fill = T, lty = 0, main = unique(sp$Species))
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()
  points(sp$X, sp$Y, pch = 16, col = "#00000170", cex = sp$RAIeRalt*pointSize)
  text(sp$X, sp$Y, sp$RAIeRalt, cex = 0.3) 
  dev.off() 
  
  # ----------------
  par(mfrow = c(1,1), mar = c(1,1,1,1))
  plot(map, col = mycolors, fill = T, lty = 0, main = unique(sp$Species))
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()
  points(sp$X, sp$Y, pch = 16, col = "#00000170", cex = sp$RAIeRalt*pointSize)
  text(sp$X, sp$Y, sp$RAIeRalt, cex = 0.3) 
  
} # end function


##################################################
# Function to project RAIeR on Esri.WorldImagery
##################################################

Esri_RAIeR <- function(species, proje2, pointsize) {
  
  require(leaflet)
  
  new.mat <- read.csv("Results/Table_RAIeR_ct.csv", header = T)
  sp  <- subset(new.mat, Species == species)
  print(sp)
  
  CT_points <- SpatialPoints(cbind(sp$X, sp$Y), proj4string = proje2) 
  CT_points <- spTransform(CT_points, "+proj=longlat +datum=WGS84")
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Base") %>% 
    addCircleMarkers(lng = sp::coordinates(CT_points)[,1], lat = sp::coordinates(CT_points)[,2], popup = paste(sp$RAIeRalt), color = "skyblue", weight = 1, radius = sp$RAIeRalt*pointsize, opacity = 0.5, fill = T,  fillOpacity = 1) %>%
    addLayersControl(baseGroups = c("Satellite", "Base"), options = layersControlOptions(collapsed = FALSE))

} # end function


##################################################
# Function to kernel utilization distribution for selected species
##################################################

RAIeR_kernelSp <- function(species, S, HR, pointSize, proje, mycolors) { 
  
  require(stringr)
  require(prettymapr)
  require(raster)
  require(prettymapr)
  require(adehabitatHR)
  require(dplyr)
  
  # Read RAIeR table:
  
  new.mat <- read.csv("Results/Table_RAIeR_ct.csv", header = T)
  new.mat  <- subset(new.mat, Species == species)
  sp <- dplyr::select(new.mat, c("X", "Y", "Events"))
  
  # Preparation of data:
  
  n <- length(sp$Events)
  nueva_mat <- vector("numeric", n)
  
  repetir <- function(i) {
    do.call("rbind", replicate(sp$Events[i], sp[i,], simplify = F))
  }
  
  for (i in 1:n) {
    nuevo <- repetir(i)
    nueva_mat <- rbind(nueva_mat, nuevo)
  }
  
  nueva_mat
  (species_coord <- nueva_mat[-1,-3])
  
  presenciaSP <- SpatialPoints(coords = species_coord)
  projection(presenciaSP) <- proje 
  
  # Kernel analysis using adehabitatHR package: 
  
  S <- S # Extent
  HR <- HR # Home range
  
  (speciesUD <- kernelUD(presenciaSP, h = "href", kern = "bivnorm", grid = 95, hlim = c(0.75, 0.75), extent = S))
  (speciesHR <- getverticeshr(speciesUD, percent = HR))
  
  par(mfrow = c(1,1), mar = c(1,1,1,1))
  
  jpeg(filename= paste("Results/Kernel_", str_to_title(species), ".jpg", sep = ""), width= 8000, height= 7000, units= "px", res=1200)
  mi_mapaL <- as(map, "SpatialLinesDataFrame")
  mi_mapaL <- spTransform(mi_mapaL, proje)
  image(speciesUD, col = mycolors, interpolate = F, cex.main = 1, main = paste("\n \n Species =", unique(new.mat$Species), "\n Utilization distribution of ", HR, "% = ", round(speciesHR$area, 1), "ha"), cex.main = 1.2)
  plot(mi_mapaL, add = T, col = "gray")
  points(sp$X, sp$Y, pch = 16, col = rgb(1, 0, 0, 0.75), cex = sp$RAIalt/pointSize)
  plot(speciesHR, add = T)
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()
  dev.off()
  
  # ----------------
  mi_mapaL <- as(map, "SpatialLinesDataFrame")
  mi_mapaL <- spTransform(mi_mapaL, proje)
  image(speciesUD, col = mycolors, interpolate = F, cex.main = 1, main = paste("\n \n Species =", unique(new.mat$Species), "\n Utilization distribution of ", HR, "% = ", round(speciesHR$area, 1), "ha"), cex.main = 1.2)
  plot(mi_mapaL, add = T, col = "gray")
  points(sp$X, sp$Y, pch = 16, col = rgb(1, 0, 0, 0.75), cex = sp$RAIalt/pointSize)
  plot(speciesHR, add = T)
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()
  
  # Create a Raster layer:
  
  Kernel_capa <- cbind(speciesUD@coords, speciesUD@data)
  kernel_data <- as.data.frame(Kernel_capa)
  coordinates(kernel_data) <- ~Var2 + Var1
  r <- raster(ncol = 60, nrow = 30)
  extent(r) <- extent(mi_mapaL)
  vals <- kernel_data$ud
  kernel_rast <- rasterize(kernel_data, r, vals)
  
  jpeg(filename = paste("Results/KernelRaster_", str_to_title(species), ".jpg", sep = ""), width= 8000, height= 7000, units= "px", res=1200)
  plot(kernel_rast)
  dev.off()
  
  writeRaster(kernel_rast, paste("Results/KernelRaster_", str_to_title(species), ".tiff", sep = ""), overwrite = T)
  
} # end function

##################################################
# Function to create a camera-trap grid
##################################################

miGrid <- function(n1, n2, CTx, CTy, CTdist, proje) {
  
  require(raster)
  require(prettymapr)
  
  # To create grid:
  jpeg(filename = "Results/CT_grid.jpg", width = 8000, height = 7000, units = "px", res = 1200)
  par(mfrow = c(1,1), mar = c(1,1,1,1))
  plot(map, col = my_colors, fill = T, lty = 0)
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()
  
  # To create the grid:
  x <- seq(from = CTx, to = (CTx + n1*CTdist-CTdist), by = CTdist)
  y <- seq(from = CTy, to = (CTy + n2*CTdist-CTdist), by = CTdist)
  xy <- expand.grid(x = x, y = y)
  points(xy, col = "black", pch = 16, cex = 1.5)
  n <- as.character(1:(n1*n2))
  text(xy, n, col = "white", cex = 0.5)
  cat("-------- \n UTMs \n")
  View(xy)
  write.csv(xy, "Results/UTMs_grid.csv")
  
  # To create buffer around the grid:
  
  xySP <- SpatialPoints(coords = xy)
  projection(xySP) <- proje
  class(xySP)
  bbox(xySP)
  xSP <- seq(from = bbox(xySP)[1]-CTdist, to = bbox(xySP)[3]+CTdist, by = n1*CTdist+CTdist)
  ySP <- seq(from = bbox(xySP)[2]-CTdist, to = bbox(xySP)[4]+CTdist, by = n2*CTdist+CTdist)
  xy2 <- expand.grid(x = xSP, y = ySP)
  points(xy2, col = "black", bg = "red", pch = 22, cex = 1)
  buf <- rbind(xy2[1,], xy2[2,], xy2[4,], xy2[3,], xy2[1,])
  lines(buf, add = T, col = "red", lty = 1, lwd = 3)
  
  # To estimate area:
  
  cat("---------- \n Grid hectares = \n ") 
  print(S_grid1 <- (((n1-1)*CTdist) * ((n2-1)*CTdist))/10000)
  
  cat("------------- \n Grid + buffer = \n ")
  print(S_grid2 <- (((n1+1)*CTdist) * ((n2+1)*CTdist))/10000)
  
  legend("bottomleft", c(paste("Grid of ", (n1*n2), "camera-traps,", "at", CTdist, "meters,"), paste("Grid area = ", S_grid1, "hectares,"), paste("and grid + buffer =", S_grid2, "hectares (red quadrant).")), cex = 0.8)
  dev.off()
  
  # -------------
  par(mfrow = c(1,1), mar = c(1,1,1,1))
  plot(map, col = my_colors, fill = T, lty = 0)
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()
  x <- seq(from = CTx, to = (CTx + n1*CTdist-CTdist), by = CTdist)
  y <- seq(from = CTy, to = (CTy + n2*CTdist-CTdist), by = CTdist)
  xy <- expand.grid(x = x, y = y)
  points(xy, col = "black", pch = 16, cex = 1.5)
  n <- as.character(1:(n1*n2))
  text(xy, n, col = "white", cex = 0.5)
  xySP <- SpatialPoints(coords = xy)
  projection(xySP) <- proje
  xSP <- seq(from = bbox(xySP)[1]-CTdist, to = bbox(xySP)[3]+CTdist, by = n1*CTdist+CTdist)
  ySP <- seq(from = bbox(xySP)[2]-CTdist, to = bbox(xySP)[4]+CTdist, by = n2*CTdist+CTdist)
  xy2 <- expand.grid(x = xSP, y = ySP)
  points(xy2, col = "black", bg = "red", pch = 22, cex = 1)
  buf <- rbind(xy2[1,], xy2[2,], xy2[4,], xy2[3,], xy2[1,])
  lines(buf, add = T, col = "red", lty = 1, lwd = 3)
  legend("bottomleft", c(paste("Grid of ", (n1*n2), "camera-traps,", "at", CTdist, "meters,"), paste("Grid area = ", S_grid1, "hectares,"), paste("and grid + buffer =", S_grid2, "hectares (red quadrant).")), cex = 0.8)
  
} # end function

##################################################
# Function to format the .csv data generated from camtrapR (DEVELOPING)
##################################################

###dataFormat <- function(data_Spp, data_CT) {

###require(camtrapR)

#-----------------------------
# Use the function surveyReport and modified the parameters setupCol  and retrivelCol:

###report <- surveyReport (recordTable = data_Spp, CTtable = data_CT, speciesCol = "Species", stationCol = "Station", setupCol = "Fecha_colocacion", retrievalCol = "Fecha_retiro", CTDateFormat = "%d/%m/%Y", recordDateTimeCol = "DateTimeOriginal", recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", CTHasProblems = T)

###sampling_effort <- report[[1]]

###days <- sampling_effort[c("Station", "n_nights_active")] 

###species <- report[[5]]
###head(species)

#-----------------------------
# Merge data.frame:
###wildlife.data <- merge(species, days, all.y = T)

#-----------------------------
# Rename columns: 

###require(dplyr)
###wildlife.data <- wildlife.data %>%
###rename(Camera= Station, Events= n_events, Effort= n_nights_active)

#-----------------------------
# Species names abbreviation:

###require(fuzzySim)
###wildlife.data$Species <- spCodes(wildlife.data$Species, sep.spcode = "_")

#-----------------------------
# Save the formated data:
###write.csv(wildlife.data, "wildlife_data.csv")
###}

##################################################
# Function to select a subset of data (IN DEVELOPMENT)

###Select_sp(species = "Odo_vir", site = "San_Pedro_Chico", season = "Dry", year = NULL)

#################################################
# END
