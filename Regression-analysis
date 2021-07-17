
#loading the data
jana <- read.csv(file="jana2021_semextracaonova_outlier.csv", sep = ";", header=TRUE, stringsAsFactors = FALSE)
str(jana)
head(jana)
summary(jana)

jana$RGh <- jana$Altura10 - jana$Altura08 / 2


jana <- na.omit(jana)
jana <- jana[jana$RGh > 0.5,]


wd <- 0.48 # wood density 

# biomass following equation in Chave et al. 2014

agb <- function (dbh, h) {
  
  res <- 0.0673 * (0.48 * dbh^2 * h)
  return(res)
}


reformat <- function(){
  
  jana.08 <- jana[,c(1,2,5,8,11)]
  jana.09 <- jana[,c(1,2,6,9,12)]
  jana.10 <- jana[,c(1,2,7,10,13)]
  
  jana.08$ano <- rep("2008", dim(jana.08)[1])
  jana.09$ano <- rep("2009", dim(jana.09)[1])
  jana.10$ano <- rep("2010", dim(jana.10)[1])
  
  names(jana.08) <- c("habitat", "manejo","das", "altura", "dano", "ano")
  names(jana.09) <- c("habitat", "manejo","das", "altura", "dano", "ano")
  names(jana.10) <- c("habitat", "manejo","das", "altura", "dano", "ano")
  
  jana.dyn <- rbind(jana.08, jana.09, jana.10)
  jana.dyn$biomass <- agb(jana.dyn$das, jana.dyn$altura)
  
  return(jana.dyn)
  
}

jana.dyn <- reformat()

summary(jana.dyn)

hist(jana.dyn$altura)

jana.dyn <- jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",]

pl <- ggplot(jana.dyn[jana.dyn$habitat == "Cerrado",], aes(x = das, y = altura))
pl + geom_point(aes(shape = factor(ano), color = factor(ano)), size = 5) + theme_classic() +
  xlab("DAS (cm)") + ylab("Height (m)") + facet_grid(. ~ dano)

# displaying the height x diameter per dano versus year 
pl <- ggplot(jana.dyn[jana.dyn$habitat == "Cerrado",], aes(x = das, y = altura))
pl + geom_point(size = 5) + theme_classic() +
  xlab("das (cm)") + ylab("Height (m)") + facet_grid(ano ~ dano, scales = "free") + labs(title = "Cerrado")

# the same for cerradao
pl <- ggplot(jana.dyn[jana.dyn$habitat == "Cerrad?o",], aes(x = das, y = altura))
pl + geom_point(size = 5) + theme_classic() +
  xlab("das (cm)") + ylab("Height (m)") + facet_grid(ano ~ dano, scales = "free") + labs(title = "Cerrad?o")



library(smatr)


# cerrado
rescerrado <- sma(altura ~ das*dano, log = "xy", data = jana.dyn[jana.dyn$ano == "2008" & jana.dyn$habitat == "Cerrado",])
summary(rescerrado)

#testing for difference in slope between dano inside cerradao and if the slopes differ from 1
res_slope1 <- sma(altura ~ das*dano, log = "xy", slope.test = 1, data = jana.dyn[jana.dyn$ano == "2008" & jana.dyn$habitat == "Cerrado",])
summary(res_slope1)
res_slope1.df <- res_slope1$groupsummary

#ploting the h x das inside cerrado between dano
plot(rescerrado, xlab = "DAS (cm)", ylab = "Height (m)", col = c("gray", "gray", "gray"), type =  "p", pch = 19, lty = c(1,1, 1), lwd = 2)
abline(a = -0.86, b = 1.59, col = "blue", lty = 1, lwd = 2) # dano 1
abline(a = -0.75, b = 1.44, col = "red", lty = 1, lwd = 2) # dano 2
abline(a = -0.13, b = 0.84, col = "green", lty = 1, lwd = 2) # dano 3

legend("bottomright",legend=c("Dano 1", "Dano 2", "Dano 3"),
       lty=c(1,1,1), col= c("blue", "red", "green"),
       ncol=1, bty="n",cex=1)

#cerradao
rescerradao <- sma(altura ~ das*dano, log = "xy", data = jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008" & jana.dyn$habitat == "Cerrad?o",])
summary(rescerradao)

#ploting the h x das inside cerrado between dano
plot(rescerradao, xlab = "DAS (cm)", ylab = "Height (m)", col = c("gray", "gray", "gray"), type =  "p", pch = 19, lty = c(1,1, 1), lwd = 2)
abline(a = -1.58, b = 2.47, col = "blue", lty = 1, lwd = 2) # dano 1
abline(a = -0.36, b = 1.12, col = "red", lty = 1, lwd = 2) # dano 2
abline(a = -0.33, b = 1.05, col = "green", lty = 1, lwd = 2) # dano 3

legend("bottomright",legend=c("Dano 1", "Dano 2", "Dano 3"),
       lty=c(1,1,1), col= c("blue", "red", "green"),
       ncol=1, bty="n",cex=1)


somePDFPath = "C:/Users/Arildo/Documents/R/Cris-Janaguba/graphs_2.pdf"
pdf(file=somePDFPath)

P <-  par(mfrow = c(1,2))

plot(rescerrado, xlab = "DAS (cm)", ylab = "Height (m)", col = c("gray", "gray", "gray"), main = "Cerrado",type =  "p", pch = 19, lty = c(1,1, 1), lwd = 2)
abline(a = -0.86, b = 1.59, col = "blue", lty = 1, lwd = 2) # dano 1
abline(a = -0.75, b = 1.44, col = "red", lty = 1, lwd = 2) # dano 2
abline(a = -0.13, b = 0.84, col = "green", lty = 1, lwd = 2) # dano 3

plot(rescerradao, xlab = "DAS (cm)", ylab = "Height (m)", col = c("gray", "gray", "gray"), main = "Cerrad?o",type =  "p", pch = 19, lty = c(1,1, 1), lwd = 2)
abline(a = -1.58, b = 2.47, col = "blue", lty = 1, lwd = 2) # dano 1
abline(a = -0.36, b = 1.12, col = "red", lty = 1, lwd = 2) # dano 2
abline(a = -0.33, b = 1.05, col = "green", lty = 1, lwd = 2) # dano 3

legend("bottomright",legend=c("Dano 1", "Dano 2", "Dano 3"),
       lty=c(1,1,1), col= c("blue", "red", "green"),
       ncol=1, bty="n",cex=1)

par(P)
dev.off()

# model height in relation to bark harvesting and habitat/light

m1 <- lm(log10(altura) ~ log10(das) + factor(dano) * factor(habitat), data =  jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",])
summary(m1) 

E <- rstandard(m1)
hist(E)
qqnorm(E)
qqline(E)
#Check for independence and homogeneity: residuals
#versus individual explanatory variables
plot(y = E, x = jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",]$das, xlab = "das",
     ylab = "Residuals")
abline(0,0)


# residuals of model interaction habitat vs. dano - m1

someTiffPath = "C:/Users/Arildo/Documents/R/Cris-Janaguba/figures_resubmission/residue_m1.tif"
tiff(file=someTiffPath)

P <-  par(mfrow = c(2,2))
hist(E)
qqnorm(E)
qqline(E)
plot(y = E, x = jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",]$das, xlab = "das",
     ylab = "Residuals")
abline(0,0)


par(P)
dev.off()


# Here model with habitat but using the OLS regression type

m2 <- lm(log10(altura) ~ log10(das) * factor(dano), data =  jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",])
summary(m2) 


# Interaction is not significant so we can refit the model considering only dano. In this case using SMA type regression

# model without log transformation
res <- sma(altura ~ das*dano, log = "", data = jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",])
summary(res)

slope.res <- with(jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",], slope.com(altura, das, dano))

bs_slope.res <- as.data.frame(t(slope.res$bs))


# model with log transformation
res1 <- sma(altura ~ das*dano, log = "xy", data = jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",])
summary(res1)

slope.res1 <- with(jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",], slope.com(log10(altura), log10(das), dano))
slope.res1

bs_slope.res1 <- as.data.frame(t(slope.res1$bs))

# checking the confidence interval overlap between the slopes
ggplot(bs_slope.res,                ### The data frame to use.
       aes(x = c(1,2,3),
           y = slope)) +
  geom_errorbar(aes(ymin = lower.CI.lim,
                    ymax = upper.CI.lim),
                width = 0.05,
                size  = 0.5) +
  geom_point(shape = 15,
             size  = 4) +
  theme_bw()


# comparing the slopes with the predictions from models - MST, stress similarity and geometric similarity

# model height growth in relation to bark harvesting and habitat/light

m3 <- lm(log10(RGh) ~ log10(Altura10) + factor(Dano10) * factor(Habitat), data =  jana)
summary(m3) 

plot(log(jana$Altura10),log(jana$RGh))

E3 <- rstandard(m3)
hist(E3)
qqnorm(E3)
qqline(E3)

#Check for independence and homogeneity: residuals
#versus individual explanatory variables

plot(y = E3, x = jana$Altura10, xlab = "das",
     ylab = "Residuals")
abline(0,0)

# fitting regression for each dano
res_rgh <- sma(RGh ~ Altura10*Dano10, log = "xy", data = jana)
summary(res_rgh)


with(jana, slope.com(log10(Altura10), log10(RGh), Dano10)) # slope test
with(jana, elev.com(log10(Altura10), log10(RGh), Dano10)) # intercept test


# model height growth in relation to bark harvesting

m4 <- lm(log10(RGh) ~ log10(Altura10) * factor(Dano10), data =  jana)
summary(m4)



#######################################
# regression with individuals DAS > 8cm

# model with log transformation
res_das8 <- sma(altura ~ das*dano, log = "xy", data = jana.dyn[jana.dyn$das > 8 & jana.dyn$ano == "2008",])
summary(res_das8)

slope.res_das8 <- with(jana.dyn[jana.dyn$altura > 1 & jana.dyn$ano == "2008",], slope.com(log10(altura), log10(das), dano))
slope.res_das8


###################################################
# regression by DAS classes independent of the DANO

# model with log transformation
# classes 7-10, 10-13 and > 13

classes <- function(){
  
  jana_1 <- jana.dyn[jana.dyn$das <=10 & jana.dyn$ano == "2008",]
  jana_2 <- jana.dyn[jana.dyn$das > 10 & jana.dyn$das <=13 & jana.dyn$ano == "2008",]
  jana_3 <- jana.dyn[jana.dyn$das > 13 & jana.dyn$ano == "2008",]
  
  jana_1$size <- rep("small", dim(jana_1)[1])
  jana_2$size <- rep("medium", dim(jana_2)[1])
  jana_3$size <- rep("tall", dim(jana_3)[1])
  
  #names(jana.08) <- c("habitat", "manejo","das", "altura", "dano", "ano")
  #names(jana.09) <- c("habitat", "manejo","das", "altura", "dano", "ano")
  #names(jana.10) <- c("habitat", "manejo","das", "altura", "dano", "ano")
  
  jana_classes <- rbind(jana_1, jana_2, jana_3)
  
  names(jana_classes) <- c("habitat", "manejo","das", "altura", "dano", "ano", "biomass", "size")
  
  
  return(jana_classes)
  
}

jana_classes <- classes()

# model with log transformation
res_classes <- sma(altura ~ das*size, log = "xy", data = jana_classes)
summary(res_classes)

slope.res_classes <- with(jana_classes, slope.com(log10(altura), log10(das), size))
slope.res_classes


summary(jana_classes)


# model with interaction size and dano but OLs regression

summary(lm(log10(altura) ~ log10(das)*dano, data = jana_classes))

