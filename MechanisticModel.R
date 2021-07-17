

# Here we simulate plant height growth trajectory through time using emperical data from bark regeneration rates of trees at field. We replace these values of bark
regeneration rates in the mechanistic model through the parameter bark turnover and simulate trees growing in two different light regimes (canopy openness).

###################################################
######Plant physiological model
##################################################



install.packages("remotes")

remotes::install_github("smbache/loggr", dependencies=TRUE)
remotes::install_github("richfitz/RcppR6", dependencies=TRUE)

install.packages("rlang", type = "source") #error rlang was not being installed than i used this command

devtools::install_github("traitecoevo/plant", dependencies=TRUE)

####################################
######Simulating a plant strategy
####################################
remotes::install_github("traitecoevo/plant", dependencies=TRUE)


library(plant)
library(deSolve)

pl <- FF16_Plant()

env <- fixed_environment(1.0)


pp <- FF16_PlantPlus(pl$strategy)

derivs <- function(t, y, plant, env){
  plant$ode_state <- y
  plant$compute_vars_phys(env)
  list(plant$ode_rates)
}
pl <- FF16_Plant()
tt <- seq(0, 50, length.out=101)
y0 <- setNames(pl$ode_state, pl$ode_names)
yy <- deSolve::lsoda(y0, tt, derivs, pl, env=env)
plot(height ~ time, yy, type="l")


params <- scm_base_parameters("FF16")

s1 <- strategy(trait_matrix(0.153,  "k_b"), params)# lower turnover - higher regeneration rate
s2 <- strategy(trait_matrix(0.201,  "k_b"), params)# higher turnover - lower regeneration rate

indv1 <- FF16_Individual(s1)
indv2 <- FF16_Individual(s2)


indv1$set_state("height", 2)
indv1$state("height")


indv2$set_state("height", 2)
indv2$state("height")

heights1 <- seq(indv1$state("height"), 10, length.out=100L)
heights2 <- seq(indv2$state("height"), 10, length.out=100L)

env <- FF16_fixed_environment(0.3)

dat1 <- grow_plant_to_height(indv1, heights1, env,
                             time_max=50, warn=FALSE, filter=TRUE)
dat2 <- grow_plant_to_height(indv2, heights2, env,
                             time_max=50, warn=FALSE, filter=TRUE)

plot(dat1$trajectory[, "time"], dat1$trajectory[, "height"],
     type="l", lty=1,
     las=1, xlab="Time (years)", ylab="Height (m)")
lines(dat2$trajectory[, "time"], dat2$trajectory[, "height"], lty=2)
legend("bottomright", c("Low Bark Turnover", "High Bark Turnover"), lty=1:2, bty="n")




env_low <- FF16_fixed_environment(0.26)
dat1_low <- grow_plant_to_height(indv1, heights1, env_low,
                                 time_max=50, warn=FALSE, filter=TRUE)
dat2_low <- grow_plant_to_height(indv2, heights2, env_low,
                                 time_max=50, warn=FALSE, filter=TRUE)

cols <- c("black", "gray")
plot(dat1$trajectory[, "time"], dat1$trajectory[, "height"],
     type="l", lty=1,
     las=1, xlab="Time (years)", ylab="Height (m)")
lines(dat2$trajectory[, "time"], dat2$trajectory[, "height"], lty=2)
lines(dat1_low$trajectory[, "time"], dat1_low$trajectory[, "height"],
      lty=1, col=cols[[2]])
lines(dat2_low$trajectory[, "time"], dat2_low$trajectory[, "height"],
      lty=2, col=cols[[2]])
legend("bottomright",
       c("High light", "Low light"), lty=1, col=cols,
       bty="n")


f <- function(x) x$rate("height")
dhdt1 <- sapply(dat1$plant, f)
dhdt2 <- sapply(dat2$plant, f)
dhdt1_low <- sapply(dat1_low$plant, f)
dhdt2_low <- sapply(dat2_low$plant, f)


someTiffPath = "C:/Users/Arildo/Documents/R/Cris-Janaguba/figures_resubmission/figure4.tif"

tiff(filename=someTiffPath, width = 5, height = 5, units = "in",  pointsize = 8,  bg = "white",res=300) 

plot(dat1$time, dhdt1, type="l", lty=1,
     las=1, xlab="Time (years)", ylab="Height growth rate (m / yr)")
lines(dat2$time, dhdt2, lty=2)
lines(dat1_low$time, dhdt1_low, lty=1, col=cols[[2]])
lines(dat2_low$time, dhdt2_low, lty=2, col=cols[[2]])
legend("topright",
       c("High light", "Low light", "Bark turnover - 0.201", "Bark turnover - 0.164"), lty=c(NA, NA, 2, 1),
       pch =c(15, 15, NA, NA), col=c(cols, "black", "black"),
       bty="n")

dev.off()

