

#####################
# Bayesian analysis to test allometric coefficient of diameter vs. height considering the predictions of different theorethical models (e.g., Metabolic Scaling Theory)

#installing and loading packages
install.packages("R2jags")
install.packages("R2OpenBUGS")
install.packages("MCMCvis")

library(R2OpenBUGS)
library(R2jags)
library(coda)
library(MCMCvis)

#dataset
jana_bay <- jana.dyn[, c("altura", "das")]


# setup model

mod = function(){
  #priors
  a ~ dnorm(0.0, 1.0E-4)
  b ~ dnorm(0.0, 1.0E-4)
  
  
  tau ~ dgamma(1.0E-3, 1.0E-3)
  sigma <- 1.0/sqrt(tau)
  
  #likelihood
  for (i in 1:278) {
    mu[i] <- a + b * das[i]
    altura[i] ~ dnorm(mu[i], tau)
    altura_pred[i] ~ dnorm(mu[i], tau)
  }
}

# write model
model.file = "model.txt"
write.model(mod, model.file)

# no initial values
inits <- NULL


# what parameters we want to track

params <- c("tau", "a", "b", "altura_pred")

# hyperparameters
# number of iterations
ni = 10000

# burn interval
nb = 1000

# thining interval 
nt = 1

# number of chains
nc = 3

# we'll compile the model as a "jags.model", then use "update" to iterate through the burn-in interval. 
# Lastly, we'll use "coda.samples" to get samples from the posterior distributions of our parameters using MCMC.

# compile model
jmod = jags.model(file = model.file, data = jana_bay, n.chains = nc, inits = inits, n.adapt = 1000)

# iterate through jmod for the extent of the burn-in
update(jmod, n.iter = nb, by = 1)

# draw samples from the posterior for params, given MCMC hyperparameters
post = coda.samples(jmod, params, n.iter = ni, thin = nt)

# That concludes the MCMC process, we'll now assess convergence and look at the results. 

# diagnostic evaluation of posterior samples
MCMCtrace(post, params = c("a", "b"), pdf = F)

#objectively assess convergence with gelmans diagnostic
gelman.diag(post) # we want the multivariate potential scale reduction factor (PSRF) to be around 1.

# get summary of posterior samples for two parameters
MCMCsummary(post, params = c("b", "a"), digits = 2)



# From the "MCMCsummary" call, we can see the mean and credible interval bounds for the each of the posterior
# distributions we're interested in.

# we can then plot the model predictions and see how they relate to the observed values.

# get samples from posteriors 
samples = jags.samples(jmod, c("a", "b", "altura_pred"), length(jana_bay$altura))

# take the mean of each group of samples
posterior_means = apply(samples$altura_pred, 1, mean)

# plot posterior means versus observed values
plot(jana_bay$altura, posterior_means, ylab = "Predicted height", xlab = "Observed height")
lines(seq(1,10), seq(1, 10), col = "red", lwd = 2, cex = 3)


# ploting mean slope against theoretical predictions

res_plot <- data.frame(dano = c(rep("dano1", 2), rep("dano2", 2), rep("dano3", 2)), parameter = c("b", "a", "b", "a", "b", "a"),
                       mean = c(0.41,0.85,0.40,1.4,0.19,3.80), sd = c(0.057,0.480,0.062,0.640,0.044,0.520),
                       lowerbound = c(0.300,-0.099,0.28,0.11,0.10,2.70), upperbound = c(0.52,1.80,0.52,2.60,0.27,4.80))


range(res_plot$mean[res_plot$parameter == "b"])



# plot the values of the slopes againt model predictions
library(ggplot2)

somePDFPath = "C:/Users/Arildo/Documents/R/Cris-Janaguba/graphs_5.pdf"
pdf(file=somePDFPath)

f <- ggplot(as.data.frame(res_plot[res_plot$mean & res_plot$parameter == "b",]), aes(x = dano, y = mean, ymin=mean-sd, ymax=mean+sd)) + 
  xlab(NULL) + ylab("Diameter scaling exponent")

f + geom_pointrange() + theme(legend.position = "none") + geom_hline(yintercept = 2/3, linetype = "dashed", col="red") + 
  geom_hline(yintercept = 1, linetype = "solid", col = "red") 

dev.off()

