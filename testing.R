input <- list(
  input_predict="Outcome",
  input_ds="trivial.csv",
  
  random_seed=1,
  numpoint=100,
  
  "mean x"=0,
  "sd x"=1,
  "mean z"=0,
  "sd z"=1,
  
  input_posterior_x="x",
  input_posterior_y="z"
)

reactive <- function(f) function() f

################################################################################


if(FALSE){
  
}


thedat <- read.csv("/corgi/websites/demo_linmod/data/diabetes.csv")

thedat_PSI <- thedat[,colnames(thedat)!="Outcome",drop=FALSE]
thedat_t <- thedat[,colnames(thedat)=="Outcome",drop=FALSE]
prior_mu <- rep(0, ncol(thedat_PSI))
prior_S_diag <- rep(1, ncol(thedat_PSI))
input_beta <- 1

thedat_PSI <- as.matrix(thedat_PSI)
thedat_t <- as.matrix(thedat_t)

prior_S <- sigma2_to_diagmatrix(prior_S_diag)

posterior <- calc_posterior_from_prior(prior_mu, prior_S, thedat_PSI, thedat_t, input_beta)
posterior$S
#PSI <- thedat_PSI
#input_t <- thedat_t

prior_mu

#Marginal distributions: can just remove irrelevant variables, 
#https://en.wikipedia.org/wiki/Multivariate_normal_distribution

cur_axis <- 1

#### Figure out range to show, sample x
numsigma <- 3
sensible_points_range <- c(
  prior_mu[cur_axis] - numsigma*sqrt(prior_S[cur_axis,cur_axis]),
  prior_mu[cur_axis] + numsigma*sqrt(prior_S[cur_axis,cur_axis]),
  posterior$mu[cur_axis] - numsigma*sqrt(posterior$S[cur_axis,cur_axis]),
  posterior$mu[cur_axis] + numsigma*sqrt(posterior$S[cur_axis,cur_axis])
)
posx <- seq(
  length.out=100,
  from=min(sensible_points_range), 
  to=max(sensible_points_range))


#### Get prior and posterior densities
densx_prior <- dnorm(posx, 
                     mean=prior_mu[cur_axis], 
                     sd=sqrt(prior_S[cur_axis,cur_axis]))
densx_prior <- densx_prior/max(densx_prior)
densx_post <- dnorm(posx, 
                    mean=posterior$mu[cur_axis], 
                    sd=sqrt(posterior$S[cur_axis,cur_axis]))
densx_post <- densx_post/max(densx_post)

### Plotting
data.frame(x=posx, y=densx_prior, group="Prior")
alldat <- rbind(
  data.frame(x=posx, y=densx_prior, group="Prior"),
  data.frame(x=posx, y=-densx_post, group="Posterior")
)
#ggplot(alldat, aes(x,y,color=group)) + geom_line()
ggplot(alldat, aes(x,y,fill=group)) + geom_area()






###############


posx <- seq(from=-10, to=10, by=0.1)
densx_prior <- dnorm(posx, mean=0, sd=1)
densx_post <- dnorm(posx, mean=5, sd=1)
data.frame(x=posx, y=densx_prior, group="Prior")
alldat <- rbind(
  data.frame(x=posx, y=densx_prior, group="Prior"),
  data.frame(x=posx, y=-densx_post, group="Posterior")
)
#ggplot(alldat, aes(x,y,color=group)) + geom_line()
ggplot(alldat, aes(x,y,fill=group)) + geom_area()



################################################################################
########### General functions ##################################################
################################################################################

dat <- data.frame(
  x=1:100
)
dat$z <- 1
dat$Outcome <- 3*dat$x+dat$z
write.csv(dat, "/corgi/websites/demo_linmod/data/trivial.csv", row.names = FALSE)

