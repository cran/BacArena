% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Arena.R
\name{simEnv_par}
\alias{simEnv_par}
\alias{simEnv_par,Arena-method}
\title{Main function for simulating in parallel all processes in the environment}
\usage{
simEnv_par(
  object,
  time,
  lrw = NULL,
  continue = FALSE,
  reduce = FALSE,
  cluster_size = NULL,
  diffusion = TRUE,
  sec_obj = "none",
  cutoff = 1e-06,
  with_shadow = FALSE,
  verbose = TRUE
)

\S4method{simEnv_par}{Arena}(
  object,
  time,
  lrw = NULL,
  continue = FALSE,
  reduce = FALSE,
  cluster_size = NULL,
  diffusion = TRUE,
  sec_obj = "none",
  cutoff = 1e-06,
  with_shadow = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{object}{An object of class Arena or Eval.}

\item{time}{A number giving the number of iterations to perform for the simulation}

\item{lrw}{A numeric value needed by solver to estimate array size (by default lwr is estimated in the simEnv() by the function estimate_lrw())}

\item{continue}{A boolean indicating whether the simulation should be continued or restarted.}

\item{reduce}{A boolean indicating if the resulting \code{Eval} object should be reduced}

\item{cluster_size}{Number of cpu cores to be used.}

\item{diffusion}{True if diffusion should be done (default on).}

\item{sec_obj}{character giving the secondary objective for a bi-level LP if wanted.}

\item{cutoff}{value used to define numeric accuracy}

\item{with_shadow}{True if shadow cost should be stores (default off).}

\item{verbose}{Set to false if no status messages should be printed.}
}
\value{
Returns an object of class \code{Eval} which can be used for subsequent analysis steps.
}
\description{
The generic function \code{simEnv_par} for a simple in parallel all simulation of the environment.
}
\details{
The returned object itself can be used for a subsequent simulation, due to the inheritance between \code{Eval} and \code{Arena}.
}
\examples{
data(Ec_core, envir = environment()) #get Escherichia coli core metabolic model
bac <- Bac(Ec_core,deathrate=0.05,
           minweight=0.05,growtype="exponential") #initialize a bacterium
arena <- Arena(n=20,m=20) #initialize the environment
arena <- addOrg(arena,bac,amount=10) #add 10 organisms
arena <- addSubs(arena,40) #add all possible substances
eval <- simEnv(arena,5)
}
\seealso{
\code{\link{Arena-class}} and \code{\link{Eval-class}}
}
