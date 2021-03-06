% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convergence_plot.R
\name{convergence_plot}
\alias{convergence_plot}
\title{convergence_plot}
\usage{
convergence_plot(model, Xtest, ytest, burnin = 0)
}
\arguments{
\item{model}{list containing a model of class "hs_rulefit".}

\item{Xtest}{Out of bag sample to check error.}

\item{ytest}{response of test data.}

\item{burnin}{Number of samples disregarded as burnin.}
}
\description{
Can be used to check model convergence.
}
\details{
Convergence is checked by the convergence of the prediction error on unseen test data, to find a suitable number of iterations, in the spirit of gradient boosting. To check convergence on the Training data just use training X and y instead of Xtest and ytest.
}
\examples{
library(MASS)
data(Boston)
#Split in train and test data
N = nrow(Boston)
train = sample(1:N, 400)
Xtrain = Boston[train,-14]
ytrain = Boston[train, 14]
Xtest = Boston[-train, -14]
ytest = Boston[-train, 14]

hrres = HorseRuleFit(X = Xtrain, y=ytrain,
                    thin=1, niter=100, burnin=10,
                    L=5, S=6, ensemble = "both", mix=0.3, ntree=100,
                    intercept=FALSE, linterms=1:13, ytransform = "log",
                    alpha=1, beta=2, linp = 1, restricted = 0)

#Check the model convergence out of sample
convergence_plot(hrres, Xtest, ytest, burnin = 10)
}
