% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importance_hs.R
\name{importance_hs}
\alias{importance_hs}
\title{Most important Rules/terms}
\usage{
importance_hs(model, k = 10)
}
\arguments{
\item{model}{list containing a model of class "HorseRuleFit".}

\item{k}{number of most important rules to be shown in the table.}
}
\description{
Produces a table containing the most important Rules or linear terms
}
\examples{
library(MASS)
library(horserule)
data(Boston)
# Split in train and test data
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

#Create an importance table containing the 10 most important rules and linear terms
importance_hs(hrres, k=10)
}
