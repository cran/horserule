#' Most important Rules/terms
#'
#' Produces a table containing the most important Rules or linear terms

#'@param model list containing a model of class "HorseRuleFit".
#'@param k number of most important rules to be shown in the table.


#' @export
#' @examples
#'library(MASS)
#'library(horserule)
#'data(Boston)
#' # Split in train and test data
#'N = nrow(Boston)
#'train = sample(1:N, 400)
#'Xtrain = Boston[train,-14]
#'ytrain = Boston[train, 14]
#'Xtest = Boston[-train, -14]
#'ytest = Boston[-train, 14]
#'hrres = HorseRuleFit(X = Xtrain, y=ytrain,
#'                     thin=1, niter=100, burnin=10,
#'                     L=5, S=6, ensemble = "both", mix=0.3, ntree=100,
#'                     intercept=FALSE, linterms=1:13, ytransform = "log",
#'                     alpha=1, beta=2, linp = 1, restricted = 0)
#'
#' #Create an importance table containing the 10 most important rules and linear terms
#' importance_hs(hrres, k=10)


importance_hs = function(model, k=10){
  if(class(model)!="HorseRulemodel"){
    stop("Model must be of class HorseRulemodel")
  }
  sup = (model$modelstuff)$mur
  sdl = (model$modelstuff)$sdl
  sdr = (model$modelstuff)$sdr
  sdy = model$modelstuff$sdy
  rules = model$rules
  lin = (model$modelstuff)$linterms
  beta = (model$postdraws)$beta*sdy
  bhat = model$bhat*sdy
  imp_post_mean = abs(bhat)
  nmost = k
  inds = order(imp_post_mean, decreasing = T)[1:nmost]
  quants = matrix(0, ncol=3, nrow=length(inds))
  betanorm = t(apply(beta,1, function(x)(abs(x)-min(abs(x)))/(max(abs(x))-min(abs(x)))))
  for(i in 1:nmost){
    quants[i,] = as.numeric(quantile(abs(betanorm[,inds[i]]), probs=c(0.025,0.5,0.975)))
  }
  rulesout = c()
  for(i in 1:nmost){
    rulesout[i] = ifelse(inds[i]<length(lin), paste("Linear:X", inds[i]), rules[(inds[i]-length(lin))])
  }

  res = data.frame(rulesout, quants, bhat[inds]/c(sdl, sdr)[inds])
  colnames(res) = c("Rule", "2.5% Imp", "50% Imp","97.5% Imp", "bhat")
  print(res)
  #need to calculate the support should be saved = mur
}
