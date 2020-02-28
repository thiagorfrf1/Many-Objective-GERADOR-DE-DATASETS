# -*- coding: UTF-8 -*-

import numpy as np
import pandas as pd
import random
import matplotlib.pyplot as plt
import pickle

from deap import base
from deap import creator
from deap import tools
from deap import algorithms

import rpy2.robjects as robjects

from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import SignatureTranslatedAnonymousPackage as STAP
from rpy2.robjects import IntVector, Formula
pandas2ri.activate()

N_ATTRIBUTES = 101
cont = 0
bobj = 0.4

NOBJ = 4
P = [12]
SCALES = [1]

NGEN = 5000
CXPB = 0.2
MUTPB = 0.2
INDPB = 0.5
POP = 50
filename = "NGEN=" + str(NGEN) + "-POP=" + str(POP) + "-CXPB=" + str(CXPB) + "-MUTPB=" + str(MUTPB) + "-INDPB=" + str(INDPB)
globalBalance = 0.25
globalLinear = 0.25
globalN1 = 0.25
globalN2 = 0.25

dic = {"Rotulo": "Valores"}

# reference points
ref_points = [tools.uniform_reference_points(NOBJ, p, s) for p, s in zip(P, SCALES)]
ref_points = np.concatenate(ref_points)
_, uniques = np.unique(ref_points, axis=0, return_index=True)
ref_points = ref_points[uniques]


string = """
#' Measures of linearity
#'
#' The linearity measures try to quantify if it is possible to separate the 
#' labels by a hyperplane or linear function. The underlying assumption is that 
#' a linearly separable problem can be considered simpler than a problem
#' requiring a non-linear decision boundary.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A response vector with one value for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the output column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{summarization} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param ... Not used.
#' @details
#'  The following classification measures are allowed for this method:
#'  \describe{
#'    \item{"L1"}{Sum of the error distance by linear programming (L1) computes 
#'      the sum of the distances of incorrectly classified examples to a linear 
#'      boundary used in their classification.}
#'    \item{"L2"}{Error rate of linear classifier (L2) computes the error rate 
#'      of the linear SVM classifier induced from dataset.}
#'    \item{"L3"}{Non-linearity of a linear classifier (L3) creates a new 
#'      dataset randomly interpolating pairs of training examples of the same 
#'      class and then induce a linear SVM on the original data and measure 
#'      the error rate in the new data points.}
#'  }
#'  The following regression measures are allowed for this method:
#'  \describe{
#'    \item{"L1"}{Mean absolute error (L1) averages the absolute values of the 
#'      residues of a multiple linear regressor.}
#'    \item{"L2"}{Residuals variance (L2) averages the square of the residuals 
#'      from a multiple linear regression.}
#'    \item{"L3"}{Non-linearity of a linear regressor (L3) measures how 
#'      sensitive the regressor is to the new randomly interpolated points.}
#'  }
#' @return A list named by the requested linearity measure.
#'
#' @references
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for the
#'    data complexity library in C++. Technical Report. La Salle - Universitat 
#'    Ramon Llull.
#'
#' @examples
#' ## Extract all linearity measures for classification task
#' data(iris)
#' linearity(Species ~ ., iris)
#'
#' ## Extract all linearity measures for regression task
#' data(cars)
#' linearity(speed ~ ., cars)
#' @export
linearity <- function(...) {
  UseMethod("linearity")
}

#' @rdname linearity
#' @export
linearity.default <- function(x, y, measures="all", 
                                    summary=c("mean", "sd"), ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  foo <- "regression"
  if(is.factor(y)) {
    foo <- "classification"
    if(min(table(y)) < 2) {
      stop("number of examples in the minority class should be >= 2")
    }
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.linearity()
  }

  measures <- match.arg(measures, ls.linearity(), TRUE)

  if (length(summary) == 0) {
    summary <- "return"
  }

  colnames(x) <- make.names(colnames(x), unique=TRUE)
  eval(call(foo, x=x, y=y, measures=measures, summary=summary))
}

#' @rdname linearity
#' @export
linearity.formula <- function(formula, data, measures="all", 
                                    summary=c("mean", "sd"), ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  linearity.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, summary, ...)
}

classification <- function(x, y, measures, summary, ...) {

  data <- data.frame(x, class=y)
  data <- ovo(data)

  model <- lapply(data, smo)
  sapply(measures, function(f) {
    measure = eval(call(paste("c", f, sep="."), model=model, data=data))
    summarization(measure, summary, f %in% ls.linearity.multiples(), ...)
  }, simplify=FALSE)
}

regression <- function(x, y, measures, summary, ...) {

  x <- normalize(x)
  y <- normalize(y)[,1]

  x <- x[order(y), ,drop=FALSE]
  y <- y[order(y)]

  model <- stats::lm(y ~ ., cbind(y=y, x))
  sapply(measures, function(f) {
    measure = eval(call(paste("r", f, sep="."), m=model, x=x, y=y))
    summarization(measure, summary, f %in% ls.linearity.multiples(), ...)
  }, simplify=FALSE)
}

ls.linearity <- function() {
  c("L1", "L2", "L3")
}

ls.linearity.multiples <- function() {
  ls.linearity()
}

smo <- function(data) {
  e1071::svm(class ~ ., data, scale=TRUE, kernel="linear")
}

c.L1 <- function(model, data) {

  aux <- mapply(function(m, d) {
    prd <- stats::predict(m, d, decision.values=TRUE)
    err <- rownames(d[prd != d$class,])
    dst <- attr(prd, "decision.values")[err,]
    sum(abs(dst))/nrow(d)
  }, m=model, d=data)

  #aux <- 1/(mean(aux) + 1)
  #aux <- 1 - aux
  aux <- 1 - 1/(aux + 1)
  return(aux)
}

error <- function(pred, class) {
  1 - sum(diag(table(class, pred)))/sum(table(class, pred))
}

c.L2 <- function(model, data) {

  aux <- mapply(function(m, d) {
    prd <- stats::predict(m, d)
    error(prd, d$class)
  }, m=model, d=data)

  #return(mean(aux))
  return(aux)
}

c.L3 <- function(model, data) {

  aux <- mapply(function(m, d) {
    tmp <- c.generate(d, nrow(d))
    prd <- stats::predict(m, tmp)
    error(prd, tmp$class)
  }, m=model, d=data)

  #return(mean(aux))
  return(aux)
}

r.L1 <- function(m, ...) {
  #mean(abs(m$residuals))
  abs(m$residuals)
}

r.L2 <- function(m, ...) {
  #mean(m$residuals^2)
  mean(m$residuals^2)
}

r.L3 <- function(m, x, y) {
  test <- r.generate(x, y, nrow(x))
  pred <- stats::predict.lm(m, test[, -ncol(test), drop=FALSE])
  #mean((pred - test[, ncol(test)])^2)
  (pred - test[, ncol(test)])^2
}





#' Measures of class balance
#'
#' Classification task. These measures capture the differences in the number of 
#' examples per class in the dataset. When these differences are severe, 
#' problems related to generalization of the ML classification techniques could 
#' happen because of the imbalance ratio.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"C1"}{The entropy of class proportions (C1) capture the imbalance in
#'      a dataset based on the proportions of examples per class.}
#'    \item{"C2"}{The imbalance ratio (C2) is an index computed for measuring
#'      class balance. This is a version of the measure that is also suited for 
#'      multiclass classification problems.}
#'  }
#' @return A list named by the requested class balance measure.
#'
#' @references
#'  Ana C Lorena, Ivan G Costa, Newton Spolaor and Marcilio C P Souto. (2012). 
#'    Analysis of complexity indices for classification problems: Cancer gene 
#'    expression data. Neurocomputing 75, 1, 33--42.
#'
#'  Ajay K Tanwani and Muddassar Farooq. (2010). Classification potential vs. 
#'    classification accuracy: a comprehensive study of evolutionary algorithms 
#'    with biomedical datasets. Learning Classifier Systems 6471, 127--144.
#'
#' @examples
#' ## Extract all balance measures for classification task
#' data(iris)
#' balance(Species ~ ., iris)
#' @export
balance <- function(...) {
  UseMethod("balance")
}

#' @rdname balance
#' @export
balance.default <- function(x, y, measures="all", ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  y <- as.factor(y)

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.balance()
  }

  measures <- match.arg(measures, ls.balance(), TRUE)

  sapply(measures, function(f) {
    eval(call(paste("c", f, sep="."), y=y))
  },  simplify=FALSE)
}

#' @rdname balance
#' @export
balance.formula <- function(formula, data, measures="all", ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  balance.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

ls.balance <- function() {
  c("C1", "C2")
}

c.C1 <- function(y) {
  c <- -1/log(nlevels(y))
  i <- table(y)/length(y)
  aux <- c*sum(i*log(i))
  return(aux)
}

c.C2 <- function(y) {
  ii <- summary(y)
  nc <- length(ii)
  aux <- ((nc - 1)/nc) * sum(ii/(length(y) - ii))
  aux <- 1 - (1/aux)
  return(aux)
}







colMin <- function(x) {
  apply(x, 2, min)
}

colMax <- function(x) {
  apply(x, 2, max)
}

dist <- function(x) {
  as.matrix(cluster::daisy(x, metric="gower", stand=TRUE, warnBin=FALSE))
}

form <- function(x) {
  att <- paste(colnames(x), collapse="+")
  stats::formula(paste("~ 0 +", att, sep=" "))
}

binarize <- function(x) {
  data.frame(stats::model.matrix(form(x), x))
}

ovo <- function(data) {

  aux <- utils::combn(levels(data$class), 2)

  tmp <- apply(aux, 2, function(i) {
    vet <- base::subset(data, data$class %in% i)
    vet$class <- factor(vet$class)
    return(vet)
  })

  return(tmp)
}

c.interpolation <- function(data) {

  aux <- data[data$class == sample(data$class, 1),]
  tmp <- aux[sample(nrow(aux), 2),]

  rnd <- stats::runif(1)

  for(i in 1:(ncol(data)-1)) {
    if(is.numeric(data[,i])) {
      tmp[1,i] <- tmp[1,i] + (tmp[2,i] - tmp[1,i]) * rnd
    } else {
      tmp[1,i] <- sample(aux[,i], 1)
    }
  }

  return(tmp[1,])
}

c.generate <- function(data, n) {

  tmp <- do.call("rbind",
    lapply(1:n, function(i) {
      c.interpolation(data)
    })
  )

  return(tmp)
}

maxmin <- function(x) {
  (x - min(x))/(max(x) - min(x))
}

normalize <- function(x) {

  x <- as.data.frame(x)
  for(i in 1:ncol(x)) {
    if(is.numeric(x[,i]))
      if(length(unique(x[,i])) != 1)
        x[,i] <- maxmin(x[,i])
  }
  return(x)
}

spearman <- function(x) {
  1-6*sum(x^2)/(length(x)^3 - length(x))
}

r.interpolation <- function(x, y, i) {

  aux <- x[(i-1):i,,drop=FALSE]

  rnd <- stats::runif(1)
  for(j in 1:ncol(x)) {
    if(is.numeric(x[,j])) {
      aux[1,j] <- aux[1,j] + (aux[2,j] - aux[1,j]) * rnd
    } else {
      aux[1,j] <- sample(aux[,j], 1)
    }
  }

  tmp <- y[(i-1):i]
  rnd <- stats::runif(1)
  tmp[1] <- tmp[1]*rnd + tmp[2]*(1-rnd)

  return(cbind(aux[1,], tmp[1]))
}

r.generate <- function(x, y, n) {

  tmp <- do.call("rbind",
    lapply(2:n, function(i) {
      r.interpolation(x, y, i)
    })
  )

  tmp <- data.frame(tmp)
  colnames(tmp) <- c(colnames(x), "y")
  return(tmp)
}






#' Post processing complexity measures
#'
#' Post-processing alternatives to deal with multiples values. This method is 
#' used by the complexity measures to summarize the obtained values.
#'
#' @param measure A list with the complexity measures values.
#' @param summary The functions to post processing the data. See the details
#'   to more information. Default: \code{c("mean", "sd")}
#' @param multiple A logical value defining if the measure should return
#'   multiple values. (Default: \code{TRUE})
#' @param ... Extra values used to the functions of summarization.
#' @details
#'  The post processing functions are used to summarize the complexity measures.
#'  They are organized into three groups: return, descriptive statistic and 
#'  distribution. Currently, the hypothesis testing post processing are not 
#'  supported.
#'
#'  In practice, there are no difference among the types, so that more than one
#'  type and functions can be combined. Usually, these function are used to
#'  summarize a set of values for each complexity measures. For instance, a 
#'  measure computed for each attribute can be summarized using the 
#'  \code{"mean"} and/or \code{"sd"}.
#'
#'  In addition to the native functions available in R, the following functions
#'  can be used:
#'  \describe{
#'    \item{"histogram"}{Computes a histogram of the given data value. The extra
#'       parameters '\code{bins}' can be used to define the number of values to
#'       be returned. The parameters '\code{max}' and '\code{min}' are used to
#'       define the range of the data. The default value for these parameters
#'       are respectively \code{10, min(x)} and \code{max(x)}.}
#'    \item{"kurtosis"}{See \code{\link[e1071]{kurtosis}}}
#'    \item{"max"}{See \code{\link{max}}}
#'    \item{"mean"}{See \code{\link{mean}}}
#'    \item{"median"}{See \code{\link{median}}}
#'    \item{"min"}{See \code{\link{min}}}
#'    \item{"quantiles"}{See \code{\link{quantile}}}
#'    \item{"sd"}{See \code{\link{sd}}}
#'    \item{"skewness"}{See \code{\link[e1071]{skewness}}}
#'    \item{"var"}{See \code{\link{var}}}
#'    \item{"return"}{Returns the original value(s) of the complexity measure.}
#'  }
#'  These functions are not restrictive, thus another functions can be applied
#'  as post-processing summarization function.
#'
#' @return A list with the post-processed complexity measures.
#'
#' @references
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for the
#'    data complexity library in C++. Technical Report. La Salle - Universitat 
#'    Ramon Llull.
#'
#' @examples
#' summarization(runif(15))
#' summarization(runif(15), c("min", "max"))
#' summarization(runif(15), c("quantiles", "skewness"))
#' @export
summarization <- function(measure, summary=c("mean", "sd"), multiple=TRUE, 
                          ...) {

  if(length(measure) == 0) {
    return(NA)
  }

  if(!multiple) {
    if(length(measure) > 1) {
      stop("More than one value was obtained for a single measure")
    }
    measure = as.numeric(measure[1])
    return(measure)
  }

  measure = measure[is.finite(measure)]

  res = sapply(summary, function(s) {
    do.call(s, list(measure, ...))
  }, simplify=FALSE)

  unlist(res)
}

skewness <- function(x, na.rm=FALSE, type=3, ...) {
  e1071::skewness(x, na.rm, type)
}

kurtosis <- function(x, na.rm=FALSE, type=3, ...) {
  e1071::kurtosis(x, na.rm, type)
}

quantiles <- function(x, type=1, ...) {
  tryCatch(
    stats::quantile(x, type=type, ...),
    error=function(e) stats::quantile(NA, na.rm=TRUE, ...)
  )
}

iqr <- function(x, na.rm=FALSE, ...) {
   if (!na.rm & any(is.na(x))) NA
   else stats::IQR(x, na.rm = na.rm)
}

histogram <- function(x, bins=10, min=base::min(x, na.rm=TRUE),
                 max=base::max(x, na.rm=TRUE), ...) {
  breaks <- seq(ifelse(is.finite(min), min, 0),
                ifelse(is.finite(max), max, bins), length.out=bins + 1)
  graphics::hist(as.numeric(x), breaks=breaks, plot=FALSE)$counts / length(x)
}




#' Measures of neighborhood
#'
#' Classification task. The Neighborhood measures analyze the neighborhoods of 
#' the data items and try to capture class overlapping and the shape of the 
#' decision boundary. They work over a distance matrix storing the distances 
#' between all pairs of data points in the dataset.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{summarization} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"N1"}{Fraction of borderline points (N1) computes the percentage of 
#'      vertexes incident to edges connecting examples of opposite classes in 
#'      a Minimum Spanning Tree (MST).}
#'    \item{"N2"}{Ratio of intra/extra class nearest neighbor distance (N2)  
#'      computes the ratio of two sums: intra-class and inter-class. The former 
#'      corresponds to the sum of the distances between each example and its 
#'      closest neighbor from the same class. The later is the sum of the 
#'      distances between each example and its closest neighbor from another 
#'      class (nearest enemy).}
#'    \item{"N3"}{Error rate of the nearest neighbor (N3) classifier corresponds
#'      to the error rate of a one Nearest Neighbor (1NN) classifier, estimated 
#'      using a leave-one-out procedure in dataset.}
#'    \item{"N4"}{Non-linearity of the nearest neighbor classifier (N4) creates 
#'      a new dataset randomly interpolating pairs of training examples of the 
#'      same class and then induce a the 1NN classifier on the original data and
#'      measure the error rate in the new data points.}
#'    \item{"T1"}{Fraction of hyperspheres covering data (T1) builds 
#'      hyperspheres centered at each one of the training examples, which have 
#'      their radios growth until the hypersphere reaches an example of another 
#'      class. Afterwards, smaller hyperspheres contained in larger hyperspheres 
#'      are eliminated. T1 is finally defined as the ratio between the number of 
#'      the remaining hyperspheres and the total number of examples in the 
#'      dataset.}
#'    \item{"LSC"}{Local Set Average Cardinality (LSC) is based on Local Set 
#'      (LS) and defined as the set of points from the dataset whose distance of
#'      each example is smaller than the distance from the exemples of the 
#'      different class. LSC is the average of the LS.}
#'  }
#' @return A list named by the requested neighborhood measure.
#'
#' @references
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for the
#'    data complexity library in C++. Technical Report. La Salle - Universitat 
#'    Ramon Llull.
#'
#'  Enrique Leyva, Antonio Gonzalez and Raul Perez. (2014). A Set of Complexity
#'    Measures Designed for Applying Meta-Learning to Instance Selection. IEEE
#'    Transactions on Knowledge and Data Engineering 27, 2, 354--367.
#'
#' @examples
#' ## Extract all neighborhood measures for classification task
#' data(iris)
#' neighborhood(Species ~ ., iris)
#' @export
neighborhood <- function(...) {
  UseMethod("neighborhood")
}

#' @rdname neighborhood
#' @export
neighborhood.default <- function(x, y, measures="all", summary=c("mean", "sd"), 
                                 ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  y <- as.factor(y)

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.neighborhood()
  }

  measures <- match.arg(measures, ls.neighborhood(), TRUE)

  if (length(summary) == 0) {
    summary <- "return"
  }

  colnames(x) <- make.names(colnames(x), unique=TRUE)
  data <- data.frame(x, class=y)
  dst <- dist(x)

  sapply(measures, function(f) {
    measure = eval(call(paste("c", f, sep="."), dst=dst, data=data))
    summarization(measure, summary, f %in% ls.neighborhood.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname neighborhood
#' @export
neighborhood.formula <- function(formula, data, measures="all", 
                                 summary=c("mean", "sd"), ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  neighborhood.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, summary, ...)
}

ls.neighborhood <- function() {
  c("N1","N2", "N3", "N4", "T1", "LSC")
}

ls.neighborhood.multiples <- function() {
  c("N2", "N3", "N4", "T1")
}

c.N1 <- function(dst, data) {

  g <- igraph::graph.adjacency(dst, mode="undirected", weighted=TRUE)
  tree <- as.matrix(igraph::as_adj(igraph::mst(g)))

  tmp <- which(tree != 0, arr.ind=TRUE)
  aux <- which(data[tmp[,1],]$class != data[tmp[,2],]$class)
  aux <- length(unique(tmp[aux,1]))
  return(aux/nrow(data))
}

intra <- function(dst, data, i) {
  tmp <- rownames(data[data$class == data[i,]$class,])
  aux <- min(dst[i, setdiff(tmp, i)])
  return(aux)
}

inter <- function(dst, data, i) {
  tmp <- rownames(data[data$class != data[i,]$class,])
  aux <- sort(dst[i, tmp])[1]
  return(aux)
}

c.N2 <- function(dst, data) {

  aux <- sapply(rownames(data), function(i) {
    c(intra(dst, data, i), inter(dst, data, i))
  })

  #aux <- sum(aux[1,])/sum(aux[2,])
  aux <- 1 - (1/((aux[1,]/aux[2,]) + 1))
  return(aux)
}

knn <- function(data, dst, k) {
  apply(dst, 1, function(i) {
    tmp <- names(sort(i)[k])
    data[tmp,]$class
  })
}

c.N3 <- function(dst, data) {
  aux <- knn(data, dst, 2) != data$class
  #return(mean(aux))
  return(aux)
}

c.N4 <- function(dst, data) {

  tran <- rbind(data, c.generate(data, nrow(data)))
  test <- utils::tail(tran, nrow(data))

  dst <- dist(tran[,-ncol(tran), drop=FALSE])
  dst <- dst[rownames(test), rownames(data)]

  aux <- knn(data, dst, 1) != test$class
  #return(mean(aux))
  return(aux)
}

radios <- function(dst, data, i) {

  di <- inter(dst, data, i)
  j <- names(di)
  dj <- inter(dst, data, j)
  k <- names(dj)

  if(i == k) {
    return(di/2)
  } else {
    tmp <- radios(dst, data, j)
    return(di - tmp)
  }
}

hyperspher <- function(dst, data) {

  aux <- sapply(rownames(data), function(i) {
    as.numeric(radios(dst, data, i))
  })

  return(aux)
}

translate <- function(dst, r) {

  aux <- t(sapply(rownames(dst), function(i) {
    dst[i,] < r[i]
  }))

  return(aux)
}

adherence <- function(adh, data) {

  h <- n <- c()

  repeat{

    aux <- which.max(rowSums(adh))
    tmp <- names(which(adh[aux,]))
    dif <- setdiff(rownames(adh), c(tmp, names(aux)))
    adh <- adh[dif, dif, drop=FALSE]

    if(all(dim(adh) != 0)) {
      h <- c(h, length(tmp))
    } else {
      h <- c(h, 1)
    }

    n <- c(n, names(aux))

    if(all(dim(adh)) == 0)
      break
  }

  names(h) <- n
  return(h)
}

c.T1 <- function(dst, data) {
  r <- hyperspher(dst, data)
  aux <- adherence(translate(dst, r), data)
  #aux <- length(aux)/nrow(data)
  return(aux/nrow(data))
}

c.LSC <- function(dst, data) {

  r <- sapply(rownames(data), function(i) {
    as.numeric(inter(dst, data, i))
  })

  aux <- 1 - sum(translate(dst, r))/(nrow(dst)^2)
  return(aux)
}


#' Measures of overlapping
#'
#' Classification task. The overlapping measures evaluate how informative the 
#' available features are to separate the classes. If there is at least one very
#' discriminative feature in the dataset, the problem can be considered simpler 
#' than if there is no such an attribute. 
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{summarization} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"F1"}{Maximum Fisher's Discriminant Ratio (F1) measures the overlap 
#'      between the values of the features and takes the value of the largest 
#'      discriminant ratio among all the available features.}
#'    \item{"F1v"}{Directional-vector maximum Fisher's discriminant ratio (F1v)
#'      complements F1 by searching for a vector able to separate two classes 
#'      after the training examples have been projected into it.}
#'    \item{"F2"}{Volume of the overlapping region (F2) computes the overlap of 
#'      the distributions of the features values within the classes. F2 can be 
#'      determined by finding, for each feature its minimum and maximum values 
#'      in the classes.}
#'    \item{"F3"}{The maximum individual feature efficiency (F3) of each 
#'      feature is given by the ratio between the number of examples that are 
#'      not in the overlapping region of two classes and the total number of 
#'      examples. This measure returns the maximum of the values found among 
#'      the input features.}
#'    \item{"F4"}{Collective feature efficiency (F4) get an overview on how 
#'      various features may work together in data separation. First the most 
#'      discriminative feature according to F3 is selected and all examples that
#'      can be separated by this feature are removed from the dataset. The 
#'      previous step is repeated on the remaining dataset until all the 
#'      features have been considered or no example remains. F4 returns the 
#'      ratio of examples that have been discriminated.}
#'  }
#' @return A list named by the requested overlapping measure.
#'
#' @references
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for the
#'    data complexity library in C++. Technical Report. La Salle - Universitat
#'    Ramon Llull.
#'
#' @examples
#' ## Extract all overlapping measures for classification task
#' data(iris)
#' overlapping(Species ~ ., iris)
#' @export
overlapping <- function(...) {
  UseMethod("overlapping")
}

#' @rdname overlapping
#' @export
overlapping.default <- function(x, y, measures="all", summary=c("mean", "sd"), 
                                ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  y <- as.factor(y)

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.overlapping()
  }

  measures <- match.arg(measures, ls.overlapping(), TRUE)

  if (length(summary) == 0) {
    summary <- "return"
  }

  colnames(x) <- make.names(colnames(x), unique=TRUE)
  x <- binarize(x)
  data <- data.frame(x, class=y)

  sapply(measures, function(f) {
    measure = eval(call(paste("c", f, sep="."), data=data))
    summarization(measure, summary, f %in% ls.overlapping.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname overlapping
#' @export
overlapping.formula <- function(formula, data, measures="all", 
                                summary=c("mean", "sd"), ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  overlapping.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, summary, ...)
}

ls.overlapping <- function() {
  c("F1", "F1v", "F2", "F3", "F4")
}

ls.overlapping.multiples <- function() {
  ls.overlapping()
}

branch <- function(data, j) {
  data[data$class == j, -ncol(data), drop=FALSE]
}

numerator <- function(j, data) {

  tmp <- branch(data, j)
  aux <- nrow(tmp) * (colMeans(tmp) - 
    colMeans(data[,-ncol(data), drop=FALSE]))^2
  return(aux)
}

denominator <- function(j, data) {

  tmp <- branch(data, j)
  aux <- rowSums((t(tmp) - colMeans(tmp))^2)
  return(aux)
}

c.F1 <- function(data) {

  num <- lapply(levels(data$class), numerator, data)
  den <- lapply(levels(data$class), denominator, data)

  aux <- rowSums(do.call("cbind", num)) / 
    rowSums(do.call("cbind", den))

  #aux <- max(aux, na.rm=TRUE)
  aux <- 1/(aux + 1)
  return(aux)
}

dvector <- function(data) {

  l <- levels(data$class)
  a <- branch(data, l[1])
  b <- branch(data, l[2])

  c1 <- colMeans(a)
  c2 <- colMeans(b)

  W <- (nrow(a)/nrow(data)) * stats::cov(a) + 
    (nrow(b)/nrow(data)) * stats::cov(b)

  B <- (c1 - c2) %*% t(c1 - c2)
  d <- MASS::ginv(W) %*% (c1 - c2)

  aux <- (t(d) %*% B %*% d)/(t(d) %*% W %*% d)
  return(aux)
}

c.F1v <- function(data) {
  data <- ovo(data)
  #aux <- mean(sapply(data, dvector))
  aux <- sapply(data, dvector)
  aux <- 1/(aux + 1)
  return(aux)
}

regionOver <- function(data) {

  l <- levels(data$class)
  a <- branch(data, l[1])
  b <- branch(data, l[2])

  maxmax <- rbind(colMax(a), colMax(b))
  minmin <- rbind(colMin(a), colMin(b))

  over <- colMax(rbind(colMin(maxmax) - colMax(minmin), 0))
  rang <- colMax(maxmax) - colMin(minmin)
  aux <- prod(over/rang, na.rm=TRUE)
  return(aux)
}

c.F2 <- function(data) {

  data <- ovo(data)
  #aux <- mean(sapply(data, regionOver))
  aux <- sapply(data, regionOver)
  return(aux)
}

nonOverlap <- function(data) {

  l <- levels(data$class)
  a <- branch(data, l[1])
  b <- branch(data, l[2])

  minmax <- colMin(rbind(colMax(a), colMax(b)))
  maxmin <- colMax(rbind(colMin(a), colMin(b)))

  aux <- do.call("cbind",
    lapply(1:(ncol(data)-1), function(i) {
        data[,i] < maxmin[i] | data[,i] > minmax[i]
    })
  )

  aux <- data.frame(aux)
  rownames(aux) <- rownames(data)
  return(aux)
}

c.F3 <- function(data) {

  data <- ovo(data)
  aux <- mapply(function(d) {
    colSums(nonOverlap(d))/nrow(d)
  }, d=data)

  #aux <- 1 - mean(colMax(aux))
  aux <- 1 - colMax(aux)
  return(aux)
}

removing <- function(data) {

  repeat {

    tmp <- nonOverlap(data)
    col <- which.max(colSums(tmp))
    aux <- rownames(tmp[tmp[,col] != TRUE, , drop=FALSE])
    data <- data[aux,- col, drop=FALSE]

    if(nrow(data) == 0 | ncol(data) == 1 |
      length(unique(data$class)) == 1)
        break
  }

  return(data)
}

c.F4 <- function(data) {

  data <- ovo(data)
  aux <- mapply(function(d) {
    nrow(removing(d))/nrow(d)
  }, d=data)

  #aux <- mean(aux)
  return(aux)
}

"""



stringr_c = STAP(string, "stringr_c")
stringr_c._rpy2r.keys()


def print_evaluate(individual):
    dataFrame['label'] = individual
    robjects.globalenv['dataFrame'] = dataFrame
    fmla = Formula('label ~ .')

    ## -- linearity
    linearityVector = stringr_c.linearity_formula(fmla, dataFrame, measures="L2", summary="return")
    linearity = linearityVector.rx(1)
    fitness = abs(globalLinear - linearity[0][0])

    ## -- neighborhood N1
    n1Vector = stringr_c.neighborhood_formula(fmla, dataFrame, measures="N1", summary="return")
    f1 = n1Vector.rx(1)
    fitness2 = abs(globalN1 - f1[0][0])

    ## -- neighborhood N2
    n2Vector = stringr_c.neighborhood_formula(fmla, dataFrame, measures="N2", summary="return")
    n2 = n2Vector.rx(1)
    fitness3 = abs(globalN2 - n2[0][0])

    ##imbalance
    imbalanceVector = stringr_c.balance_formula(fmla, dataFrame, measures="C2", summary="return")
    imbalance = imbalanceVector.rx(1)
    fitness4 = abs(globalBalance - imbalance[0][0])

    ## --
    return (imbalance[0][0]), (linearity[0][0]), (f1[0][0]), (n2[0][0]),


def my_evaluate(individual):
    dataFrame['label'] = individual
    robjects.globalenv['dataFrame'] = dataFrame
    fmla = Formula('label ~ .')

    ## -- linearity
    linearityVector = stringr_c.linearity_formula(fmla, dataFrame, measures="L2", summary="return")
    linearity = linearityVector.rx(1)
    fitness = abs(globalLinear - linearity[0][0])

    ## -- neighborhood N1
    n1Vector = stringr_c.neighborhood_formula(fmla, dataFrame, measures="N1", summary="return")
    f1 = n1Vector.rx(1)
    fitness2 = abs(globalN1 - f1[0][0])

    ## -- neighborhood N2
    n2Vector = stringr_c.neighborhood_formula(fmla, dataFrame, measures="N2", summary="return")
    n2 = n2Vector.rx(1)
    fitness3 = abs(globalN2 - n2[0][0])

    ##imbalance
    imbalanceVector = stringr_c.balance_formula(fmla, dataFrame, measures="C2", summary="return")
    imbalance = imbalanceVector.rx(1)
    fitness4 = abs(globalBalance - imbalance[0][0])

    print("imbalance: " + str(imbalance[0][0]) + " linearity: " + str(linearity[0][0]) + " N1: " + str(
        f1[0][0]) + " N2: " + str(n2[0][0]))
    ## --
    return (fitness4), (fitness), (fitness2), (fitness3),


creator.create("FitnessMin", base.Fitness, weights=(-1.0,)*NOBJ)
creator.create("Individual", list, fitness=creator.FitnessMin)

RANDINT_LOW = 0
RANDINT_UP = 1

toolbox = base.Toolbox()
toolbox.register("attr_int", random.randint, RANDINT_LOW, RANDINT_UP)
toolbox.register("individual", tools.initRepeat, creator.Individual, toolbox.attr_int, N_ATTRIBUTES)
toolbox.register("population", tools.initRepeat, list, toolbox.individual)
toolbox.register("evaluate", my_evaluate)
toolbox.register("mate", tools.cxTwoPoint)
toolbox.register("mutate", tools.mutShuffleIndexes, indpb=INDPB)
toolbox.register("select", tools.selNSGA3, ref_points=ref_points)

def main(seed=None):
    random.seed(seed)

    # Initialize statistics object
    stats = tools.Statistics(lambda ind: ind.fitness.values)
    stats.register("avg", np.mean, axis=0)
    stats.register("std", np.std, axis=0)
    stats.register("min", np.min, axis=0)
    stats.register("max", np.max, axis=0)

    logbook = tools.Logbook()
    logbook.header = "gen", "evals", "std", "min", "avg", "max"

    pop = toolbox.population(POP)

    # Evaluate the individuals with an invalid fitness
    invalid_ind = [ind for ind in pop if not ind.fitness.valid]
    fitnesses = toolbox.map(toolbox.evaluate, invalid_ind)
    for ind, fit in zip(invalid_ind, fitnesses):
        ind.fitness.values = fit

    # Compile statistics about the population
    record = stats.compile(pop)

    logbook.record(gen=0, evals=len(invalid_ind), **record)
    print(logbook.stream)

    # Begin the generational process
    for gen in range(1, NGEN):
        offspring = algorithms.varAnd(pop, toolbox, CXPB, MUTPB)

        # Evaluate the individuals with an invalid fitness
        invalid_ind = [ind for ind in offspring if not ind.fitness.valid]
        fitnesses = toolbox.map(toolbox.evaluate, invalid_ind)
        for ind, fit in zip(invalid_ind, fitnesses):
            ind.fitness.values = fit
        # Select the next generation population from parents and offspring
        pop = toolbox.select(pop + offspring, POP)

        for x in range(POP):
            dic[print_evaluate(pop[x])] = pop[x]
            outfile = open(filename, 'wb')
            pickle.dump(dic, outfile)
            outfile.close()
        # Compile statistics about the new population
        record = stats.compile(pop)
        logbook.record(gen=gen, evals=len(invalid_ind), **record)
        print(logbook.stream)
    return pop, logbook

if __name__ == '__main__':
    cont1 = 0
    cont0 = 0
    dataFrame = pd.read_csv(str(N_ATTRIBUTES) + '.csv')
    dataFrame = dataFrame.drop('c0', axis=1)
    results = main()
    infile = open(filename, 'rb')
    new_dict = pickle.load(infile)
    print("NEW DICT")
    print(new_dict)
    infile.close()
    print("logbook")
    print(results[1])
    robjects.globalenv['dataFrame'] = dataFrame
    dataFrame.to_csv(
        str(N_ATTRIBUTES) + '_' + str(bobj).replace('.', ',') + '_' + str(globalBalance).replace('.', ',') + '.csv',
        index=False)
    dataFrame.head(N_ATTRIBUTES)

    df = pd.read_csv(
        str(N_ATTRIBUTES) + '_' + str(bobj).replace('.', ',') + '_' + str(globalBalance).replace('.', ',') + ".csv")
    colors = {0: 'red', 1: 'blue'}
    markers = {0: '+', 1: '_'}
    fig, ax = plt.subplots()
    grouped = df.groupby('label')
    plt.rcParams['figure.figsize'] = (11, 7)
    for key, group in grouped:
        group.plot(ax=ax, kind='scatter', marker=markers[key], x='1', y='2', label=key, color=colors[key])
    plt.show()