## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2015  Chris Kypridemos
 
## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.

require(randomForest)

load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[diabtotr == 1, diabtotr :=0]
HSE.ts[diabtotr == 2, diabtotr :=1]
HSE.ts[bmival<15 & age>16, bmival := 15]
HSE.ts[bmival>40 & age>16, bmival := 40]
HSE.ts[age>85, age:= 85] # different than the usual 85
HSE.ts[, a30to06m.imp := factor(a30to06m.imp)]
HSE.ts[, cigst1 := factor(cigst1)]
HSE.ts[, porftvg := factor(porftvg)]

train <- HSE.ts[is.na(diabtotr)== F & year<0 &  is.na(bmival)== F & is.na(a30to06m.imp)== F & is.na(cigst1) == F & is.na(porftvg) == F, ]
test <- HSE.ts[is.na(diabtotr)== F & year==0 &  is.na(bmival)== F & is.na(a30to06m.imp)== F & is.na(cigst1) == F & is.na(porftvg) == F, ]

train[, bmivalc := cut(bmival, breaks = c(0, seq(20,35, 5), Inf), ordered = T)]
test [, bmivalc := cut(bmival, breaks = c(0, seq(20,35, 5), Inf), ordered = T)]
train[, table(diabtotr)]

tmp = as.vector(train[, table(diabtotr)])
num_classes = length(tmp)
min_size = tmp[order(tmp,decreasing=FALSE)[1]]
sampsizes = rep(min_size,num_classes)
rf_output = randomForest(factor(diabtotr) ~ age + bmival + a30to06m.imp + cigst1,
                       data = train,
                       importance = TRUE,
                       ntree = 100, 
                       #mtry = 1,
                       na.action = na.omit,
                      # proximity=TRUE,
                      sampsize=c(1000, 1000),
                      classwt = c(1, 1),
                      nodesize = 5,
                      maxnodes = 10,
                      do.trace=100
)

rf_output
importance(rf_output, scale=FALSE)
plot(rf_output)

summary(predict(rf_output, test, type = "response"))
ttt <- data.table(predict(rf_output, test, type = "prob"))
ttt[, pr := runif(.N)<`1`]
ttt[, sum(pr, na.rm = T)]
test[, table(diabtotr)]

rf_importances=importance(rf_output, scale=FALSE)
confusion=rf_output$confusion
sensitivity=(confusion[2,2]/(confusion[2,2]+confusion[2,1]))*100
specificity=(confusion[1,1]/(confusion[1,1]+confusion[1,2]))*100
overall_error=rf_output$err.rate[length(rf_output$err.rate[,1]),1]*100
overall_accuracy=1-overall_error
class1_error=paste(rownames(confusion)[1]," error rate= ",confusion[1,3], sep="")
class2_error=paste(rownames(confusion)[2]," error rate= ",confusion[2,3], sep="")
overall_accuracy=100-overall_error

library(caret)
library(pROC)

rf_output=train(factor(diabtotr) ~ age + bmivalc + a30to06m.imp + qimd + sex,
                       data = train,
                       method='rf',
               # trControl=trainControl(method="cv",number=5),
                #prox=TRUE,allowParallel=TRUE,
                weights = wt.blood,
                na.action = na.omit
)
mod0 <- train(factor(diabtotr, levels = 0:1, labels = c("n", "y")) ~ age + bmival + a30to06m.imp + qimd + sex,
              data = train,
              method = "rf",
              metric = "ROC",
              tuneGrid = data.frame(mtry = 3),
              ntree = 1000,
              weights = wt.blood,
              trControl = trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       classProbs = TRUE,
                                       summaryFunction = twoClassSummary))
getTrainPerf(mod0)
test[, diabtotr := factor(diabtotr, levels = 0:1, labels = c("n", "y"))]

## Get the ROC curve
roc0 <- roc(test$diabtotr,
            predict(mod0, test, type = "prob")[,1],
            levels = rev(levels(test$diabtotr)))
roc0

## Now plot
plot(roc0, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)

## Get the model code for the original random forest method:

thresh_code <- getModelInfo("rf", regex = FALSE)[[1]]
thresh_code$type <- c("Classification")
## Add the threshold as another tuning parameter
thresh_code$parameters <- data.frame(parameter = c("mtry", "threshold"),
                                     class = c("numeric", "numeric"),
                                     label = c("#Randomly Selected Predictors",
                                               "Probability Cutoff"))
## The default tuning grid code:
thresh_code$grid <- function(x, y, len = NULL) {
  p <- ncol(x)
  expand.grid(mtry = floor(sqrt(p)),
              threshold = seq(.5, .99, by = .01))
}

## Here we fit a single random forest model (with a fixed mtry)
## and loop over the threshold values to get predictions from the same
## randomForest model.
thresh_code$loop = function(grid) {
  library(plyr)
  loop <- ddply(grid, c("mtry"),
                function(x) c(threshold = max(x$threshold)))
  submodels <- vector(mode = "list", length = nrow(loop))
  for(i in seq(along = loop$threshold)) {
    index <- which(grid$mtry == loop$mtry[i])
    cuts <- grid[index, "threshold"]
    submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
  }
  list(loop = loop, submodels = submodels)
}

## Fit the model independent of the threshold parameter
thresh_code$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
  if(length(levels(y)) != 2)
    stop("This works only for 2-class problems")
  randomForest(x, y, mtry = param$mtry, ...)
}

## Now get a probability prediction and use different thresholds to
## get the predicted class
thresh_code$predict = function(modelFit, newdata, submodels = NULL) {
  class1Prob <- predict(modelFit,
                        newdata,
                        type = "prob")[, modelFit$obsLevels[1]]
  ## Raise the threshold for class #1 and a higher level of
  ## evidence is needed to call it class 1 so it should 
  ## decrease sensitivity and increase specificity
  out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                modelFit$obsLevels[1],
                modelFit$obsLevels[2])
  if(!is.null(submodels)) {
    tmp2 <- out
    out <- vector(mode = "list", length = length(submodels$threshold))
    out[[1]] <- tmp2
    for(i in seq(along = submodels$threshold)) {
      out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                           modelFit$obsLevels[1],
                           modelFit$obsLevels[2])
    }
  }
  out
}

## The probabilities are always the same but we have to create
## mulitple versions of the probs to evaluate the data across
## thresholds
thresh_code$prob = function(modelFit, newdata, submodels = NULL) {
  out <- as.data.frame(predict(modelFit, newdata, type = "prob"))
  if(!is.null(submodels)) {
    probs <- out
    out <- vector(mode = "list", length = length(submodels$threshold)+1)
    out <- lapply(out, function(x) probs)
  }
  out
}

fourStats <- function (data, lev = levels(data$obs), model = NULL) {
  ## This code will get use the area under the ROC curve and the
  ## sensitivity and specificity values using the current candidate
  ## value of the probability threshold.
  out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
  
  ## The best possible model has sensitivity of 1 and specificity of 1. 
  ## How far are we from that value?
  coords <- matrix(c(1, 1, out["Spec"], out["Sens"]),
                   ncol = 2,
                   byrow = TRUE)
  colnames(coords) <- c("Spec", "Sens")
  rownames(coords) <- c("Best", "Current")
  c(out, Dist = dist(coords)[1])
}

# set.seed(949)
mod1 <- train(factor(diabtotr, levels = 0:1, labels = c("n", "y")) ~ age + bmival + a30to06m.imp + qimd + sex,
              data = train,
              method = thresh_code,
              ## Minimize the distance to the perfect model
              metric = "Dist",
              maximize = FALSE,
              tuneLength = 20,
              ntree = 1000,
              trControl = trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       classProbs = TRUE,
                                       summaryFunction = fourStats))

mod1


library(reshape2)
metrics <- mod1$results[, c(2, 4:6)]
metrics <- melt(metrics, id.vars = "threshold",
                variable.name = "Resampled",
                value.name = "Data")

ggplot(metrics, aes(x = threshold, y = Data, color = Resampled)) +
  geom_line() +
  ylab("") + xlab("Probability Cutoff") +
  theme(legend.position = "top")

tt <- data.table(predict(mod1, test, "prob"))[, pr := runif(.N) < y]






