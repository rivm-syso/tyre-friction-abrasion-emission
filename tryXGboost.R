#try xgboost
source("R/ReadIDIADAtracksAs1.R")
library(xgboost)
library(data.table)
library(ggplot2)
#library(caret)
#library(pdp)
#once to sum easy
#for (dfName in names(TrackSumData)) {
#  df <- TrackSumData[[dfName]]
#  sumDist <- sum(df$distance)
#  df$DistFrac = df$distance / sumDist
#  TrackSumData[[dfName]] <- df
#}
general <- cbind(general, perkmTrack)
DFin <- merge(WearAsLong, general[, c("TrackType", "decel_ms2", "accel_ms2", "accelFrac", "decelFrac",
                                      "mainSpeed", "mainCorner1dRadius", "maincornerSpeed", "CornerFraction", "breaks",
                                      "distance","rotationpkm", "AIname", "centrW_g_1",
                                      "decelW_g_1","accelW_g_1","ownWind_Ac_1")], 
              by.x = "track", by.y = "AIname")

#summary(DFin)

DFin$track <- NULL

DFin$TrackisPAV <- DFin$TrackType == "PAV"
DFin$TrackType <- NULL
#DFin$Wheel <- as.factor(DFin$Wheel)
DFin$FrontWheel <- startsWith(levels(DFin$Wheel)[DFin$Wheel], "F")
DFin$RearWheel <- startsWith(levels(DFin$Wheel)[DFin$Wheel], "R")
DFin$InnerWheel <- grepl("Inner", levels(DFin$Wheel)[DFin$Wheel])
DFin$OuterWheel <- grepl("Outer", levels(DFin$Wheel)[DFin$Wheel])
DFin$Wheel <- NULL

#DFin$track <- as.factor(DFin$track)
DFin$FrOrRear <- NULL

wearfactors <- Matrix::sparse.model.matrix(wear~.,  data = DFin[,c("wear", names(DFin)[sapply(DFin, is.factor)])])
wearfactors <- wearfactors[,colnames(wearfactors)[colnames(wearfactors)!= "(Intercept)"]]
PrepMat <- cbind(as.matrix(DFin[,names(DFin)[!sapply(DFin, is.factor)]]), wearfactors)
#colnames(PrepMat)
dtrain <- xgb.DMatrix(data = as.matrix(PrepMat[,-match("wear", colnames(PrepMat))]), label = PrepMat[,"wear"])

params <- list(
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.2,
  eval_metric = "rmse",
  subsample = 0.75,
  min_child_weight = 1,
  silent = 1
)

cvtry <- xgb.cv(
    params = params,
    data = dtrain,
    nfold = 20,
    nrounds = 100,
    early_stopping_rounds = 4,
    maximize = F,
    callbacks = list(cb.cv.predict(save_models = T))
    )

xmportance <- as.data.frame(xgb.importance(model = cvtry$models[[1]]))
xmportance$FeatureIndex <- as.numeric(gsub("f", "",xmportance$Feature))
xmportance$Feature <- colnames(PrepMat)[colnames(PrepMat) != "wear"][xmportance$FeatureIndex+1] #py to R -> + 1

#first everything except the tyres

#save first 
Tyrecolumn <- DFin$Tyre
DFin$Tyre <- NULL
PrepMat <- as.matrix(DFin)
#colnames(PrepMat)
dtrain <- xgb.DMatrix(data = as.matrix(PrepMat[,-match("wear", colnames(PrepMat))]), label = PrepMat[,"wear"])

params <- list(
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.2,
  eval_metric = "rmse",
  subsample = 0.8,
  min_child_weight = 1,
  silent = 1
)

cvtry <- xgb.cv(
  params = params,
  data = dtrain,
  nfold = 20,
  nrounds = 100,
  early_stopping_rounds = 4,
  maximize = F,
  callbacks = list(cb.cv.predict(save_models = T))
)

xmportance <- as.data.frame(xgb.importance(model = cvtry$models[[1]]))
xmportance$FeatureIndex <- as.numeric(gsub("f", "",xmportance$Feature))
xmportance$Feature <- colnames(PrepMat)[colnames(PrepMat) != "wear"][xmportance$FeatureIndex+1] #py to R -> + 1

#with caret
library(caret)
cntr <- trainControl(method = "cv", number = 20)

xgb_model <- train(
  x = as.matrix(PrepMat[,-match("wear", colnames(PrepMat))]),
  y = PrepMat[,"wear"],
  method = "xgbTree",
  trControl = cntr,
#  early_stopping_rounds = 4,
#  params = list(maximize = F),
  tuneGrid = expand.grid(max_depth = 3, 
                         eta = c(0.1), 
                         nrounds = 19:26, 
                         subsample = 0.8,
                         gamma = 0.1,
                         colsample_bytree = 0.7,
                         min_child_weight = 1
                         ),
  metric = "Rsquared"
)
#results in
#nrounds = 20, max_depth = 3, eta = 0.1, gamma = 0.1, colsample_bytree = 0.7, min_child_weight = 1 and subsample = 0.8
#early stopping did not work, trying more rouns, leading to
#The final values used for the model were nrounds = 25, max_depth = 3, eta = 0.1, gamma = 0.1, colsample_bytree = 0.7, min_child_weight = 1 and subsample = 0.8.
#nrounds  RMSE      Rsquared   MAE     
#19       195.2151  0.7926450  109.0937
#20       194.3732  0.7927175  108.7034
#21       194.6882  0.7915678  109.1736
#22       194.0262  0.7924585  108.8164
#23       193.9027  0.7925420  108.7716
#24       193.7689  0.7928188  108.7532
#25       194.0443  0.7933408  109.0630
#26       194.5835  0.7918481  109.2954
#and the final model is:
params <- list(
  nrounds = 25, max_depth = 3, eta = 0.1, gamma = 0.1, colsample_bytree = 0.7, min_child_weight = 1, subsample = 0.8,
  silent = 1
)

roadModel <- xgboost(data = as.matrix(PrepMat[,-match("wear", colnames(PrepMat))]), 
                     label = PrepMat[,"wear"],
                     nrounds = 25, max_depth = 3, eta = 0.1, gamma = 0.1, colsample_bytree = 0.7, min_child_weight = 1, subsample = 0.8)

xgb.plot.tree(model = roadModel, trees = c(0,1))
xgb.plot.shap(data = as.matrix(PrepMat[,-match("wear", colnames(PrepMat))]), model = roadModel, top_n = 15)
xgb.importance(model = roadModel)

wearRoad <- predict(roadModel, newdata = as.matrix(PrepMat[,-match("wear", colnames(PrepMat))]))
TyrePm <- data.frame(
  Tyre = gsub(" ", "", Tyrecolumn),
  wearDivRoad = DFin$wear / wearRoad
)

ggplot(data = TyrePm, aes(x = Tyre, y = wearDivRoad, fill = Tyre)) +
  geom_violin(trim = F) +
  geom_boxplot(width = 0.1)
