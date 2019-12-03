#######################################################################
#
# Freight Data - Random Forest
#
# started: 2019-11-02
# revised: 2019-11-04
# Team 6
#
#######################################################################

# Load Relevant Packages
library(tidyverse)
library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(lime)
library(randomForest)
library(randomForestExplainer)
library(ggRandomForests)
library(rattle)
library(RColorBrewer)
library(DiagrammeR)
# Set working directory
# setwd()

# Load data
freight_forest <- fread("freightforest.csv") %>%
  mutate(UNITNUMBER=as.character(UNITNUMBER))

# Change data variable types
freight_forest <- freight_forest %>% 
  mutate(UNITNUMBER=as.factor(UNITNUMBER),
         ACTIVCODE=as.factor(ACTIVCODE),
         MAKE=as.factor(MAKE),
         MODEL=as.factor(MODEL),
         ENGINE=as.factor(ENGINE),
         TYPE=as.factor(TYPE)) %>% 
  select(-V1,-UNITNUMBER,-ACTIVCODE,-HOURS,-Total)

# Split datasets into Tractor and Trailers
freight_tractor <- freight_forest %>% 
  filter(TYPE=="TRACTOR")

freight_trailer <- freight_forest %>% 
  filter(TYPE=="TRAILER")

# Sample the data for tractors
set.seed(1842)
sample_set <- sample(nrow(freight_tractor), round(nrow(freight_tractor)*.70), replace = FALSE)
freight_train <- freight_tractor[sample_set, ] 
freight_test <- freight_tractor[-sample_set, ]

# Starting with Decision Tree as we can use the visual to infer impact of different variables
# Train the model
freight_tree <-
  rpart(
    BREAKDOWN ~ .,
    method = "anova",
    data = freight_train,
    control = rpart.control(cp = 0.005)
  )

# Plot the tree
rpart.plot(freight_tree,cex=.70)
fancyRpartPlot(freight_tree,cex=.9,tweak=.5,main="",sub="",palettes="Oranges")
DiagrammeR(diagram="freight_tree",type="freight_tree")

# get predictions
tree_pred <- predict(freight_tree, freight_test,  type = "matrix")
head(tree_pred)
RMSE(pred = tree_pred, obs = freight_test$BREAKDOWN) #TREE
RMSE(pred = rf_pred, obs = freight_test$BREAKDOWN) #RF 
sum((tree_pred - freight_test$BREAKDOWN)^2)
summary(freight_tree)

# Try it on full dataset??
# freight_full_tree_data <- freight_forest %>% 
#  select(-UNITNUMBER,-ACTIVCODE)

# freight_full_tree_data2 <- freight_full_tree_data %>% 
#  select(-Total,-HOURS)

# freight_full_tree <-
#  rpart(
#    BREAKDOWN ~ .,
#    method = "anova",
#    data = freight_full_tree_data,
#    control = rpart.control(cp = 0.005)
#  )
# Plot the tree
# prp(freight_full_tree)

# Random forest time
grid <- expand.grid(.mtry = 25)
ctrl <- trainControl(method = "cv",number = 10,selectionFunction = "oneSE")

# Train the model
rf.mod <-
  train(
    BREAKDOWN ~ .,
    data = freight_train,
    method = "rf",
    metric = "RMSE",
    trControl = ctrl,
    tuneGrid = grid,
    importance=T
  )

rf.mod

# Model Predictions
# preds_rf <- predict(rf.mod, freight_test3, type = "raw")
# test <- freight_test2$BREAKDOWN

# Variable Importance
varImp(rf.mod)

# Visualization
rf2 <- rfsrc(formula = BREAKDOWN ~ ., 
             data = data.frame(freight_train),
             mtry = rf.mod$finalModel$mtry)
gg_v <- gg_variable(rf.mod)
gg_md <- gg_minimal_depth(rf.mod)
xvar <-  gg_md$topvars[1:2]
plot(gg_v, xvar=xvar, panel=TRUE, partial=TRUE, alpha=.1)

# LIME
# Explanation of lime found here: https://m-clark.github.io/introduction-to-machine-learning/opening-the-black-box.html#trees-forests
rf_lime <- lime(freight_train,rf.mod)
rf_explain <- explain(freight_test,
                      rf_lime,
                      feature_select = 'auto',n_features = 3)
glimpse(rf_explain)
plot_explanations(rf_explain)

rf_pred <- predict(rf.mod, freight_test) # predictions

sqrt(sum(rf_pred - freight_test$BREAKDOWN)^2) #RMSE

# Random Forest Explainer
# Explanation of randomForestExplainer found here: https://m-clark.github.io/introduction-to-machine-learning/opening-the-black-box.html#trees-forests
plot_min_depth_distribution(rf.mod$finalModel)
plot_min_depth_interactions(rf.mod$finalModel, k=7) # only do 7 best interactions, take a very long time to run
multi_imps <-  measure_importance(rf.mod$finalModel)
plot_importance_ggpairs(multi_imps)





#library(modelStudio)
#library(DALEX)
# Understanding Model Predictions
#explainer <- lime(freight_train2,model=rf.mod)
#explanation <- explain(freight_test2,rf.mod)
# explanation
#explainer <- DALEX::explain(model = rf.mod,
#                            data = freight_test2,
#                            y = freight_test2,
#                            verbose = FALSE)
#modelStudio(explainer,freight_test2)