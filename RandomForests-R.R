library("rpart")
library("rpart.plot")

library("ggplot2")

# Regression model (train)
reg_tree <- rpart(formula = Collection~.,
                  data = train, 
                  control = rpart.control(maxdepth = 3))

# Visualize decision tree
rpart.plot(reg_tree, box.palette = "RdBu", digits = -3)

# Model evaluation
test <- test %>%  mutate(pred = predict(reg_tree,test, type = "vector"),
                         dif_Coll_pred = Collection - pred,
                         n = 1:nrow(test)
                         ) 

colnames(test)

# Plot
ggplot(test) +
  geom_point(aes(x=n, y =dif_Coll_pred))+
  ggtitle("Diferencia entre Real y Predictor")

# MSE2 

MSE2 <- mean((test$pred-test$Collection)^2)
MSE2 

full_tree <- rpart(formula = Collection~.,
                   data = train, 
                   control = rpart.control(cp = 0))
rpart.plot(full_tree, box.palette = "RdBu", digits = -3)

printcp(full_tree)

plotcp(full_tree)
plotcp(reg_tree)


mincp <- full_tree$cptable[which.min(full_tree$cptable[,"xerror"]),"CP"]
mincp

mincp2 <- reg_tree$cptable[which.min(reg_tree$cptable[,"xerror"]),"CP"]
mincp2

# Tree prunning
prune_tree <- prune(full_tree, cp = mincp)
rpart.plot(prune_tree, box.palette = "RdBu", digits = -3)

prune_tree2 <- prune(full_tree, cp = mincp2)
rpart.plot(prune_tree2, box.palette = "RdBu", digits = -3)

# Test Validation
test <- test %>%  mutate(predfull_tree = predict(full_tree,test, type = "vector"),
                         predpruned = predict(prune_tree,test, type = "vector"),
                         predpruned2 = predict(prune_tree2,test, type = "vector")
                        ) 
colnames(test)
