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
