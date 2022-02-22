library("rpart")
library("rpart.plot")


# Regression model (train)
reg_tree <- rpart(formula = Collection~.,
                  data = train, 
                  control = rpart.control(maxdepth = 3))

# Visualize decision tree
rpart.plot(reg_tree, box.palette = "RdBu", digits = -3)
