#### Pika!!!
library(dplyr)
#ds1 <- read.csv(file = "/Users/linzexiang/Desktop/R study/Pokemon.csv")
ds1 <- read.csv(file = "C:\\Users\\81065\\OneDrive\\Desktop\\R study\\Pokemon.csv")
lv1 = levels(ds1[1,3])
lv2 = levels(ds1[1,4])
lv3 = append(lv1, lv2)
lv4 = unique(lv3)
name = ds1[,2]
some = as.matrix(ds1, ncol=13, byrow=F)
ds2 <- data.frame(matrix(unlist(ds1), ncol=13, byrow=F), stringsAsFactors = TRUE)
fin = data.frame(some, ds2[,3], ds2[,4])
samp1 <- fin[sample(nrow(fin)),]

### convert to matrix and data.frame without losing the string 
## understand Decision Tree and SVM (Especially the model algorithm and how the computer classifies)
#how people can use it mathmaticalys


###Drom here, we will work on SLRM(simple linear regression model)


###
Gen1 <- ifelse(samp1$Generation==1,1,0)
Gen2 <- ifelse(samp1$Generation==2,1,0)
Gen3 <- ifelse(samp1$Generation==3,1,0)
Gen4 <- ifelse(samp1$Generation==4,1,0)
Gen5 <- ifelse(samp1$Generation==5,1,0)
Gen6 <- ifelse(samp1$Generation==6,1,0)
samp2 = cbind(samp1, Gen1, Gen2,Gen3,Gen4,Gen5,Gen6)

samp1 <- fin[sample(nrow(fin)),]

test <- samp2[641:800,]

training <- samp2[sample(nrow(samp2)),][1:600,]




#2/1/2019
# Classification Tree with rpart
library(rpart)
# grow tree 
pok_tree <- rpart(Gen1 ~ HP + Attack + Defense+Type.1+Type.2,
             method="class", data=training)

printcp(pok_tree) # display the results 
plotcp(pok_tree) # visualize cross-validation results 
summary(pok_tree) # detailed summary of splits

# plot tree 
plot(pok_tree, uniform=TRUE, 
     main="Classification Tree for poke")
text(pok_tree, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(pok_tree, file = "C:\\Users\\81065\\OneDrive\\Desktop\\tree.ps", #c:/tree.ps
     title = "Classification Tree for Kyphosis")



#2/4/2019
# SVM
library(e1071)
poke_svm = svm(formula = Gen1 ~ HP + Attack + Defense + Type.1 +Type.2, 
                 data = training, 
                 type = 'C-classification', 
                 kernel = 'linear') 
summary(poke_svm)
test_svm <- predict(poke_svm, test)
test_af <- cbind(test["Gen1"], test_svm)

