ridge_test<-read.csv("ridge_test.csv")
ridge_train<-read.csv("ridge_train.csv")
save(ridge_test,file="../data/ridge_test.rda")
save(ridge_train,file="../data/ridge_train.rda")