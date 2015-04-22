require("caret")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

tr <- read.csv("pml-training.csv")
ts <- read.csv("pml-testing.csv")

tr <- tr[,colSums(is.na(tr)) == 0]
ts <- ts[,colSums(is.na(ts)) == 0]

trc <- tr[, colnames(tr)[colnames(tr) %in% colnames(ts)]]
trc$classe  <- tr$classe

head(trc, 1)

p <- prcomp(data.matrix(trc[, 8:59]), center=T, scale=T)
summary(p)

model <- train(classe ~ ., data=trc[, 8:60], method="knn", preProcess=c("pca"), trControl = trainControl(method = "cv", number=10))
a <- predict(model, newdata =ts)
pml_write_files(a)