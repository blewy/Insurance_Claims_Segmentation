
*Please let me know of any suggestions or errors*  
  
  
  Read in the data:
  
  ```{r, warning=FALSE, message=FALSE}
library(ggplot2)
library(gridExtra)
library(readr)

train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")

```

Separate target, categorial and continous data

```{r}

target <- train$target
train$target <- train$ID <- NULL

cat.var.names <- colnames(train)[sapply(train, is.character)]
cont.var.names <- colnames(train)[sapply(train, is.numeric)]

train.cat <- train[, cat.var.names]
test.cat <- test[, cat.var.names]

train.cont <- train[, cont.var.names]
test.cont <- test[, cont.var.names]

train.cat <- as.data.frame(lapply(train.cat, factor))
test.cat <- as.data.frame(lapply(test.cat, factor))

```

Let's take a first look at the data:

```{r}
str(train.cont)
str(test.cont)

summary(train.cont)
summary(test.cont)

head(train)
head(test)

```

We see very much features have 20.0 as maximum value.

Take a look at the data with categorical features:

```{r}
str(train.cat)
str(test.cat)

summary(train.cat)
summary(test.cat)

```

Features v22, v56 and v125 have very much levels, so we probably we should use them as numeric 
features as a first stept.

What are the dimensions of the datasets?

```{r}
cat("Train data has", nrow(train), "rows and", ncol(train), "columns! \n")
cat("Test data has", nrow(test), "rows and", ncol(test), "columns! \n")

```

In the above structure commands we saw missing data, how much is it?


```{r}
sum(is.na(train)) / (nrow(train) * ncol(train))
sum(is.na(test)) / (nrow(test) * ncol(test))

apply(train, 2, function(x) { sum(is.na(x)) })
apply(test, 2, function(x) { sum(is.na(x)) })

```

We see about 34 Percent of missing data, this will need some work.

Can we see any different missing data structure depending on the target?

```{r}
train$target <- target
train.na.per.target <- sapply(sort(unique(train$target)), function(x) { apply(train[train$target == x, ], 2, function(y) { sum(is.na(y)) }) })

train.na.per.target
round(colSums(train.na.per.target) / sum(train.na.per.target), digits = 4)

```

If the target has the value 1 the number of missing data is about 3.4 times as high when the target
is 0.

Are there any constant columns?

```{r}
train.const <- sapply(train, function(x) { length(unique(x)) == 1 })
test.const <- sapply(test, function(x) { length(unique(x)) == 1 })
cat("Train data set - Number of constant columns:", sum(train.const), "\n")
cat("Test data set - Number of constant columns:", sum(test.const), "\n")
```


Densities of continous Features

```{r, warning=FALSE}
train.cont <- data.frame(train.cont, target=train$target)
plotDensity <- function(data.in, i) {
data <- data.frame(x=data.in[,i], Target=data.in$target)
p <- ggplot(data) + 
geom_line(aes(x=x), stat="density", size=1, alpha=1.0) +
xlab(colnames(data.in)[i]) + theme_light()
return (p)
}

doPlots <- function(data.in, fun, ii, ncol=3) {
pp <- list()
for (i in ii) {
p <- fun(data.in=data.in, i=i)
pp <- c(pp, list(p))
}
do.call("grid.arrange", c(pp, ncol=ncol))
}

for (i in 1:28) {
ii <- (1:4)+(4*(i-1))
doPlots(data.in=train.cont, fun=plotDensity, ii=ii, ncol=2)
}

```


Boxplots of continous Features depending on target

```{r, warning=FALSE}
plotBox <- function(data.in, i) {
data <- data.frame(y=data.in[,i], target=data.in$target)
p <- ggplot(data, aes(x=factor(target), y=y)) + geom_boxplot() + ylab(colnames(data.in)[i]) + theme_light()
return (p)
}

for (i in 1:28) {
ii <- (1:4)+(4*(i-1))
doPlots(data.in=train.cont, fun=plotBox, ii=ii, ncol=2)
}

```


Take a look at the target

```{r}
ggplot(train) + geom_bar(aes(factor(target))) + xlab("target") + theme_light()

```

