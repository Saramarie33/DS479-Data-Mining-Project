flightdelays2 <- read.csv("flightdelay_cleandata.csv", stringsAsFactors = TRUE)
View(flightdelays2)
str(flightdelays2)
table(flightdelays2$OP_UNIQUE_CARRIER)
table(flightdelays2$ORIGIN)
summary(flightdelays2$DEP_DELAY)
summary(flightdelays2$CARRIER_DELAY)
summary(flightdelays2$NAS_DELAY)

hist(flightdelays2$DEP_DELAY)
hist(flightdelays2$DEP_TIME)

High = ifelse(flightdelays2$DEP_DELAY <= 30, "No", "Yes")
flightdelays2 = data.frame(flightdelays2, High)

View(flightdelays2)
flightdelays2$High <- as.factor(flightdelays2$High)

require(tree)

tree.flightdelays2 = tree(High~. -DEP_DELAY, data=flightdelays2)
summary(tree.flightdelays2)

tree.flightdelays2
plot(tree.flightdelays2)
text(tree.flightdelays2, pretty = 0)

set.seed(500)
train=sample(1:nrow(flightdelays2), 1000)
tree.flightdelays2 = tree(High~. - DEP_DELAY, flightdelays2, subset=train)
plot(tree.flightdelays2)
text(tree.flightdelays2, pretty = 0)

tree.flightdelays2
plot(tree.flightdelays2)
text(tree.flightdelays2)

tree.pred = predict(tree.flightdelays2, flightdelays2[-train,], type="class")
with(flightdelays2[-train,], table(tree.pred, High))

cv.flightdelays2 = cv.tree(tree.flightdelays2, FUN = prune.misclass)
cv.flightdelays2

plot(cv.flightdelays2)

prune.flightdelays2 = prune.misclass(tree.flightdelays2, best = 15)
plot(prune.flightdelays2)
text(prune.flightdelays2, pretty = 0)
tree.pred = predict(prune.flightdelays2, flightdelays2[-train,], type = "class")
with(flightdelays2[-train,], table(tree.pred, High))
