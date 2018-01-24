p1_comp <- prcomp(bf[,-15], scale. = T)
p1_comp$x
st1_dev <- p1_comp$sdev
pr1_var <- st1_dev^2

prop1_var <- pr1_var/sum(pr1_var)

plot(cumsum(prop1_var), xlab = "Principal Component", ylab = "cummulative measure", type = 'b')

train.pca_comp <- data.frame(y = bf[,15], p1_comp$x[,1:9])

mod = lm(y~ ., data = train.pca_comp)
summary(mod)
plot(mod)
newtrain.pca_comp <- data.frame(train.pca_comp[,-9])
mod2 = lm(y~ ., data = newtrain.pca_comp)
summary(mod2)

plot(mod2)
par(mfrow=c(2,2))


