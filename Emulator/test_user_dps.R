

test1x <- c(0.00,   0.25,	 0.31,	 0.38,	 0.50,	 0.62,	 0.75,	 1.00)
test1y <- mock.model1(test1x)

test1dp <- cbind(test1x, test1y)
colnames(test1dp) <- c("x", "y")

run.optimal.dp(micro.sim.model=mock.model1, n.init.dp=8, max.num.dp=3,
               have.dp=TRUE, user.init.dp=test1dp)$res.GP.model