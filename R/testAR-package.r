#' testAR
#'
#' @name testAR
#' @docType package

library( "ggplot2" )

qqplot.data <- function (vec) 
{
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  d <- data.frame(resids = vec)

  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)

}





dat.rdata

dat <- reshape2:::melt.data.frame(raw, id.vars=c("type", "time"), variable.name="measurement", value.name="uptake")

ggplot(dat, aes(x=time*2, y=uptake, colour=type) ) + geom_point() + geom_smooth(method="lm")

model.minimal <- lm(uptake ~time, dat)
model <- lm(uptake ~time * type, dat)

summary(model)

# qqplot.data(model$res)

anova(model.minimal, model)

dat$pred <- predict(model, dat)
dat$residuals <- dat$uptake-dat$pred




ggplot(dat, aes(x=residuals, fill=type)) + geom_histogram(binwidth=3, position="dodge")
plotit <- ggplot(dat, aes(x=time*2, y=residuals, colour=type) ) + geom_point() + geom_smooth()

dat$res.class <- "mid"
dat$res.class[dat$residuals>5] <- "high"
dat$res.class[dat$residuals<(-3)]<- "low"


model2 <- lm(uptake ~ time * type, dat[dat$time>4,])
model3 <- lm(uptake ~ time, dat[dat$time>4,])
anova(model2, model3)

# qqnorm(model2$res)
# qqnorm(model3$res)



