set.seed(123)
library(data.table)
datstat <- data.table(x = 1:1000,
                      ts1 = rnorm(1000),
                      ts2 = cumsum(rnorm(1000)),
                      ts3 = c(rnorm(200),
                              rnorm(400, mean = 5),
                              rnorm(400)),
                      ts4 = c(rnorm(100),
                              rnorm(300, sd = 5),
                              rnorm(500),
                              rnorm(100)),
                      ts5 = sapply(1:1000, function(x) rnorm(1, sd = x/1000)),
                      ts6 = sapply(1:1000, function(x) rnorm(1, sd = (x - 500)^2))/1000000)

plot_fun <- function(series) {
  ggplot(data = datstat, aes_string(x = "x", y = series)) + 
    geom_line() + xlab("Time") + 
    ggtitle(paste0(ifelse(series == "ts1", "", "Non-"),"Stationary Series "))
}

p1 <- plot_fun("ts1")
p2 <- plot_fun("ts2")
p3 <- plot_fun("ts3")
p4 <- plot_fun("ts4")
p5 <- plot_fun("ts5")
p6 <- plot_fun("ts6")

#grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

# Tests
# 
# library(tseries)
# tests <- c("adf.test", "Box.test", "kpss.test", "PP.test")
# 
# sapply(tests, function(test){
#   apply(dat[, .(ts1, ts2, ts3, ts4, ts5, ts6)], 2, function(x) {
#   get(test)(x)$p.value})
# })

#      adf.test    Box.test kpss.test   PP.test
# ts1 0.0100000 0.386053779      0.10 0.0100000
# ts2 0.4195604 0.000000000      0.01 0.3260713
# ts3 0.5467517 0.000000000      0.01 0.0100000
# ts4 0.0100000 0.004360365      0.10 0.0100000
# ts5 0.0100000 0.033007310      0.10 0.0100000
# ts6 0.0100000 0.307453035      0.10 0.0100000
library(fractal)
apply(datstat[, .(ts1, ts2, ts3, ts4, ts5, ts6)], 2, stationarity)
