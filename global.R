# Functions ----
ifna <- function(x, elseval) ifelse(is.na(x) || is.null(x), elseval, x)
inclMarkdown <- function(file) withMathJax(includeMarkdown(file))
sim_samp_cov <- function(a, b, n = 1000) {

  if (length(a) == 0 || length(b) == 0) {
    return(NA)
  } else {
    if (length(a) > length(b)) {
      s <- b
      l <- a
    } else {
      s <- a
      l <- b
    }
     # sample the shorter one
     res <- sapply(1:n, function(x) {
       rn <- sample(1:(length(l) - length(s)), 1)
       cov(s, l[rn:(rn + length(s) - 1)])
           #sample(s, length(l), replace = T))
     })
    #res <- cov(s, l[1:length(s)])
    return(mean(res))   
  }
}
getSel <- function(val, elseval){
  # check if its not NA etc, otherwise elseval
  val <- ifna(val, elseval)
  
  if (val > 1000) { # check if its out of bounds
    elseval
  } else {
    round(val, 0)
  }
}

# Data ----
cols <- c("red", "blue")
set.seed(123)
dat <- data.table(x = 1:1000,
                  stat = rnorm(1000, mean = 0.075))
dat[, non_stat := cumsum(dat$stat)]

base <<- 200:400 # Base Selection


# different time-series
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

# Plot-Functions ---- 
plot_hist <- function(hdat){
  
  bas <- hdat[type == "Base", y]
  sel <- hdat[type == "Selection", y]
  
  statdat <- rbind(data.table(mean = hdat[type == "Base", mean(y)],
                              sd = hdat[type == "Base", sd(y)],
                              yval = 0.51),
                   data.table(mean = hdat[type == "Selection", mean(y)],
                              sd = hdat[type == "Selection", sd(y)],
                              yval = 0.49))
   
  covval <- ifelse(length(bas) == length(sel),
                   cov(bas, sel),
                   sim_samp_cov(bas, sel, n = 1000))
  tdat <- data.table(x = hdat[, min(y) + diff(range(y))*0.1],
                     y = 0.45,
                     val = paste("cov = ", round(covval, 3)))
  
  ggplot() + 
    geom_histogram(data = hdat, aes(x = y, fill = type, y = ..density..), 
                   alpha = 0.5, position = "dodge") + 
    scale_fill_manual(name = "", values = cols) + 
    theme(legend.position = "none") + ylab("Frequency") + xlab("Value") + 
    geom_errorbarh(data = statdat, aes(x = mean, y = yval, 
                                       xmin = mean - sd, xmax = mean + sd),
                   color = cols) + 
    geom_vline(data = statdat, aes(xintercept = mean), color = cols, 
               linetype = "dashed") + 
    geom_text(data = tdat, aes(x = x, y = y, label = val), size = 3, 
              environment = environment())
}
plot_ts <- function(series) {
  ggplot(data = datstat, aes_string(x = "x", y = series)) + 
    geom_line() + xlab("Time") + 
    ggtitle(paste0(ifelse(series == "ts1", "", "Non-"),"Stationary Series "))
}
