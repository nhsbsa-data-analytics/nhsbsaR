#' Estimate runtime
#'
#' Estimate runtime of fitting a computationally intensive model to a big dataset prior to the run itself, which, in some cases, may be measured in hours or days. The runtime is estimated by extrapolation from a best-fitting model (power, exponential or linear) fitted to a sample of runtimes in a small range.
#'
#' @param code String: code executing the model in one line. Should specify execution of an iterable subset of the full dataset. Usually, this is done by setting the data argument inside the model's function to, for example, `DT[1:i]` or `DT[sample(.N, i)]` for data.tables or `df[1:i,]` for data.frames.
#' @param subset_sizes Numeric vector: a range of subsets of the full dataset that have manageable running times (e.g. from several seconds to several minutes) that extends as far as practical into the full dataset. May require some trial-and-error to determine an optimal trade-off between the time it takes to produce an estimate and the accuracy of the estimate. As we would commonly want to estimate long runtimes fairly quickly, the accuracy won't be great, but the estimate would still be useful as a ballpark indicator.
#' @param full_size Numeric value: full size of the dataset, i.e. `nrow(DT)`. Has to be set manually, because we aren't passing the full dataset object to the function. Can also be set to any number to estimate the runtime of a model on a similar dataset of any size.
#'
#' @return Annotated `ggplot2` graph showing estimated runtime over the full dataset's size.
#'
#' @examples \dontrun{
#' #' library(data.table)
#' library(randomForest)
#'
#' n = 1e6
#'
#' DT <- data.table(OUTCOME = sample(c(0L,1L), n, replace = T) |> as.factor(),
#'                  FEATURE1 = sample(LETTERS[1:4], n, replace = T),
#'                  FEATURE2 = sample(LETTERS[5:8], n, replace = T),
#'                  FEATURE3 = sample(LETTERS[9:15], n, replace = T),
#'                  FEATURE4 = sample(LETTERS[1:10], n, replace = T),
#'                  FEATURE5 = runif(n, 1, 10) |> round(2),
#'                  FEATURE6 = runif(n, 20, 40) |> round(2),
#'                  FEATURE7 = rnorm(n, 50, 20) |> round(2),
#'                  FEATURE8 = rnorm(n, 100, 40) |> round(2))
#'
#' estimate_runtime(
#'   code = "randomForest(OUTCOME ~ ., data = DT[sample(.N, i)])",
#'   subset_sizes = c(2500,5000,10000,15000,25000,50000),
#'   full_size = n
#' )
#' }

#'
#' @export
estimate_runtime <- function(code, subset_sizes, full_size) {

  library(ggplot2)
  library(data.table)

  R <- data.frame()

  pb <- txtProgressBar(min = 0, max = length(subset_sizes), style = 3)
  step = 1

  for (i in subset_sizes) {

    # Start time
    start <- Sys.time()

    # Execute the code that needs timing
    eval(parse(text=code))

    # Record running time in seconds
    t <- as.numeric(Sys.time() - start, units = "secs")

    # Collect the results from each loop
    R <- rbind(R, data.frame(n = i, t = t))

    setTxtProgressBar(pb, step)
    step = step+1

  }

  close(pb)
  rm(pb, step, start,t)

  m1 <- lm(log(t)~log(n), data=R) # Power model
  m2 <- lm(log(t)~n, data=R) # Exponential model
  m3 <- lm(t~n, data=R) # Linear model

  r1 <- summary(m1)$r.squared
  r2 <- summary(m2)$r.squared
  r3 <- summary(m3)$r.squared

  timings <- data.table(n = seq(from = 1, to = full_size, by = round(full_size/100)))

  if (r1 >= r2 & r1 >= r3) {

    rel = "Power"

    r = r1
    a = coefficients(m1)[1] |> exp()
    p = coefficients(m1)[2]

    timings[, t := a*n^p]

  } else if (r2 >= r1 & r2 >= r3) {

    rel = "Exponential"

    r = r2
    a = coefficients(m2)[1] |> exp()
    b = coefficients(m2)[2] |> exp()

    timings[, t := a*b^n]

  } else if (r3 >= r1 & r3 >= r2) {

    rel = "Linear"

    r = r3
    c = coefficients(m3)[1]
    b = coefficients(m3)[2]

    timings[, t := b*n + c]

  } else {print("Error")}

  rm(m1,m2,m3)

  #Label
  if (timings[, max(t)] < 60) {
    lab = paste(paste0("'",rel), "relationship; estimated time =", timings[, round(max(t),1)], "sec;'", sep=" ")
  }
  if (timings[, max(t)] >= 60) {
    lab = paste(paste0("'",rel), "relationship; estimated time =", timings[, round(max(t/60),1)], "min;'", sep=" ")
  }
  if (timings[, max(t)] >= 3600) {
    lab = paste(paste0("'",rel), "relationship; estimated time =", timings[, round(max(t/3600),1)], "hrs;'", sep=" ")
  }
  if (timings[, max(t)] >= 3600*24) {
    lab = paste(paste0("'",rel), "relationship; estimated time =", timings[, round(max(t/3600/24),1)], "days;'", sep=" ")
  }

  r <- round(r, 3)

  ggplot(timings, aes(n,t/60)) +
    geom_line(lty=2) +
    geom_point(data=R) +
    scale_x_continuous(labels = \(x) {x/1e3}) +
    xlab("Rows (1000s)") +
    ylab("Time (min)") +
    annotate("text",
             x = timings[, max(n)*0.03],
             y = timings[, max(t/60)*0.94],
             label = paste0(lab, " ~italic(R) ^ 2 == ", r), parse=T, hjust=0) +
    theme_bw()

}
