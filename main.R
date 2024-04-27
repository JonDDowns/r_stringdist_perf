library(data.table)
library(ggplot2)
library(microbenchmark)
library(stringdist)

# Sample data
nrec <- 1000
options <- c("CAT", "HAT", "THAT", "SPAT", "SCENTED", "APRON", LETTERS)
dt <- data.table(ID = 1:nrec, left = sample(options, nrec, TRUE), right = sample(options, nrec, TRUE))

# String distance: check for equality if one string is only one character wide
fcase_by <- function(left, right) {
  fcase(
    nchar(left) == 1 | nchar(right) == 1,
    as.numeric(substr(left, 1, 1) != substr(right, 1, 1)),
    default = stringdist(left, right)
  )
}

# This throws an error
# dt[, res := fcase_by(left, right)]

# This is the "fix"
# dt[, res := fcase_by(left, right), by = ID]

# Same logic, but in for loop
sdist_loop <- function(left, right) {
  n <- length(left)
  out <- rep(NA, length(left))
  for (i in n) {
    if (nchar(left[i]) == 1 | nchar(right[i]) == 1) {
      out[i] <- as.numeric(substr(right[i], 1, 1) != substr(left[i], 1, 1))
    } else {
      out[i] <- stringdist(left, right)
    }
  }
  out
}

# Allows us to benchmark multiple datasizes
bench_by_nrec <- function(nrec) {
  options <- c("CAT", "HAT", "THAT", "SPAT", "SCENTED", "APRON", LETTERS)
  dt <- data.table(ID = 1:nrec, left = sample(options, nrec, TRUE), right = sample(options, nrec, TRUE))

  bench <- data.table(microbenchmark(
    "By + fcase" = dt[, res := fcase_by(left, right), by = ID],
    "For loop" = dt[, res2 := sdist_loop(left, right)],
    times = 10
  ))
  bench[, nrec := nrec]
  bench[, time := time / 1e9]
  bench[, list(expr, nrec, time)]
}

# Get benchmarks for select row counts
out <- do.call(rbind, lapply(seq(10000, 50000, by = 10000), bench_by_nrec))

# Visualize the results, draw trendline that assumes linear growth
p <- ggplot(out, aes(x = nrec, y = time, group = expr, color = expr)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma_format()) +
  ylab("Time (s)") +
  xlab("Number of rows") +
  guides(color = guide_legend("Function used")) +
  ggtitle("Benchmarking Two data.table stringdist Implementations") +
  scale_color_manual(values = c("gray40", "gray70"))

# Output for display
svglite::svglite("../jondownsnet_axum/rust/assets/images/dt_strdist_bench.svg", width = 7.0, height = 5)
plot(p)
dev.off()
