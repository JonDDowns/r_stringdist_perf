objs <- list("single int" = 1, "two ints" = 1:2, "first 10,000 ints" = 1:10000)
lapply(objs, function(x) c("Total size" = object.size(x), "Size per element" = object.size(x) / length(x)))
