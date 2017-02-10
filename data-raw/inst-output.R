# creates system files for testss
library(subfoldr)

data2 <- data.frame(a = 3:4)
mtcars <- datasets::mtcars

main <- file.path("inst", "output")
sub <- file.path("first", "second")

save_table(data2, caption = "A table", main = main, sub = sub, ask = FALSE)
save_table(mtcars, main = main, sub = sub, ask = FALSE)

sub <- file.path("first", "2nd", "third")

save_table(datasets::ToothGrowth, "TG", report = TRUE, main = main, sub = sub, ask = FALSE)
