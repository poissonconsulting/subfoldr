# creates system files for testss
library(subfoldr)
library(ggplot2)

rm(list = ls())

data2 <- data.frame(a = 3:4)
mtcars <- datasets::mtcars

main2 <- file.path("inst", "output")
sub2 <- file.path("first", "second")

save_table(data2, caption = "A table", main = main2, sub = sub2, ask = FALSE)
save_table(mtcars, report = FALSE, main = main2, sub = sub2, ask = FALSE)
save_table(mtcars, "mtcars2", main = main2, sub = sub2, ask = FALSE)
save_table(mtcars, "mtcars3", caption = "Another table", main = main2, sub = sub2, ask = FALSE)

sub2 <- file.path("first", "2nd", "third")

save_table(mtcars, "mtcars", report = FALSE, main = main2, sub = sub2, ask = FALSE)
save_table(datasets::ToothGrowth, "TG", main = main2, sub = sub2, ask = FALSE)

x <- 1 # tested later that not saved

save_object(main = main2, sub = sub2, ask = FALSE)

x <- 1 # tested later that not be saved

save_object(main = main2, sub = "also1", is = function(x) TRUE, ask = FALSE)

template_2 <- "model{
do stuff
}
"
save_template(template_2, main = main2, sub = sub2, ask = FALSE)

open_window()
print(ggplot(data = datasets::mtcars, aes(x = cyl, y = mpg)) + geom_point())

save_plot("cylmpg", caption = "a fine plot", main = main2, sub = sub2, ask = FALSE)
