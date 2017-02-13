# creates system files for testss
library(subfoldr)
library(ggplot2)

rm(list = ls())

data2 <- data.frame(a = 3:4)
mtcars <- datasets::mtcars

main <- file.path("inst", "output")
sub <- file.path("first", "second")

save_table(data2, caption = "A table", main = main, sub = sub, ask = FALSE)
save_table(mtcars, main = main, sub = sub, ask = FALSE)

sub <- file.path("first", "2nd", "third")

save_table(datasets::ToothGrowth, "TG", report = TRUE, main = main, sub = sub, ask = FALSE)

x <- 1 # tested later that not saved

save_object(main = main, sub = sub, ask = FALSE)

x <- 1 # tested later that not be saved

save_object(main = main, sub = "also1", is = function(x) TRUE, ask = FALSE)

template2 <- "
    model{
do stuff
}
"
save_template(template2, main = main, sub = sub, ask = FALSE)

open_window()
print(ggplot(data = datasets::mtcars, aes(x = cyl, y = mpg)) + geom_point())

save_plot("cylmpg", main = main, sub = sub, ask = FALSE)
