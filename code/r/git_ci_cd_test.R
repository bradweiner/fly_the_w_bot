##THIS IS A GIT TEST FOR CI/CD##

library(tidyverse)

d <- mtcars

d.agg <- d %>%
        group_by(cyl) %>%
        summarise(avg_mpg = mean(mpg,na.rm = T)) %>%
        as.data.frame(.) %>%
        write.csv("data/transformed/mtcars.csv",row.names = F)
