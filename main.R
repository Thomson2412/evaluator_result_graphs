# install.packages("dplyr")
library(dplyr)

users <- read.csv("export_users_202112161232.csv")
View(users)

answers <- read.csv("export_answers_202112161232.csv")
View(answers)



q4 <- answers[answers$qType == 4,]
q5 <- answers[answers$qType == 5,]
q6 <- answers[answers$qType == 6,]

hist(q4$result)
hist(q5$result)
hist(q6$result)