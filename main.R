# install.packages("tidyverse")
# install.packages("kableExtra")
# install.packages("xtable")
library(tidyverse)
library(xtable)

users <- read.csv("export_users_202112161232.csv")
questions <- read.csv("export_questions_202112171115.csv")
answers <- read.csv("export_answers_202112161232.csv")

names(users)[names(users) == "X_id"] <- "uId"
names(questions)[names(questions) == "X_id"] <- "qId"
names(answers)[names(answers) == "X_id"] <- "aId"

combined <- merge(answers, questions, by = "qId")
combined <- merge(combined, users, by = "cSessionId")

combined$result[combined$result == -1] <- "None"

combined$result[combined$qType %in% c(4, 5) & combined$result == 0] <- "Model 1"
combined$result[combined$qType %in% c(4, 5) & combined$result == 1] <- "Model 2"
combined$result[combined$qType %in% c(4, 5) & combined$result == 2] <- "Model 3"
combined$result[combined$qType %in% c(4, 5) & combined$result == 3] <- "Model 4"

combined$result[combined$qType == 6 & combined$result == 0] <- "Model 1"
combined$result[combined$qType == 6 & combined$result == 1] <- "Model 2"
combined$result[combined$qType == 6 & combined$result == 2] <- "Model 4"

paintings_list <- lapply(
  combined$content,
  function (x)(gsub('.{6}$', '', tail(strsplit(x, "/")[[1]], 1)))
)
combined$painting <- paintings_list

model_colors <- c(
  "Model 1" = "#ffcdd2",
  "Model 2" = "#e1bee7",
  "Model 3" = "#c5cae9",
  "Model 4" = "#b2dfdb",
  "None" = "#cfd8dc"
)

q4 <- combined[combined$qType == 4,]
q5 <- combined[combined$qType == 5,]
q6 <- combined[combined$qType == 6,]

pq4 <- ggplot(q4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Question 1:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pq5 <- ggplot(q5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Question 2:\nChoose the audio piece you find most pleasant to listen to.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pq6 <- ggplot(q6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Question 3:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

q4_best_model <- names(which.max(table(q4$result)))
q5_best_model <- names(which.max(table(q5$result)))
q6_best_model <- names(which.max(table(q6$result)))

q4_percentage_not_none <- (length(q4$result[q4$result != "None"]) / length(q4$result)) * 100
q5_percentage_not_none <- (length(q5$result[q5$result != "None"]) / length(q5$result)) * 100
q6_percentage_not_none <- (length(q6$result[q6$result != "None"]) / length(q6$result)) * 100

# Plots
pq4
pq5
pq6

# Table
summary_table <- data.frame(
  row.names = c("Question 1", "Question 2", "Question 3"),
  "Favorite Model" = c(q4_best_model, q5_best_model, q6_best_model),
  "percentage descriptive or pleasant" = c(q4_percentage_not_none, q5_percentage_not_none, q6_percentage_not_none)
)

xtable(
  summary_table,
  label = "tab:summary_sec_eval",
  caption = "Overview of evaluation results",
  type = "latex"
)