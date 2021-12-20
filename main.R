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



   
p1 <- combined[combined$painting == "charge_of_the_scots_greys _at_waterloo ",]
p2 <- combined[combined$painting == "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709",]
p3 <- combined[combined$painting == "Enrique_Simonet_-_El_barbero_del_zoco_-_18976",]
p4 <- combined[combined$painting == "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294",]
p5 <- combined[combined$painting == "flower_painting-Claude_Monet-Water_Lilies-1917-89996",]
p6 <- combined[combined$painting == "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453",]
p7 <- combined[combined$painting == "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004",]
p8 <- combined[combined$painting == "Paul_delvaux_the_viaducto",]

pp1 <- ggplot(p1, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "William Merritt Chase - The Olive Grove - 1910\nQuestions combined",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp2 <- ggplot(p2, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Paul Delvaux - The Viaducto\nQuestions combined",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp3 <- ggplot(p3, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Claude Monet - Water Lilies - 1917\nQuestions combined",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp4 <- ggplot(p4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Valentin Serov - Iphigenia in Tauris - 1893\nQuestions combined",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp5 <- ggplot(p5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Charge of the scots greys at waterloo\nQuestions combined",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp6 <- ggplot(p6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Alfred Sisley - Snow at Louveciennes - 1878\nQuestions combined",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp7 <- ggplot(p7, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Enrique Simonet - El barbero del zoco - 1897\nQuestions combined",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp8 <- ggplot(p8, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "John Lavery - The Fairy Fountain\nQuestions combined",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()





p1q4 <- q4[q4$painting == "charge_of_the_scots_greys _at_waterloo ",]
p2q4 <- q4[q4$painting == "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709",]
p3q4 <- q4[q4$painting == "Enrique_Simonet_-_El_barbero_del_zoco_-_1897",]
p4q4 <- q4[q4$painting == "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294",]
p5q4 <- q4[q4$painting == "flower_painting-Claude_Monet-Water_Lilies-1917-89996",]
p6q4 <- q4[q4$painting == "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453",]
p7q4 <- q4[q4$painting == "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004",]
p8q4 <- q4[q4$painting == "Paul_delvaux_the_viaducto",]

pp1q4 <- ggplot(p1q4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Charge of the scots greys at waterloo\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp2q4 <- ggplot(p2q4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Alfred Sisley - Snow at Louveciennes - 1878\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp3q4 <- ggplot(p3q4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Enrique Simonet - El barbero del zoco - 1897\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp4q4 <- ggplot(p4q4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "John Lavery - The Fairy Fountain\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp5q4 <- ggplot(p5q4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Claude Monet - Water Lilies - 1917\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp6q4 <- ggplot(p6q4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "William Merritt Chase - The Olive Grove - 1910\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp7q4 <- ggplot(p7q4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Valentin Serov - Iphigenia in Tauris - 1893\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp8q4 <- ggplot(p8q4, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Paul Delvaux - The Viaducto\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()




p1q5 <- q5[q5$painting == "charge_of_the_scots_greys _at_waterloo ",]
p2q5 <- q5[q5$painting == "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709",]
p3q5 <- q5[q5$painting == "Enrique_Simonet_-_El_barbero_del_zoco_-_1897",]
p4q5 <- q5[q5$painting == "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294",]
p5q5 <- q5[q5$painting == "flower_painting-Claude_Monet-Water_Lilies-1917-89996",]
p6q5 <- q5[q5$painting == "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453",]
p7q5 <- q5[q5$painting == "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004",]
p8q5 <- q5[q5$painting == "Paul_delvaux_the_viaducto",]

pp1q5 <- ggplot(p1q5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Charge of the scots greys at waterloo\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp2q5 <- ggplot(p2q5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Alfred Sisley - Snow at Louveciennes - 1878\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp3q5 <- ggplot(p3q5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Enrique Simonet - El barbero del zoco - 1897\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp4q5 <- ggplot(p4q5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "John Lavery - The Fairy Fountain\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp5q5 <- ggplot(p5q5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Claude Monet - Water Lilies - 1917\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp6q5 <- ggplot(p6q5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "William Merritt Chase - The Olive Grove - 1910\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp7q5 <- ggplot(p7q5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Valentin Serov - Iphigenia in Tauris - 1893\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp8q5 <- ggplot(p8q5, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Paul Delvaux - The Viaducto\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()




p1q6 <- q6[q6$painting == "charge_of_the_scots_greys _at_waterloo ",]
p2q6 <- q6[q6$painting == "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709",]
p3q6 <- q6[q6$painting == "Enrique_Simonet_-_El_barbero_del_zoco_-_1897",]
p4q6 <- q6[q6$painting == "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294",]
p5q6 <- q6[q6$painting == "flower_painting-Claude_Monet-Water_Lilies-1917-89996",]
p6q6 <- q6[q6$painting == "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453",]
p7q6 <- q6[q6$painting == "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004",]
p8q6 <- q6[q6$painting == "Paul_delvaux_the_viaducto",]

pp1q6 <- ggplot(p1q6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Charge of the scots greys at waterloo\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp2q6 <- ggplot(p2q6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Alfred Sisley - Snow at Louveciennes - 1878\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp3q6 <- ggplot(p3q6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Enrique Simonet - El barbero del zoco - 1897\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp4q6 <- ggplot(p4q6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "John Lavery - The Fairy Fountain\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp5q6 <- ggplot(p5q6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Claude Monet - Water Lilies - 1917\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp6q6 <- ggplot(p6q6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "William Merritt Chase - The Olive Grove - 1910\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp7q6 <- ggplot(p7q6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Valentin Serov - Iphigenia in Tauris - 1893\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
    x = "Answer",
    y = "Count"
  ) +
  scale_fill_manual(values = model_colors) +
  theme_minimal()

pp8q6 <- ggplot(p8q6, aes(x = result, fill = result)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  ) +
  labs(
    title = "Paul Delvaux - The Viaducto\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
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

pp1
pp2
pp3
pp4
pp5
pp6
pp7
pp8

pp1q4
pp2q4
pp3q4
pp4q4
pp5q4
pp6q4
pp7q4
pp8q4

pp1q5
pp2q5
pp3q5
pp4q5
pp5q5
pp6q5
pp7q5
pp8q5

pp1q6
pp2q6
pp3q6
pp4q6
pp5q6
pp6q6
pp7q6
pp8q6

ggsave("plots/pq4.png", plot = pq4)
ggsave("plots/pq5.png", plot = pq5)
ggsave("plots/pq6.png", plot = pq6)

ggsave("plots/pp1q4.png", plot = pp1q4)
ggsave("plots/pp2q4.png", plot = pp2q4)
ggsave("plots/pp3q4.png", plot = pp3q4)
ggsave("plots/pp4q4.png", plot = pp4q4)
ggsave("plots/pp5q4.png", plot = pp5q4)
ggsave("plots/pp6q4.png", plot = pp6q4)
ggsave("plots/pp7q4.png", plot = pp7q4)
ggsave("plots/pp8q4.png", plot = pp8q4)

ggsave("plots/pp1q5.png", plot = pp1q5)
ggsave("plots/pp2q5.png", plot = pp2q5)
ggsave("plots/pp3q5.png", plot = pp3q5)
ggsave("plots/pp4q5.png", plot = pp4q5)
ggsave("plots/pp5q5.png", plot = pp5q5)
ggsave("plots/pp6q5.png", plot = pp6q5)
ggsave("plots/pp7q5.png", plot = pp7q5)
ggsave("plots/pp8q5.png", plot = pp8q5)

ggsave("plots/pp1q6.png", plot = pp1q6)
ggsave("plots/pp2q6.png", plot = pp2q6)
ggsave("plots/pp3q6.png", plot = pp3q6)
ggsave("plots/pp4q6.png", plot = pp4q6)
ggsave("plots/pp5q6.png", plot = pp5q6)
ggsave("plots/pp6q6.png", plot = pp6q6)
ggsave("plots/pp7q6.png", plot = pp7q6)
ggsave("plots/pp8q6.png", plot = pp8q6)

# Table
summary_table <- data.frame(
  row.names = c("Question 1", "Question 2", "Question 3"),
  "Amount of answers" = c(length(q4), length(q5), length(q6)),
  "Favorite Model" = c(q4_best_model, q5_best_model, q6_best_model),
  "Percentage descriptive or pleasant" = c(q4_percentage_not_none, q5_percentage_not_none, q6_percentage_not_none)
)
xtable(
  summary_table,
  label = "tab:summary_sec_eval",
  caption = "Overview of evaluation results",
  type = "latex"
)