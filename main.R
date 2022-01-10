# install.packages("tidyverse")
# install.packages("kableExtra")
# install.packages("xtable")
# install.packages("scales")
library(tidyverse)
library(xtable)
library(scales)


# Prepare data
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
combined$painting <- unlist(paintings_list)


# Init variables and functions
model_colors <- c(
  "Model 1" = "#ffcdd2",
  "Model 2" = "#e1bee7",
  "Model 3" = "#c5cae9",
  "Model 4" = "#b2dfdb",
  "None" = "#cfd8dc"
)

xr_width <- 6
xr_height <- 6

xp_width <- 12
xp_height <- 6

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

xr_plot <- function (p_data, p_title, p_colors){
  pp <- ggplot(p_data, aes(x = result, fill = result)) +
    geom_bar() +
    geom_text(
      aes(label = ..count..),
      stat = "count",
      vjust = -.5
    ) +
    labs(
      title = p_title,
      x = "Answer",
      y = "Count"
    ) +
    scale_fill_manual(values = p_colors) +
    scale_y_continuous(breaks = integer_breaks()) +
    theme_minimal()
  return(pp)
}

xp_plot <- function (p_data, p_title, p_colors){
  pp <- ggplot(p_data, aes(x = painting, fill = painting)) +
    geom_bar() +
    geom_text(
      aes(label = ..count..),
      stat = "count",
      vjust = -.5
    ) +
    labs(
      title = p_title,
      x = "Painting",
      y = "Count"
    ) +
    # scale_fill_manual(values = p_colors) +
    scale_y_continuous(breaks = integer_breaks()) +
    theme_minimal() +
    theme(axis.text.x=element_blank())
  return(pp)
}



# Filter data
q4 <- combined[combined$qType == 4,]
q5 <- combined[combined$qType == 5,]
q6 <- combined[combined$qType == 6,]
#Create plots
pq4 <- xr_plot(q4, "Question 1:\nChoose the audio piece you find most descriptive of the painting.", model_colors)
pq5 <- xr_plot(q5, "Question 2:\nChoose the audio piece you find most pleasant to listen to.", model_colors)
pq6 <- xr_plot(q6, "Question 3:\nChoose the audio piece you find most descriptive of the painting.", model_colors)



# Filter data
p1 <- combined[combined$painting == "charge_of_the_scots_greys _at_waterloo ",]
p2 <- combined[combined$painting == "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709",]
p3 <- combined[combined$painting == "Enrique_Simonet_-_El_barbero_del_zoco_-_18976",]
p4 <- combined[combined$painting == "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294",]
p5 <- combined[combined$painting == "flower_painting-Claude_Monet-Water_Lilies-1917-89996",]
p6 <- combined[combined$painting == "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453",]
p7 <- combined[combined$painting == "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004",]
p8 <- combined[combined$painting == "Paul_delvaux_the_viaducto",]
# Create plots
pp1 <- xr_plot(p1, "William Merritt Chase - The Olive Grove - 1910\nQuestions combined", model_colors)
pp2 <- xr_plot(p2, "Paul Delvaux - The Viaducto\nQuestions combined", model_colors)
pp3 <- xr_plot(p3, "Claude Monet - Water Lilies - 1917\nQuestions combined", model_colors)
pp4 <- xr_plot(p4, "Valentin Serov - Iphigenia in Tauris - 1893\nQuestions combined", model_colors)
pp5 <- xr_plot(p5, "Charge of the scots greys at waterloo\nQuestions combined", model_colors)
pp6 <- xr_plot(p6, "Alfred Sisley - Snow at Louveciennes - 1878\nQuestions combined", model_colors)
pp7 <- xr_plot(p7, "Enrique Simonet - El barbero del zoco - 1897\nQuestions combined", model_colors)
pp8 <- xr_plot(p8, "John Lavery - The Fairy Fountain\nQuestions combined", model_colors)



# Filter data
p1q4 <- q4[q4$painting == "charge_of_the_scots_greys _at_waterloo ",]
p2q4 <- q4[q4$painting == "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709",]
p3q4 <- q4[q4$painting == "Enrique_Simonet_-_El_barbero_del_zoco_-_1897",]
p4q4 <- q4[q4$painting == "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294",]
p5q4 <- q4[q4$painting == "flower_painting-Claude_Monet-Water_Lilies-1917-89996",]
p6q4 <- q4[q4$painting == "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453",]
p7q4 <- q4[q4$painting == "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004",]
p8q4 <- q4[q4$painting == "Paul_delvaux_the_viaducto",]
# Create plots
pp1q4 <- xr_plot(
  p1q4,
  "Charge of the scots greys at waterloo\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp2q4 <- xr_plot(
  p2q4,
  "Alfred Sisley - Snow at Louveciennes - 1878\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp3q4 <- xr_plot(
  p3q4,
  "Enrique Simonet - El barbero del zoco - 1897\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp4q4 <- xr_plot(
  p4q4,
  "John Lavery - The Fairy Fountain\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp5q4 <- xr_plot(
  p5q4,
  "Claude Monet - Water Lilies - 1917\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp6q4 <- xr_plot(
  p6q4,
  "William Merritt Chase - The Olive Grove - 1910\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp7q4 <- xr_plot(
  p7q4,
  "Valentin Serov - Iphigenia in Tauris - 1893\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp8q4 <- xr_plot(
  p8q4,
  "Paul Delvaux - The Viaducto\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)


# Filter data
p1q5 <- q5[q5$painting == "charge_of_the_scots_greys _at_waterloo ",]
p2q5 <- q5[q5$painting == "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709",]
p3q5 <- q5[q5$painting == "Enrique_Simonet_-_El_barbero_del_zoco_-_1897",]
p4q5 <- q5[q5$painting == "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294",]
p5q5 <- q5[q5$painting == "flower_painting-Claude_Monet-Water_Lilies-1917-89996",]
p6q5 <- q5[q5$painting == "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453",]
p7q5 <- q5[q5$painting == "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004",]
p8q5 <- q5[q5$painting == "Paul_delvaux_the_viaducto",]
# Create plots
pp1q5 <- xr_plot(
  p1q5,
  "Charge of the scots greys at waterloo\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)
pp2q5 <- xr_plot(
  p2q5,
  "Alfred Sisley - Snow at Louveciennes - 1878\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)
pp3q5 <- xr_plot(
  p3q5,
  "Enrique Simonet - El barbero del zoco - 1897\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)
pp4q5 <- xr_plot(
  p4q5,
  "John Lavery - The Fairy Fountain\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)
pp5q5 <- xr_plot(
  p5q5,
  "Claude Monet - Water Lilies - 1917\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)
pp6q5 <- xr_plot(
  p6q5,
  "William Merritt Chase - The Olive Grove - 1910\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)
pp7q5 <- xr_plot(
  p7q5,
  "Valentin Serov - Iphigenia in Tauris - 1893\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)
pp8q5 <- xr_plot(
  p8q5,
  "Paul Delvaux - The Viaducto\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)



# Filter data
p1q6 <- q6[q6$painting == "charge_of_the_scots_greys _at_waterloo ",]
p2q6 <- q6[q6$painting == "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709",]
p3q6 <- q6[q6$painting == "Enrique_Simonet_-_El_barbero_del_zoco_-_1897",]
p4q6 <- q6[q6$painting == "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294",]
p5q6 <- q6[q6$painting == "flower_painting-Claude_Monet-Water_Lilies-1917-89996",]
p6q6 <- q6[q6$painting == "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453",]
p7q6 <- q6[q6$painting == "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004",]
p8q6 <- q6[q6$painting == "Paul_delvaux_the_viaducto",]
# Create plots
pp1q6 <- xr_plot(
  p1q6,
  "Charge of the scots greys at waterloo\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp2q6 <- xr_plot(
  p2q6,
  "Alfred Sisley - Snow at Louveciennes - 1878\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp3q6 <- xr_plot(
  p3q6,
  "Enrique Simonet - El barbero del zoco - 1897\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp4q6 <- xr_plot(
  p4q6,
  "John Lavery - The Fairy Fountain\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp5q6 <- xr_plot(
  p5q6,
  "Claude Monet - Water Lilies - 1917\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp6q6 <- xr_plot(
  p6q6,
  "William Merritt Chase - The Olive Grove - 1910\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
   model_colors
)
pp7q6 <- xr_plot(
  p7q6,
  "Valentin Serov - Iphigenia in Tauris - 1893\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pp8q6 <- xr_plot(
  p8q6,
  "Paul Delvaux - The Viaducto\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)



# Filter data
m1q4 <- q4[q4$result == "Model 1",]
m2q4 <- q4[q4$result == "Model 2",]
m3q4 <- q4[q4$result == "Model 3",]
m4q4 <- q4[q4$result == "Model 4",]
# Create plots
pm1q4 <- xp_plot(
  m1q4,
  "Model 1\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pm2q4 <- xp_plot(
  m2q4,
  "Model 2\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pm3q4 <- xp_plot(
  m3q4,
  "Model 3\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pm4q4 <- xp_plot(
  m4q4,
  "Model 4\nQuestion 1:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)


# Filter data
m1q5 <- q5[q5$result == "Model 1",]
m2q5 <- q5[q5$result == "Model 2",]
m4q5 <- q5[q5$result == "Model 4",]
# Create plots
pm1q5 <- xp_plot(
  m1q5,
  "Model 1\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)
pm2q5 <- xp_plot(
  m2q5,
  "Model 2\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)
pm4q5 <- xp_plot(
  m4q5,
  "Model 4\nQuestion 2:\nChoose the audio piece you find most pleasant to listen to.",
  model_colors
)



# Filter data
m1q6 <- q6[q6$result == "Model 1",]
m2q6 <- q6[q6$result == "Model 2",]
m3q6 <- q6[q6$result == "Model 3",]
m4q6 <- q6[q6$result == "Model 4",]
# Create plots
pm1q6 <- xp_plot(
  m1q6,
  "Model 1\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pm2q6 <- xp_plot(
  m2q6,
  "Model 2\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pm3q6 <- xp_plot(
  m3q6,
  "Model 3\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)
pm4q6 <- xp_plot(
  m4q6,
  "Model 4\nQuestion 3:\nChoose the audio piece you find most descriptive of the painting.",
  model_colors
)



# Show plots
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

pm1q4
pm2q4
pm3q4
pm4q4

pm1q5
pm2q5
pm4q5

pm1q6
pm2q6
pm3q6
pm4q6



# Save plots
ggsave("plots/pq4.png", plot = pq4, width = xr_width, height = xr_height)
ggsave("plots/pq5.png", plot = pq5, width = xr_width, height = xr_height)
ggsave("plots/pq6.png", plot = pq6, width = xr_width, height = xr_height)

ggsave("plots/pp1q4.png", plot = pp1q4, width = xr_width, height = xr_height)
ggsave("plots/pp2q4.png", plot = pp2q4, width = xr_width, height = xr_height)
ggsave("plots/pp3q4.png", plot = pp3q4, width = xr_width, height = xr_height)
ggsave("plots/pp4q4.png", plot = pp4q4, width = xr_width, height = xr_height)
ggsave("plots/pp5q4.png", plot = pp5q4, width = xr_width, height = xr_height)
ggsave("plots/pp6q4.png", plot = pp6q4, width = xr_width, height = xr_height)
ggsave("plots/pp7q4.png", plot = pp7q4, width = xr_width, height = xr_height)
ggsave("plots/pp8q4.png", plot = pp8q4, width = xr_width, height = xr_height)

ggsave("plots/pp1q5.png", plot = pp1q5, width = xr_width, height = xr_height)
ggsave("plots/pp2q5.png", plot = pp2q5, width = xr_width, height = xr_height)
ggsave("plots/pp3q5.png", plot = pp3q5, width = xr_width, height = xr_height)
ggsave("plots/pp4q5.png", plot = pp4q5, width = xr_width, height = xr_height)
ggsave("plots/pp5q5.png", plot = pp5q5, width = xr_width, height = xr_height)
ggsave("plots/pp6q5.png", plot = pp6q5, width = xr_width, height = xr_height)
ggsave("plots/pp7q5.png", plot = pp7q5, width = xr_width, height = xr_height)
ggsave("plots/pp8q5.png", plot = pp8q5, width = xr_width, height = xr_height)

ggsave("plots/pp1q6.png", plot = pp1q6, width = xr_width, height = xr_height)
ggsave("plots/pp2q6.png", plot = pp2q6, width = xr_width, height = xr_height)
ggsave("plots/pp3q6.png", plot = pp3q6, width = xr_width, height = xr_height)
ggsave("plots/pp4q6.png", plot = pp4q6, width = xr_width, height = xr_height)
ggsave("plots/pp5q6.png", plot = pp5q6, width = xr_width, height = xr_height)
ggsave("plots/pp6q6.png", plot = pp6q6, width = xr_width, height = xr_height)
ggsave("plots/pp7q6.png", plot = pp7q6, width = xr_width, height = xr_height)
ggsave("plots/pp8q6.png", plot = pp8q6, width = xr_width, height = xr_height)

ggsave("plots/pm1q4.png", plot = pm1q4, width = xp_width, height = xp_height)
ggsave("plots/pm2q4.png", plot = pm2q4, width = xp_width, height = xp_height)
ggsave("plots/pm3q4.png", plot = pm3q4, width = xp_width, height = xp_height)
ggsave("plots/pm4q4.png", plot = pm4q4, width = xp_width, height = xp_height)
ggsave("plots/pm1q5.png", plot = pm1q5, width = xp_width, height = xp_height)
ggsave("plots/pm2q5.png", plot = pm2q5, width = xp_width, height = xp_height)
ggsave("plots/pm4q5.png", plot = pm4q5, width = xp_width, height = xp_height)
ggsave("plots/pm1q6.png", plot = pm1q6, width = xp_width, height = xp_height)
ggsave("plots/pm2q6.png", plot = pm2q6, width = xp_width, height = xp_height)
ggsave("plots/pm3q6.png", plot = pm3q6, width = xp_width, height = xp_height)
ggsave("plots/pm4q6.png", plot = pm4q6, width = xp_width, height = xp_height)



# Generate summary data
q4_best_model <- names(which.max(table(q4$result)))
q5_best_model <- names(which.max(table(q5$result)))
q6_best_model <- names(which.max(table(q6$result)))

q4_percentage_not_none <- (length(q4$result[q4$result != "None"]) / length(q4$result)) * 100
q5_percentage_not_none <- (length(q5$result[q5$result != "None"]) / length(q5$result)) * 100
q6_percentage_not_none <- (length(q6$result[q6$result != "None"]) / length(q6$result)) * 100



# Generate summary table
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