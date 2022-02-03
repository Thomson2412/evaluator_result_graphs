# install.packages("tidyverse")
# install.packages("kableExtra")
# install.packages("xtable")
# install.packages("scales")
# install.packages("pwr")
library(tidyverse)
library(xtable)
library(scales)
library(pwr)


# Prepare data
users <- read.csv("export_users_202112161232.csv")
questions <- read.csv("export_questions_202112171115.csv")
answers <- read.csv("export_answers_202112161232.csv")

names(users)[names(users) == "X_id"] <- "uId"
names(questions)[names(questions) == "X_id"] <- "qId"
names(answers)[names(answers) == "X_id"] <- "aId"

combined <- merge(answers, questions, by = "qId")
combined <- merge(combined, users, by = "cSessionId")

painting_list <- lapply(
  combined$content,
  function(x)(gsub('.{6}$', '', tail(strsplit(x, "/")[[1]], 1)))
)
combined$painting <- unlist(painting_list)

plot_df <- data.frame(combined)
plot_df$result[plot_df$result == -1] <- "None"

plot_df$result[plot_df$qType %in% c(4, 5) & plot_df$result == 0] <- "Model 1.4"
plot_df$result[plot_df$qType %in% c(4, 5) & plot_df$result == 1] <- "Model 2.4"
plot_df$result[plot_df$qType %in% c(4, 5) & plot_df$result == 2] <- "Model 3.4"
plot_df$result[plot_df$qType %in% c(4, 5) & plot_df$result == 3] <- "Model 4.4"

plot_df$result[plot_df$qType == 6 & plot_df$result == 0] <- "Model 4.1"
plot_df$result[plot_df$qType == 6 & plot_df$result == 1] <- "Model 4.2"
plot_df$result[plot_df$qType == 6 & plot_df$result == 2] <- "Model 4.4"


# Init variables and functions
ag_model_list <- c(
  "Model 1.4",
  "Model 2.4",
  "Model 3.4",
  "Model 4.4"
)

vp_model_list <- c(
  "Model 4.1",
  "Model 4.2",
  "Model 4.4"
)

question_df <- data.frame(
  "type" = c(4, 5, 6),
  "question" = c(
    "Question 1:\nChoose the audio piece you find most descriptive of the painting.",
    "Question 2:\nChoose the audio piece you find most pleasant to listen to.",
    "Question 3:\nChoose the audio piece you find most descriptive of the painting."
  ),
  "models" = I(list(ag_model_list, ag_model_list, vp_model_list))
)

painting_names <- data.frame(
  "id" = c(
    "charge_of_the_scots_greys _at_waterloo ",
    "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709",
    "Enrique_Simonet_-_El_barbero_del_zoco_-_1897",
    "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294",
    "flower_painting-Claude_Monet-Water_Lilies-1917-89996",
    "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453",
    "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004",
    "Paul_delvaux_the_viaducto"
  ),
  "name" = c(
    "Charge of the scots greys at waterloo",
    "Alfred Sisley - Snow at Louveciennes - 1878",
    "Enrique Simonet - El barbero del zoco - 1897",
    "John Lavery - The Fairy Fountain",
    "Claude Monet - Water Lilies - 1917",
    "William Merritt Chase - The Olive Grove - 1910",
    "Valentin Serov - Iphigenia in Tauris - 1893",
    "Paul Delvaux - The Viaducto"
  )
)

model_colors <- c(
  "Model 1.4" = "#ffcdd2",
  "Model 2.4" = "#e1bee7",
  "Model 3.4" = "#c5cae9",

  "Model 4.1" = "#cb9ca1",
  "Model 4.2" = "#af8eb5",

  "Model 1.1" = "#ffcdd2",
  "Model 2.2" = "#e1bee7",
  "Model 3.3" = "#c5cae9",

  "Model 4.4" = "#b2dfdb",

  "None" = "#cfd8dc"
)

painting_colors <- c(
  "charge_of_the_scots_greys _at_waterloo " = "#ffccbc",
  "cityscape-Alfred_Sisley-Snow_at_Louveciennes-1878-46709" = "#cfd8dc",
  "Enrique_Simonet_-_El_barbero_del_zoco_-_1897" = "#ffe0b2",
  "figurative-John_Lavery-The_Fairy_Fountain_Glasgow_International_Exhibition-1888-294" = "#bdbdbd",
  "flower_painting-Claude_Monet-Water_Lilies-1917-89996" = "#ffcdd2",
  "landscape-William_Merritt_Chase-The_Olive_Grove-1910-48453" = "#c8e6c9",
  "literary_painting-Valentin_Serov-Iphigenia_in_Tauris-1893-28004" = "#bbdefb",
  "Paul_delvaux_the_viaducto" = "#b2dfdb"
)

plot_list <- list()

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

xr_plot <- function(p_data, x_obj, p_title, p_colors, x_blank = FALSE) {
  p <- ggplot(p_data, aes_string(x = x_obj, fill = x_obj)) +
    geom_bar() +
    geom_text(
      aes(label = sprintf("%s (%s)", scales::percent(prop.table(stat(count))), stat(count))),
      stat = "count",
      vjust = -.5
    ) +
    labs(
      title = p_title,
      x = "Answer",
      y = "Count"
    ) +
    scale_y_continuous(breaks = integer_breaks()) +
    theme_minimal()
  if (!is.null(p_colors) & length(p_colors) > 0) {
    p <- p + scale_fill_manual(values = p_colors)
  }
  if (x_blank) {
    p <- p + theme(axis.text.x = element_blank())
  }
  return(p)
}

update_plot_list <- function(plot_data, name, width, height, plt_lst) {
  update <- 0
  for (index in seq_len(length(plt_lst))) {
    if (plt_lst[[index]][["name"]] == name) {
      update <- index
      break
    }
  }
  new_item <- c(plot = list(plot_data), name = name, width = width, height = height)
  if (update == 0)
    plt_lst[[length(plt_lst) + 1]] <- new_item
  else
    plt_lst[[update]] <- new_item
  return(plt_lst)
}

clipboard <- function(x, sep="\t", row.names=FALSE, col.names=TRUE){
  con <- pipe("xclip -selection clipboard -i", open="w")
  write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
  close(con)
}


#Create plots: Model choosen per questions
for (row in seq_len(nrow(question_df))) {
  qtype <- question_df[row,]$type
  question <- question_df[row,]$question
  models <- unlist(question_df[row,]$models)
  filtered_data <- plot_df[plot_df$qType == qtype,]
  filtered_mc <- append(model_colors[names(model_colors) %in% models], model_colors[length(model_colors)])
  data_plot <- xr_plot(filtered_data, "result", question, filtered_mc)
  plot_list <- update_plot_list(data_plot, sprintf("pq%d", qtype), xr_width, xr_height, plot_list)
}


#Create plots: Model choosen per painting
# for (row in seq_len(nrow(painting_names))){
#   id <- painting_names[row,]$id
#   name <- painting_names[row,]$name
#   filtered_data <- plot_df[plot_df == id,]
#   data_plot <- xr_plot(filtered_data, "result", sprintf("%s\nQuestions combined", name), model_colors)
#   plot_list <- update_plot_list(data_plot, sprintf("pp%d", row), xr_width, xr_height, plot_list)
# }


#Create plots: Model choosen per questions per painting
for (qrow in seq_len(nrow(question_df))) {
  qtype <- question_df[qrow,]$type
  question <- question_df[qrow,]$question
  models <- unlist(question_df[qrow,]$models)
  for (prow in seq_len(nrow(painting_names))) {
    id <- painting_names[prow,]$id
    name <- painting_names[prow,]$name
    filtered_data <- plot_df[plot_df$qType == qtype & combined$painting == id,]
    plot_title <- sprintf("%s\n%s", name, question)
    filtered_mc <- append(model_colors[names(model_colors) %in% models], model_colors[length(model_colors)])
    data_plot <- xr_plot(filtered_data, "result", plot_title, filtered_mc)
    plot_list <- update_plot_list(data_plot, sprintf("pp%dq%d", prow, qtype), xr_width, xr_height, plot_list)
  }
}

#Create plots: Painting choosen per model per question
for (qrow in seq_len(nrow(question_df))) {
  qtype <- question_df[qrow,]$type
  question <- question_df[qrow,]$question
  models <- unlist(question_df[qrow,]$models)
  for (model in models) {
    filtered_data <- plot_df[plot_df$result == model & combined$qType == qtype,]
    plot_title <- sprintf("%s\n%s", model, question)
    filtered_pc <- painting_colors[names(painting_colors) %in% unique(filtered_data$painting)]
    data_plot <- xr_plot(filtered_data, "painting", plot_title, filtered_pc, TRUE)
    model_no_space <- gsub(" ", "", model, fixed = TRUE)
    model_nr <- str_remove(model_no_space, "Model")
    model_no_dot <- gsub(".", "-", model_nr, fixed = TRUE)
    name <- sprintf("pm%sq%d", model_no_dot, qtype)
    plot_list <- update_plot_list(data_plot, name, xp_width, xp_height, plot_list)
  }
}


# Show plots
for (plt in plot_list) {
  print(plt[["plot"]])
}


# Save plots
for (plt in plot_list) {
  ggsave(
    sprintf("plots/%s.png", plt[["name"]]),
    plot = plt[["plot"]],
    width = plt[["width"]],
    height = plt[["height"]],
    bg = "white"
  )
}

# Filter data
q4 <- combined[combined$qType == 4,]
q5 <- combined[combined$qType == 5,]
q6 <- combined[combined$qType == 6,]

# H0: No synthesis method is considered more descriptive than the others
# H0: The synthesis method has no effect on the descriptiveness of the sonification
# H0: The results will show the same amount of votes for each model (or None)

# Create click no click matrix
q4_click_no_click <- matrix(nrow = 2, ncol = length(unique(q4$result)))
colnames(q4_click_no_click) <- c("None", ag_model_list)
rownames(q4_click_no_click) <- c("Click", "No click")
for (r in unique(q4$result)){
  q4_click_no_click[1, r + 2] <- nrow(q4[q4$result == r,])
  q4_click_no_click[2, r + 2] <- nrow(q4) - nrow(q4[q4$result == r,])
}

# Click no click Chi Square Test
q4_all_chisq <- chisq.test(q4_click_no_click, simulate.p.value = TRUE)
# X-squared = 9.0306, df = NA, p-value = 0.06347

# Click no click without None
chisq.test(q4_click_no_click[, 2:ncol(q4_click_no_click)], simulate.p.value = TRUE)
# X-squared = 8.7949, df = NA, p-value = 0.03498

# Click no click all models and None paired
q4_pairs <- combn(unique(q4$result), 2)
q4_cnc_pair_result <- matrix(nrow = length(unique(q4$result)), ncol = length(unique(q4$result)))
colnames(q4_cnc_pair_result) <- c("None", ag_model_list)
rownames(q4_cnc_pair_result) <- colnames(q4_cnc_pair_result)
for (col in seq_len(ncol(q4_pairs))) {
  model_x <- q4_pairs[, col][[1]]
  model_y <- q4_pairs[, col][[2]]
  val <- chisq.test(q4_click_no_click[, c(model_x + 2, model_y + 2)] ,simulate.p.value = TRUE)
  q4_cnc_pair_result[model_y + 2, model_x + 2] <- val[["p.value"]]
  q4_cnc_pair_result[model_x + 2, model_y + 2] <- val[["p.value"]]
}

# Check if there are enough samples
# TODO: Power analysis
# ----------------------------------------------------------------------------------------------------------------------
pwr.chisq.test(
  sqrt(q4_all_chisq[["statistic"]][[1]] / (nrow(q4) * (ncol(q4_click_no_click) - 1))),
  nrow(q4),
  length(unique(q4$result) - 1)
)
# ----------------------------------------------------------------------------------------------------------------------



# If we differ the painting or the participant we do not expect a change in the the discriptiveness of the models
summary(aov(result ~ cSessionId, q4))
# cor.test(q4$result, as.numeric(factor(q4$cSessionId)))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  21  43.46   2.069    1.45   0.18
# Residuals   27  38.54   1.427
# length(unique(q4$cSessionId))

summary(aov(result ~ painting, q4))
# cor.test(q4$result, as.numeric(factor(q4$painting)))
#              Df Sum Sq Mean Sq F value Pr(>F)
# painting     7  12.67   1.810    1.07    0.4
# Residuals   41  69.33   1.691

summary(aov(result ~ cSessionId + painting, q4))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  21  43.46   2.069   1.372  0.242
# painting     7   8.37   1.196   0.793  0.602
# Residuals   20  30.17   1.508

summary(aov(result ~ cSessionId, q4[q4$result != -1,]))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  17  23.39   1.376   1.154   0.37
# Residuals   22  26.21   1.192
# length(unique(q4$cSessionId))

summary(aov(result ~ painting, q4[q4$result != -1,]))
#              Df Sum Sq Mean Sq F value Pr(>F)
# painting     7  17.75  2.5357   2.548 0.0333 *
# Residuals   32  31.85  0.9953

summary(aov(result ~ cSessionId + painting, q4[q4$result != -1,]))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  17  23.39  1.3756   1.477  0.226
# painting     7  12.24  1.7492   1.878  0.145
# Residuals   15  13.97  0.9313

q4merge <- data.frame(q4)
q4merge[q4merge$result == 2,] <- 3
summary(aov(result ~ cSessionId + painting, q4merge[q4merge$result != -1,]))
#             Df Sum Sq Mean Sq F value  Pr(>F)
# cSessionId  17  55.48   3.263   4.716 0.00164 **
# painting     6   6.23   1.038   1.500 0.24058
# Residuals   16  11.07   0.692
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# H0: No synthesis method is considered more pleasant than the others
# H0: The synthesis method has no effect on the pleasantness of the sonification
# H0: The results will show the same amount of votes for each model (or None)

# Create click no click matrix
q5_click_no_click <- matrix(nrow = 2, ncol = length(unique(q5$result)))
colnames(q5_click_no_click) <- c("None", ag_model_list)
rownames(q5_click_no_click) <- c("Click", "No click")
for (r in unique(q5$result)){
  q5_click_no_click[1, r + 2] <- nrow(q5[q5$result == r,])
  q5_click_no_click[2, r + 2] <- nrow(q5) - nrow(q5[q5$result == r,])
}

# Click no click Chi Square Test
chisq.test(q5_click_no_click, simulate.p.value = TRUE)
# X-squared = 29.623, df = NA, p-value = 0.0004998

# Click no click without None
chisq.test(q5_click_no_click[, 2:ncol(q5_click_no_click)], simulate.p.value = TRUE)
# X-squared = 18.74, df = NA, p-value = 0.001999

# Click no click all models and None paired
q5_pairs <- combn(unique(q5$result), 2)
q5_cnc_pair_result <- matrix(nrow = length(unique(q5$result)), ncol = length(unique(q5$result)))
colnames(q5_cnc_pair_result) <- c("None", ag_model_list)
rownames(q5_cnc_pair_result) <- colnames(q5_cnc_pair_result)
for (col in seq_len(ncol(q5_pairs))) {
  model_x <- q5_pairs[, col][[1]]
  model_y <- q5_pairs[, col][[2]]
  val <- chisq.test(q5_click_no_click[, c(model_x + 2, model_y + 2)] ,simulate.p.value = TRUE)
  q5_cnc_pair_result[model_y + 2, model_x + 2] <- val[["p.value"]]
  q5_cnc_pair_result[model_x + 2, model_y + 2] <- val[["p.value"]]
}

# If we differ the painting or the participant we do not expect a change in the the discriptiveness of the models
summary(aov(result ~ cSessionId, q5))
#cor.test(q5$result, as.numeric(factor(q5$cSessionId)))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  20  26.32  1.3160   1.452  0.169
# Residuals   32  29.00  0.9062

summary(aov(result ~ painting, q5))
#cor.test(q5$result, as.numeric(factor(q5$painting)))
#             Df Sum Sq Mean Sq F value Pr(>F)
# painting     7   3.61  0.5152   0.448  0.866
# Residuals   45  51.71  1.1492

summary(aov(result ~ cSessionId + painting, q5))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  20  26.32  1.3160   1.319  0.253
# painting     7   4.06  0.5800   0.581  0.764
# Residuals   25  24.94  0.9976

summary(aov(result ~ cSessionId, q5[q5$result != -1,]))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  19   15.5  0.8158   0.859  0.629
# Residuals   30   28.5  0.9500

summary(aov(result ~ painting, q5[q5$result != -1,]))
#              Df Sum Sq Mean Sq F value Pr(>F)
# painting     7   5.29  0.7551   0.819  0.577
# Residuals   42  38.71  0.9218

summary(aov(result ~ cSessionId + painting, q5[q5$result != -1,]))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  19 15.500  0.8158   0.781  0.706
# painting     7  4.465  0.6378   0.610  0.742
# Residuals   23 24.035  1.0450


# H0: No visual processing method is considered more descriptive than the others
# H0: The visual processing method has no effect on the descriptiveness of the sonification
# H0: The results will show the same amount of votes for each model (or None)

# Create click no click matrix
q6_click_no_click <- matrix(nrow = 2, ncol = length(unique(q6$result)))
colnames(q6_click_no_click) <- c("None", vp_model_list)
rownames(q6_click_no_click) <- c("Click", "No click")
for (r in unique(q6$result)){
  q6_click_no_click[1, r + 2] <- nrow(q6[q6$result == r,])
  q6_click_no_click[2, r + 2] <- nrow(q6) - nrow(q6[q6$result == r,])
}

# Click no click Chi Square Test
chisq.test(q6_click_no_click, simulate.p.value = TRUE)
# X-squared = 7.6078, df = NA, p-value = 0.05397

# Click no click without None
chisq.test(q6_click_no_click[, 2:ncol(q6_click_no_click)], simulate.p.value = TRUE)
# X-squared = 6.973, df = NA, p-value = 0.03248

# Click no click all models and None paired
q6_pairs <- combn(unique(q6$result), 2)
q6_cnc_pair_result <- matrix(nrow = length(unique(q6$result)), ncol = length(unique(q6$result)))
colnames(q6_cnc_pair_result) <- c("None", vp_model_list)
rownames(q6_cnc_pair_result) <- colnames(q6_cnc_pair_result)
for (col in seq_len(ncol(q6_pairs))) {
  model_x <- q6_pairs[, col][[1]]
  model_y <- q6_pairs[, col][[2]]
  val <- chisq.test(q6_click_no_click[, c(model_x + 2, model_y + 2)] ,simulate.p.value = TRUE)
  q6_cnc_pair_result[model_y + 2, model_x + 2] <- val[["p.value"]]
  q6_cnc_pair_result[model_x + 2, model_y + 2] <- val[["p.value"]]
}

# If we differ the painting or the participant we do not expect a change in the the discriptiveness of the models
summary(aov(result ~ cSessionId, q6))
#cor.test(q6$result, as.numeric(factor(q6$cSessionId)))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  22  28.94   1.315   0.861  0.636
# Residuals   28  42.75   1.527

summary(aov(result ~ painting, q6))
#cor.test(q6$result, as.numeric(factor(q6$painting)))
#             Df Sum Sq Mean Sq F value  Pr(>F)
# painting     7  27.38   3.911   3.795 0.00273 **
# Residuals   43  44.31   1.030
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(aov(result ~ cSessionId + painting, q6))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  22  28.94   1.315   1.604 0.1420
# painting     7  25.52   3.646   4.445 0.0036 **
# Residuals   21  17.23   0.820
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(aov(result ~ cSessionId, q6[q6$result != -1,]))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  18  14.05  0.7807   1.099  0.414
# Residuals   21  14.92  0.7106

summary(aov(result ~ painting, q6[q6$result != -1,]))
#              Df Sum Sq Mean Sq F value Pr(>F)
# painting     7  16.03  2.2896   5.659 0.000258 ***
# Residuals   32  12.95  0.4046
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


summary(aov(result ~ cSessionId + painting, q6[q6$result != -1,]))
#             Df Sum Sq Mean Sq F value Pr(>F)
# cSessionId  18  14.05  0.7807   2.584 0.0386 *
# painting     7  10.69  1.5275   5.055 0.0049 **
# Residuals   14   4.23  0.3022
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



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
  "Amount of answers" = c(nrow(q4), nrow(q5), nrow(q6)),
  "Favorite Model" = c(q4_best_model, q5_best_model, q6_best_model),
  "Percentage descriptive or pleasant" = c(q4_percentage_not_none, q5_percentage_not_none, q6_percentage_not_none)
)

xtable(
  summary_table,
  label = "tab:summary_sec_eval",
  caption = "Overview of evaluation results",
  type = "latex"
)


# Old stuff

# model_control <- data.frame(combined)
# model_control <- model_control[model_control$qType != 5,]
# # model_control[model_control$result %in% c("Model 1.4", "Model 4.1"),]$result <- "Model 1.1"
# # model_control[model_control$result %in% c("Model 2.4", "Model 4.2"),]$result <- "Model 2.2"
# # model_control[model_control$result %in% c("Model 3.4", "Model 4.2"),]$result <- "Model 3.3"
# # model_control[model_control$result %in% c("Model 4.4", "Model 4.4"),]$result <- "Model 4.4"
#
#
# for (prow in seq_len(nrow(painting_names))){
#   id <- painting_names[prow,]$id
#   name <- painting_names[prow,]$name
#   filtered_data <- combined[combined$painting == id,]
#   pxq4r_list <- filtered_data[filtered_data$qType == 4 & filtered_data$result != -1,]$result
#   pxq6r_list <- filtered_data[filtered_data$qType == 6 & filtered_data$result != -1,]$result
#
#   print(pxq4r_list)
#   print("---------------")
#   print(pxq6r_list)
#
#   if(length(pxq4r_list) != length(pxq6r_list)){
#     print("WHOOPS")
#   }
#   else{
#     for (index in seq_len(pxq4r_list)){
#       pxq4r <- pxq4r_list[[index]]
#       pxq6r <- pxq6r_list[[index]]
#       print(pxq4r)
#       print("---------------")
#       print(pxq6r)
#       print(sprintf("%s.%s", pxq4r, pxq6r))
#     }
#   }
# }
#
#
# summary(aov(result ~ qType + Error(cSessionId/result), combined[combined$qType != 5,]))
# summary(aov(result ~ qType, combined[combined$qType != 5,]))
# summary(aov(result ~ qType + painting, combined[combined$qType != 5,]))
# summary(aov(result ~ qType, combined[combined$qType != 5,]))
# summary(aov(result ~ qType + painting + Error(cSessionId/result), combined[combined$qType != 5,]))
#
# summary(aov(result ~ painting + Error(cSessionId/result), combined[combined$qType == 4,]))
# summary(aov(result ~ qType + painting + Error(cSessionId/result), combined[combined$result != -1 & combined$qType != 5,]))
# summary(aov(result ~ qType + painting, combined[combined$result != -1 & combined$qType != 5,]))
#
# summary(aov(result ~ painting + cSessionId, combined[combined$result != -1 & combined$qType == 5,]))
#
# summary(aov(result ~ cSessionId, combined[combined$result != -1 & combined$qType == 4,]))
# summary(aov(result ~ cSessionId, combined[combined$result != -1 & combined$qType == 5,]))
# summary(aov(result ~ cSessionId, combined[combined$result != -1 & combined$qType == 6,]))
#
# summary(aov(result ~ painting, combined[combined$qType == 4,]))
# summary(aov(result ~ painting, combined[combined$qType == 5,]))
# summary(aov(result ~ painting, combined[combined$qType == 6,]))
#
# summary(aov(result ~ painting + cSessionId, combined[combined$qType == 4,]))
# summary(aov(result ~ painting + cSessionId, combined[combined$qType == 5,]))
# summary(aov(result ~ painting + cSessionId, combined[combined$qType == 6,]))




# # All models and None
# chisq.test(c(
#   nrow(q4[q4$result == 0,]),
#   nrow(q4[q4$result == 1,]),
#   nrow(q4[q4$result == 2,]),
#   nrow(q4[q4$result == 3,]),
#   nrow(q4[q4$result == -1,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 7.2245, df = NA, p-value = 0.1344
# 
# # All models no None
# chisq.test(c(
#   nrow(q4[q4$result == 0,]),
#   nrow(q4[q4$result == 1,]),
#   nrow(q4[q4$result == 2,]),
#   nrow(q4[q4$result == 3,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 7, df = NA, p-value = 0.07496
# 
# # All models vs None
# chisq.test(c(
#   nrow(q4[q4$result == 0,]) +
#     nrow(q4[q4$result == 1,]) +
#     nrow(q4[q4$result == 2,]) +
#     nrow(q4[q4$result == 3,]),
#   nrow(q4[q4$result == -1,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 19.612, df = 1, p-value = 9.486e-06
# 
# # Mean models vs None
# chisq.test(c(
#   (nrow(q4[q4$result == 0,]) +
#     nrow(q4[q4$result == 1,]) +
#     nrow(q4[q4$result == 2,]) +
#     nrow(q4[q4$result == 3,])) / 4,
#   nrow(q4[q4$result == -1,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 0.052632, df = NA, p-value = 1
# 
# # All Models and None paired
# q4_chisq_pair_result <- matrix(nrow = 5, ncol = 5)
# colnames(q4_chisq_pair_result) <- c("None", ag_model_list)
# rownames(q4_chisq_pair_result) <- colnames(q4_chisq_pair_result)
# for (col in seq_len(ncol(q4_pairs))) {
#   pair_x <- q4_pairs[, col][[1]]
#   pair_y <- q4_pairs[, col][[2]]
#   val <- chisq.test(c(
#     nrow(q4[q4$result == pair_x,]),
#     nrow(q4[q4$result == pair_y,])),
#                     simulate.p.value = TRUE
#   )
#   q4_chisq_pair_result[pair_y + 2, pair_x + 2] <- val[["p.value"]]
#   q4_chisq_pair_result[pair_x + 2, pair_y + 2] <- val[["p.value"]]
# }


# # All models and None
# chisq.test(c(
#   nrow(q5[q4$result == 0,]),
#   nrow(q5[q4$result == 1,]),
#   nrow(q5[q4$result == 2,]),
#   nrow(q5[q4$result == 3,]),
#   nrow(q5[q4$result == -1,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 7.6604, df = NA, p-value = 0.1079
# 
# # All models no None
# chisq.test(c(
#   nrow(q5[q5$result == 0,]),
#   nrow(q5[q5$result == 1,]),
#   nrow(q5[q5$result == 2,]),
#   nrow(q5[q5$result == 3,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 14.32, df = NA, p-value = 0.003998
# 
# # All models vs None
# chisq.test(c(
#   nrow(q5[q5$result == 0,]) +
#     nrow(q5[q5$result == 1,]) +
#     nrow(q5[q5$result == 2,]) +
#     nrow(q5[q5$result == 3,]),
#   nrow(q5[q5$result == -1,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 41.679, df = NA, p-value = 0.0004998
# 
# # Mean models vs None
# chisq.test(c(
#   (nrow(q5[q5$result == 0,]) +
#     nrow(q5[q5$result == 1,]) +
#     nrow(q5[q5$result == 2,]) +
#     nrow(q5[q5$result == 3,])) / 4,
#   nrow(q5[q5$result == -1,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 5.8226, df = NA, p-value = 0.007496
# 
# # All Models and None paired
# q5_pairs <- combn(unique(q5$result), 2)
# q5_chisq_pair_result <- matrix(nrow = 5, ncol = 5)
# colnames(q5_chisq_pair_result) <- c("None", ag_model_list)
# rownames(q5_chisq_pair_result) <- colnames(q5_chisq_pair_result)
# for (col in seq_len(ncol(q5_pairs))) {
#   pair_x <- q5_pairs[, col][[1]]
#   pair_y <- q5_pairs[, col][[2]]
#   val <- chisq.test(c(
#     nrow(q5[q5$result == pair_x,]),
#     nrow(q5[q5$result == pair_y,])),
#                     simulate.p.value = TRUE
#   )
#   q5_chisq_pair_result[pair_y + 2, pair_x + 2] <- val[["p.value"]]
#   q5_chisq_pair_result[pair_x + 2, pair_y + 2] <- val[["p.value"]]
# }


# # All models and None
# chisq.test(c(
#   nrow(q6[q6$result == 0,]),
#   nrow(q6[q6$result == 1,]),
#   nrow(q6[q6$result == 2,]),
#   nrow(q6[q6$result == -1,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 5.7059, df = NA, p-value = 0.1239
# 
# # All models no None
# chisq.test(c(
#   nrow(q6[q6$result == 0,]),
#   nrow(q6[q6$result == 1,]),
#   nrow(q6[q6$result == 2,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 5.15, df = 2, p-value = 0.07615
# 
# # All models vs None
# chisq.test(c(
#   nrow(q6[q6$result == 0,]) +
#     nrow(q6[q6$result == 1,]) +
#     nrow(q6[q6$result == 2,]),
#   nrow(q6[q6$result == -1,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 16.49, df = NA, p-value = 0.0004998
# 
# # Mean models vs None
# chisq.test(c(
#   (nrow(q6[q6$result == 0,]) +
#     nrow(q6[q6$result == 1,]) +
#     nrow(q6[q6$result == 2,])) / 3,
#   nrow(q6[q6$result == -1,])),
#            simulate.p.value = TRUE
# )
# # X-squared = 0.22374, df = NA, p-value = 0.5457
# 
# # All Models and None paired
# q6_pairs <- combn(unique(q6$result), 2)
# q6_chisq_pair_result <- matrix(nrow = 4, ncol = 4)
# colnames(q6_chisq_pair_result) <- c("None", vp_model_list)
# rownames(q6_chisq_pair_result) <- colnames(q6_chisq_pair_result)
# for (col in seq_len(ncol(q6_pairs))) {
#   pair_x <- q6_pairs[, col][[1]]
#   pair_y <- q6_pairs[, col][[2]]
#   val <- chisq.test(c(
#     nrow(q6[q6$result == pair_x,]),
#     nrow(q6[q6$result == pair_y,])),
#                     simulate.p.value = TRUE
#   )
#   q6_chisq_pair_result[pair_y + 2, pair_x + 2] <- val[["p.value"]]
#   q6_chisq_pair_result[pair_x + 2, pair_y + 2] <- val[["p.value"]]
# }