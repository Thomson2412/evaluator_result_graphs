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

combined$result[combined$qType %in% c(4, 5) & combined$result == 0] <- "Model 1.4"
combined$result[combined$qType %in% c(4, 5) & combined$result == 1] <- "Model 2.4"
combined$result[combined$qType %in% c(4, 5) & combined$result == 2] <- "Model 3.4"
combined$result[combined$qType %in% c(4, 5) & combined$result == 3] <- "Model 4.4"

combined$result[combined$qType == 6 & combined$result == 0] <- "Model 4.1"
combined$result[combined$qType == 6 & combined$result == 1] <- "Model 4.2"
combined$result[combined$qType == 6 & combined$result == 2] <- "Model 4.4"

painting_list <- lapply(
  combined$content,
  function (x)(gsub('.{6}$', '', tail(strsplit(x, "/")[[1]], 1)))
)
combined$painting <- unlist(painting_list)


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

xr_plot <- function (p_data, x_obj, p_title, p_colors, x_blank=FALSE){
  p <- ggplot(p_data, aes_string(x = x_obj, fill = x_obj)) +
    geom_bar() +
    # geom_text(
    #   aes(label = ..count..),
    #   stat = "count",
    #   vjust = -.5
    # ) +
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
    scale_fill_manual(values = p_colors) +
    theme_minimal()
  if(x_blank){
    p <- p + theme(axis.text.x=element_blank())
  }
  return(p)
}

update_plot_list <- function (plot_data, name, width, height, plt_lst){
  update <- 0
  for (index in seq_len(length(plt_lst))){
    if(plt_lst[[index]][["name"]] == name){
      update <- index
      break
    }
  }
  new_item <- c(plot=list(plot_data), name=name, width=width, height=height)
  if(update == 0)
    plt_lst[[length(plt_lst) + 1]] <- new_item
  else
    plt_lst[[update]] <- new_item
  return(plt_lst)
}


#Create plots: Model choosen per questions
for (row in seq_len(nrow(question_df))){
  qtype <- question_df[row,]$type
  question <- question_df[row,]$question
  models <- unlist(question_df[row,]$models)
  filtered_data <- combined[combined$qType == qtype,]
  filtered_mc <- append(model_colors[names(model_colors) %in% models], model_colors[length(model_colors)])
  data_plot <- xr_plot(filtered_data, "result", question, filtered_mc)
  plot_list <- update_plot_list(data_plot, sprintf("pq%d", qtype), xr_width, xr_height, plot_list)
}


#Create plots: Model choosen per painting
for (row in seq_len(nrow(painting_names))){
  id <- painting_names[row,]$id
  name <- painting_names[row,]$name
  filtered_data <- combined[combined$painting == id,]
  data_plot <- xr_plot(filtered_data, "result", sprintf("%s\nQuestions combined", name), model_colors)
  plot_list <- update_plot_list(data_plot, sprintf("pp%d", row), xr_width, xr_height, plot_list)
}


#Create plots: Model choosen per questions per painting
for (qrow in seq_len(nrow(question_df))){
  qtype <- question_df[qrow,]$type
  question <- question_df[qrow,]$question
  models <- unlist(question_df[qrow,]$models)
  for (prow in seq_len(nrow(painting_names))){
    id <- painting_names[prow,]$id
    name <- painting_names[prow,]$name
    filtered_data <- combined[combined$qType == qtype & combined$painting == id,]
    plot_title <- sprintf("%s\n%s", name, question)
    filtered_mc <- append(model_colors[names(model_colors) %in% models], model_colors[length(model_colors)])
    data_plot <- xr_plot(filtered_data, "result", plot_title, filtered_mc)
    plot_list <- update_plot_list(data_plot, sprintf("pp%dq%d", prow, qtype), xr_width, xr_height, plot_list)
  }
}

#Create plots: Painting choosen per model per question
for (qrow in seq_len(nrow(question_df))){
  qtype <- question_df[qrow,]$type
  question <- question_df[qrow,]$question
  models <- unlist(question_df[qrow,]$models)
  for (model in models){
    filtered_data <- combined[combined$result == model & combined$qType == qtype,]
    plot_title <- sprintf("%s\n%s", model, question)
    filtered_pc <- painting_colors[names(painting_colors) %in% unique(filtered_data$painting)]
    data_plot <- xr_plot(filtered_data, "painting", plot_title, filtered_pc, TRUE)
    model_no_space <-gsub(" ", "", model, fixed = TRUE)
    model_nr <- str_remove(model_no_space, "Model")
    model_no_dot <- gsub(".", "-", model_nr, fixed = TRUE)
    name <- sprintf("pm%sq%d", model_no_dot, qtype)
    plot_list <- update_plot_list(data_plot, name, xp_width, xp_height, plot_list)
  }
}



# Show plots
for (plt in plot_list){
  print(plt[["plot"]])
}



# Save plots
for (plt in plot_list){
  ggsave(
    sprintf("plots/%s.png", plt[["name"]]),
    plot = plt[["plot"]],
    width = plt[["width"]],
    height = plt[["height"]]
  )
}



# Filter data
q4 <- combined[combined$qType == 4,]
q5 <- combined[combined$qType == 5,]
q6 <- combined[combined$qType == 6,]

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