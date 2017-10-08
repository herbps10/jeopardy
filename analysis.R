library(tidyverse)
library(ROCR)
seasons <- read_csv("./seasons.csv")

seasons %>% filter(game_number == "#5977") %>%
  ggplot(aes(x = game_question_index, y = current_score, color = name)) +
  geom_line() +
  geom_vline(xintercept = 30, linetype = "dashed") +
  geom_vline(xintercept = 60, linetype = "dashed") +
  scale_x_continuous(breaks = c(1, 15, 30, 45, 60), limits = c(1, 61)) +
  labs(y = "Score", x = "Question", caption = "Data: J-Archive.com\nherbsusmann.com", title = "Jeopardy! Game #5977") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

seasons <- seasons %>%
  mutate(won = ifelse(game_number %in% c("#6539", "#7216"), FALSE, won))

seasons %>%
  filter(game_question_index == 61, won == TRUE) %>%
  arrange(current_score) %>%
  select(game_number, name, current_score, season)

seasons %>%
  filter(game_question_index == 61) %>%
  mutate(won = factor(won, levels = c(TRUE, FALSE))) %>%
  ggplot(aes(x = current_score, fill = won)) +
  geom_density(aes(y = ..scaled..), alpha = 0.5)

seasons %>%
  mutate(name = paste(game_number, name)) %>%
  ggplot(aes(x = game_question_index, y = current_score, color = won)) +
  geom_smooth(se = TRUE) +
  geom_vline(xintercept = 30, linetype = "dashed") +
  geom_vline(xintercept = 60, linetype = "dashed") +
  scale_x_continuous(breaks = c(1, 15, 30, 45, 60), limits = c(1, 61)) +
  #scale_y_continuous(breaks = c(-5000, 0, 10000, 20000, 30000), labels = c("-$5,000", "$0", "$10,000", "$20,000", "$30,000")) +
  labs(x = "Question", y = "Score", caption = "Data: J-Archive.com\nherbsusmann.com", title = "Jeopardy! Season 33 Score Trajectories")

season33 <- seasons %>%
  filter(season == 33) %>%
  mutate(won = factor(won))

# seasons %>%
#   filter(game_question_index < 61) %>%
#   ggplot(aes(x = current_score, y = as.numeric(won))) +
#   geom_point() +
#   stat_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
#   facet_wrap(~game_question_index)

seasons <- seasons %>%
  group_by(game_number) %>%
  mutate(name_index = as.numeric(as.factor(name))) %>%
  ungroup()

carry_forward <- function(x) {
  if(is.na(x[1])) x[1] <- 0
  for(i in 1:(length(x) - 1)) {
    x[i + 1] = ifelse(is.na(x[i + 1]), x[i], x[i + 1])
  }
  x
}

games <- unique(seasons$game_number)
N_games <- length(games)
complete_games <- tibble(
  game_number = rep(games, each = 61 * 3)
) %>%
  mutate(name_index = rep(rep(1:3, each = 61), N_games),
         game_question_index = rep(1:61, N_games * 3)) %>%
  left_join(select(seasons, game_number, name_index, name, game_question_index, current_score, season, won)) %>%
  group_by(game_number, name_index) %>%
  mutate(current_score = carry_forward(current_score),
         season = carry_forward(season),
         name = unique(name[!is.na(name)]),
         won = unique(won[!is.na(won)])) %>%
  ungroup()



# Gives the other two opponents scores
# in decreasing order (so index = 1 will always give other opponent with higher score,
# index = 2 will give other opponent with lower score of the two opponents)
other_score <- function(current_score, index) {
  rep(list(current_score), 3) %>%
    map2(1:3, function(x, i) x[-i]) %>%
    map(sort, decreasing = TRUE) %>%
    map(`[`, index) %>%
    unlist()
}

complete_games <- complete_games %>%
  group_by(game_number, game_question_index) %>%
  mutate(distance_from_lead = max(current_score) - current_score,
         opponent_score_1 = other_score(current_score, 1),
         opponent_score_2 = other_score(current_score, 2),
         distance_to_closest = opponent_score_1 - current_score,
         rank = rank(current_score),
         rank_factor = factor(rank, levels = c(1.0, 1.5, 2.0, 2.5, 3.0), labels = c('Third', 'Tied for second', 'Second', 'Tied for first', 'First'))) %>%
  ungroup()


trajectory_plot <- function(dat, alpha = 0.1, title = "") dat %>%
  mutate(name = paste(game_number, name), won = factor(won, levels = c(TRUE, FALSE))) %>%
  ggplot(aes(x = game_question_index, y = current_score, color = won)) +
  geom_line(alpha = alpha, aes(group = paste(game_number, name))) +
  geom_vline(xintercept = 30, linetype = "dashed") +
  geom_vline(xintercept = 60, linetype = "dashed") +
  scale_x_continuous(breaks = c(1, 15, 30, 45, 60), limits = c(1, 61)) +
  labs(x = "Question", y = "Score", caption = "Data: J-Archive.com\nherbsusmann.com", title = title) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

trajectory_plot(complete_games, alpha = 0.01, title = "Jeopardy! Seasons 22-33 Score Trajectories")
ggsave("~/herbps10.github.io/public/images/season22-33-trajectories.png",
       trajectory_plot(complete_games, alpha = 0.01, title = "Jeopardy! Seasons 22-33 Score Trajectories"),
       width = 1600 / 200,
       height = 1600 / 200,
       units = "in",
       dpi = 200)

trajectory_plot(filter(complete_games, season == 27), alpha = 0.1, title = "Jeopardy! Seasons 27 Score Trajectories")
ggsave("~/herbps10.github.io/public/images/season27-trajectories.png",
       trajectory_plot(filter(complete_games, season == 27), alpha = 0.1, title = "Jeopardy! Seasons 27 Score Trajectories"),
       width = 1600 / 200,
       height = 1600 / 200,
       units = "in",
       dpi = 200)

complete_games %>%
  mutate(won = factor(won, levels = c(TRUE, FALSE))) %>%
  group_by(won, game_question_index) %>%
  summarize(y = median(current_score),
            y_lower = quantile(current_score, probs = c(0.05)),
            y_upper = quantile(current_score, probs = c(0.95))) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = game_question_index)) +
  geom_line(aes(y = y, color = won), size = 1.25) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper, fill = won), alpha = 0.25) +
  labs(x = "Question", y = "Current score", caption = "Data: J-Archive.com\nherbsusmann.com", title = "Season 22-33 Score Trajectories") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1, 15, 30, 45, 60), limits = c(1, 61), minor_breaks = c(5, 10, 20, 25, 35, 40, 50, 55))

ggsave("~/herbps10.github.io/public/images/season22-33-trajectories-ribbon.png",
       last_plot(),
       width = 1600 / 200,
       height = 1600 / 200,
       units = "in",
       dpi = 200)

training <- complete_games %>% filter(season != 33)
test <- complete_games %>% filter(season == 33)

make_model <- function(data, f) {
  glm(f, family=binomial(link="logit"), data = data)
}

predict_with_filter <- function(model, index) {
  predict(model, newdata = filter(test, game_question_index == index), type = 'response')
}

add_roc <- function(d, test, number) d %>%
  mutate(response = map2(model, game_question_index, predict_with_filter),
         prediction = map2(response, game_question_index, function(response, index) prediction(response, filter(test, game_question_index == index)$won)),
         performance = map(prediction, performance, measure = "tpr", x.measure = "fpr"),
         performance_df = pmap(list(game_question_index, performance, number), function(i, d, n) { tibble(model = n, game_question_index = i, x = d@x.values[[1]], y = d@y.values[[1]])}))

plot_roc <- function(models) {
  bind_rows(models$performance_df) %>%
    ggplot(aes(x,y)) +
    geom_line() +
    labs(x = "False Positive Rate", y = "True Positive Rate", caption = "Data: J-Archive.com\nherbsusmann.com") +
    facet_wrap(~game_question_index) +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
    scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"))
}

plot_rocs <- function(...) {
  list(...) %>%
    map(`[[`, "performance_df") %>%
    map(bind_rows) %>%
    bind_rows() %>%
    ggplot(aes(x, y, color = model)) +
    geom_line() +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
    scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
    labs(x = "False Positive Rate", y = "True Positive Rate", caption = "Data: J-Archive.com\nherbsusmann.com") +
    facet_wrap(~game_question_index)
}

models <- training %>%
  group_by(game_question_index) %>%
  nest() %>%
  mutate(model = map(data, make_model, won ~ current_score)) %>%
  add_roc(test, "current score")

plot_roc(models) +
  labs(title = "Logistic regression model results\nwon ~ current_score") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("~/herbps10.github.io/public/images/roc1.png",
       last_plot(),
       width = 1600 / 200,
       height = 1600 / 200,
       unit = "in",
       dpi = 200)

models2 <- complete_games %>%
  group_by(game_question_index) %>%
  nest() %>%
  mutate(model = map(data, make_model, won ~ rank_factor)) %>%
  add_roc(test, "rank")

models3 <- complete_games %>%
  group_by(game_question_index) %>%
  nest() %>%
  mutate(model = map(data, make_model, won ~ distance_to_closest)) %>%
  add_roc(test, "distance from lead")

# compare ROCs of different models
plot_rocs(models, models2, models3) +
  labs(title = "Model comparisons") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11))



null_model_predictions = test %>%
  mutate(prediction = rank == 3,
         true_positive = prediction == TRUE & won == TRUE,
         false_positive = prediction == TRUE & won == FALSE,
         false_negative = prediction == FALSE & won == TRUE,
         true_negative = prediction == FALSE & won == FALSE) %>%
  group_by(game_question_index) %>%
  summarize(tpr = sum(true_positive) / (sum(true_positive) + sum(false_negative)),
            fpr = sum(false_positive) / (sum(false_positive) + sum(true_negative))) %>%
  mutate(model = "current leader")

plot_rocs(models, models2, models3) +
  geom_point(data = null_model_predictions, aes(x = fpr, y = tpr)) + 
  labs(title = "Model comparisons") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11))

ggsave("~/herbps10.github.io/public/images/rocs1.png",
       last_plot(),
       width = 1600 / 200,
       height = 1600 / 200,
       units = "in",
       dpi = 200)

#
# Plot logistic curve for original set of models
#

models %>%
  filter(game_question_index == 30) %>%
  mutate(data = map2(model, data, function(model, data) mutate(data, fitted = predict(model)))) %>%
  .[["data"]] %>%
  .[[1]] %>%
  ggplot(aes(x = current_score, y = as.numeric(won))) +
  geom_point(size = 0.1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, formula = y ~ x) +
  labs(x = "Score", y = "Estimated probability of winning",
       title = "Logistic regression fit at question 30") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("~/herbps10.github.io/public/images/logistic-regression-curve-question-30.png",
       last_plot(),
       width = 1000 / 200,
       height = 1000 / 200,
       dpi = 200)

library(gridExtra)

plot_prediction <- function(game) {
  test_prediction <- test %>%
    filter(game_number == game) %>%
    group_by(game_question_index, name) %>%
    nest() %>%
    left_join(models3, by = c("game_question_index")) %>%
    mutate(p = map2(data.x, model, function(d, m) as.data.frame(predict(m, newdata = d, type = "response", se.fit = TRUE)))) %>%
    unnest(p) %>%
    mutate(fit_lower = fit - 1.95 * se.fit,
           fit_upper = fit + 1.95 * se.fit)
  
  p1 <- ggplot(test_prediction, aes(game_question_index)) +
    geom_line(aes(y = fit, color = name)) +
    #geom_ribbon(data = test_prediction, aes(ymin = fit_lower, ymax = fit_upper, group = name), alpha = 0.25) +
    labs(x = "", y = "Probability of winning", title = paste("Game", game, "Predictions")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_x_continuous(breaks = c(1, 15, 30, 45, 60), limits = c(1, 61), minor_breaks = c(5, 10, 20, 25, 35, 40, 50, 55)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%"))
  
  p2 <- test %>% filter(game_number == game) %>%
    ggplot(aes(game_question_index, current_score, color = name)) +
    geom_line() +
    labs(x = "Question", y = "Current score", caption = "Data: J-Archive.com\nherbsusmann.com", title = "Scores") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1, 15, 30, 45, 60), limits = c(1, 61), minor_breaks = c(5, 10, 20, 25, 35, 40, 50, 55))
  
  grid.arrange(p1, p2)
}


ggsave("~/herbps10.github.io/public/images/game7573.png",
       plot_prediction("#7573"),
       width = 1600 / 200,
       height = 1600 / 200,
       units = "in",
       dpi = 200)

ggsave("~/herbps10.github.io/public/images/game7544.png",
       plot_prediction("#7544"),
       width = 1600 / 200,
       height = 1600 / 200,
       units = "in",
       dpi = 200)

# How many times are their upsets? (leader going into FJ doesn't end up winning)
upset_table <- complete_games %>%
  filter(game_question_index == 60) %>%
  select(rank, won) %>%
  table()

seasons %>%
  filter(game_question_index == 61) %>%
  ggplot(aes(current_score - value, value, color = factor(won))) +
  geom_point(alpha = 0.25)

complete_games %>% 
  filter(game_question_index == 61) %>%
  ggplot(aes(y = current_score, x = opponent_score_1, color = factor(won))) +
  geom_point(alpha = 0.5)

complete_games %>% 
  filter(game_question_index == 61, won == TRUE) %>%
  ggplot(aes(y = current_score, x = opponent_score_1)) +
  geom_bin2d()

