library(tidyverse)
library(rvest)
library(stringr)


parse_clue <- function(clue) {
  if(length(html_nodes(clue, "td")) == 0) {
    return(tibble())
  }
  
  text <- clue %>% html_node(".clue_text") %>% html_text()
  daily_double <- length(html_nodes(clue, ".clue_value_daily_double")) > 0
  if(daily_double) {
    value <- clue %>% html_node(".clue_value_daily_double") %>%
      html_text() %>%
      str_replace("DD: \\$", "") %>%
      str_replace(",", "") %>%
      as.numeric
  } else {
    value <- clue %>%
      html_node(".clue_value") %>%
      html_text() %>%
      str_replace("\\$", "") %>%
      str_replace(",", "") %>%
      as.numeric
  }
  
  order <- clue %>% 
    html_node(".clue_order_number") %>% 
    html_text() %>%
    as.numeric
  
  names <- clue %>% 
    html_node("td > div") %>%
    html_attr("onmouseover") %>%
    str_match_all("<td class=\"(right|wrong)\">(.+?)</td>") %>%
    .[[1]] %>%
    as_tibble() %>%
    dplyr::select(-V1) %>%
    rename(name = V3, correct = V2) %>%
    mutate(correct = correct == "right") %>%
    filter(name != "Triple Stumper")
  
  answer <- clue %>%
    html_node("td > div") %>%
    html_attr("onmouseover") %>%
    str_match("<em class=\"correct_response\">(.+)</em>") %>% .[,2]
  
  category_index <- clue %>%
    html_nodes("tr td") %>%
    .[5] %>%
    html_attr('id') %>%
    str_match("clue_D?J_([1-6])") %>% 
    .[,2] %>%
    as.numeric
  
  question_index <- clue %>% 
    html_nodes("tr td") %>% 
    .[5] %>% 
    html_attr('id') %>% 
    str_match("clue_D?J_[1-6]_([1-6])") %>% 
    .[,2] %>%
    as.numeric
  
  return(tibble(name = list(names),
                value = value,
                order = order,
                text = text,
                category_index = category_index,
                answer = answer,
                question_index = question_index,
                daily_double = daily_double) %>%
           unnest(name))
}

parse_round <- function(xml, round_name) {
  clues <- xml %>%
    html_nodes(".clue") %>%
    map(parse_clue) %>%
    bind_rows() %>%
    mutate(round = round_name)
  
  categories <- xml %>%
    html_nodes(".category") %>%
    html_text() %>%
    str_trim() %>%
    str_replace("\\(.+?\\)", "") %>%
    as_tibble() %>%
    rename(category = value) %>%
    mutate(index = 1:6)
  
  clues %>%
    left_join(categories, by = c(category_index = "index")) %>%
    return()
}

parse_final_jeopardy <- function(page) {
  names = page %>%
    html_nodes("#final_jeopardy_round > table") %>%
    .[[2]] %>%
    html_nodes(".score_player_nickname") %>%
    html_text()
  
  table_index <- 2
  
  if(length(names) == 0) {
    names = page %>%
      html_nodes("#final_jeopardy_round > table") %>%
      .[[3]] %>%
      html_nodes(".score_player_nickname") %>%
      html_text()
    
    table_index <- 3
  }

  scores = page %>%
    html_nodes("#final_jeopardy_round > table") %>%
    .[[table_index]] %>%
    html_nodes("tr") %>%
    .[[2]] %>%
    html_nodes("td") %>%
    html_text() %>%
    str_trim() %>%
    str_replace_all("\\$|,", "") %>%
    as.numeric
  
  category = page %>%
    html_nodes("#final_jeopardy_round > table") %>%
    .[[1]] %>%
    html_node(".category > div") %>%
    html_text() %>%
    str_trim()
  
  text = page %>%
    html_nodes("#final_jeopardy_round > table") %>%
    .[[1]] %>%
    html_node(".clue") %>%
    html_text() %>%
    str_trim()
  
  answer = page %>%
    html_nodes("#final_jeopardy_round > table") %>%
    .[[1]] %>%
    html_node(".category > div") %>%
    html_attr("onmouseover") %>%
    str_match("<em class=\\\\\"correct_response\\\\\">(.+)</em>") %>% .[,2]
  
  correct = page %>%
    html_nodes("#final_jeopardy_round > table") %>%
    .[[1]] %>%
    html_node(".category > div") %>%
    html_attr("onmouseover") %>%
    str_match_all("<td class=\"(right|wrong)\">(.+?)</td>") %>%
    .[[1]] %>%
    as_tibble() %>%
    dplyr::select(-V1) %>%
    rename(name = V3, correct = V2) %>%
    mutate(correct = correct == "right")
  
  return(tibble(name = names,
                score = scores,
                category = category,
                text = text,
                answer = answer,
                daily_double = FALSE,
                category_index = NA,
                order = 1,
                game_question_index = 61,
                round = "Final Jeopardy") %>%
           left_join(correct))
}

parse_game <- function(page) {
  rounds <- page %>% html_nodes(".round")
  
  if(length(rounds) != 2) {
    return(data.frame())
  }
  
  clues <- bind_rows(
    parse_round(rounds[1], "Jeopardy"),
    parse_round(rounds[2], "Double Jeopardy")
  ) %>%
    arrange(desc(round), order) %>%
    mutate(game_question_order = 1:nrow(.),
           game_question_index = order + ifelse(round == "Double Jeopardy", 30, 0)) %>%
    group_by(name) %>%
    mutate(current_score = cumsum(correct * value) - cumsum((correct == FALSE) * value)) %>%
    ungroup()
  
  before_final <- clues %>%
    group_by(name) %>%
    top_n(1, game_question_index) %>% 
    dplyr::select(name, current_score)
  
  final_jeopardy <- parse_final_jeopardy(page)
  
  final_jeopardy_value <- left_join(before_final, final_jeopardy) %>%
    mutate(value = score - current_score,
           game_question_order = max(clues$game_question_order) + 1) %>%
    rename(current_score = score)
  
  bind_rows(clues, final_jeopardy_value)
}

parse_game_url <- function(url) {
  game_number = str_extract(url, "[0-9]{4}")
  
  file_name <- paste0("./game", game_number, ".csv")
  
  if(file.exists(file_name)) {
    print(paste0("reading cached data for ", game_number))
    return(read_csv(file_name))
  } else {
    print(paste0("downloading ", game_number))
    dat <- url %>%
      read_html() %>%
      parse_game()
    
    write_csv(dat, file_name)
    Sys.sleep(20)
    
    return(dat)
  }
}

parse_season <- function(xml, season_number) {
  links = xml %>% html_nodes("#content table td a")
  tibble(
    href = html_attr(links, 'href'),
    text = html_text(links)
  ) %>%
    filter(str_detect(href, "showgame\\.php")) %>%
    mutate(game_number = str_match(text, "\\#[0-9]+"),
           date = str_match(text, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>%
    mutate(game = map(href, parse_game_url),
           season = season_number) %>%
    unnest(game) %>%
    dplyr::select(-href, -text) %>%
    return()
}

parse_season_url <- function(url, season_number) url %>%
  read_html() %>%
  parse_season(season_number)

for(i in 27:32) {
  print(i)
  season <- parse_season_url(paste0("http://j-archive.com/showseason.php?season=", i), i)
  
  winners <- season %>%
    filter(game_question_index == 61) %>%
    group_by(game_number) %>%
    mutate(won = current_score == max(current_score)) %>%
    ungroup() %>%
    select(game_number, name, won)
  
  season <- season %>%
    left_join(winners, by = c("game_number", "name"))
  
  write_csv(season, paste0("./season", i, ".csv"))
}


