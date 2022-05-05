# functions to search dictionary

library(stringr)
library(tidyverse)
library(words)

# import scrabble dictionary
words5 <- words::words 

# filter 5 letter words
words5 <- words5%>%
  filter(word_length == 5)

# find all combinations of 5 words that search 25 unique letters
# note: no "vibex"

# form one-hot matrix of letters
onehot_words5 <- matrix(0,
                        nrow = nrow(words5),
                        ncol = length(letters))
for (i in 1:length(words5$word)){
  word_i <- words5$word[i]
  onehot_words5[i,] <- letters %in% unlist(str_extract_all(word_i, ""))
}

unique_letters <- rowSums(onehot_words5)
words5 <- cbind(words5,onehot_words5, unique_letters)
names(words5)[3:28] <- letters

# 5 letter word solver
five_solver <- function(word_str, match_n = 5, not_letters = "", words_full_df = words5){
  # check if word in list
  if (!word_str %in% words_full_df$word){
    warning("Word not in word list")
  }
  
  letters_used <- letters %in% unlist(str_extract_all(word_str, ""))
  letters_not_used <- letters %in% unlist(str_extract_all(not_letters, ""))
  
  overlap_matrix <- matrix(unlist(rep(letters_used, nrow(words_full_df))),
                           nrow = nrow(words_full_df),
                           byrow = TRUE) &
    as.matrix(words_full_df[,letters])
  overlap_words <- apply(overlap_matrix, 1, sum) >= match_n
  
  remaining_df <- words_full_df[overlap_words,]
  
  if (nrow(remaining_df) == 0){
    stop("no words found")
  }
  
  dont_match_matrix <- matrix(
    unlist(rep(letters_not_used, nrow(remaining_df))),
    nrow = nrow(remaining_df),
    byrow = TRUE) &
    as.matrix(remaining_df[,letters])
  dont_match_words <- apply(dont_match_matrix, 1, any)
  
  remaining_df <- remaining_df[!dont_match_words,]
  
  word_list <- remaining_df$word
  return(word_list)
}

wordle_help <- function(word_str, match_pattern = "00000", not_letters = "", words_full_df=words5){
  # match is a string of 5 numbers
  # - 0 is grey (no match)
  # - 1 is yellow (exist)
  # - 2 is green (position)
  
  pattern_vec <- unlist(str_extract_all(match_pattern, ""))
  if (length(pattern_vec) !=5){
    stop("match_pattern not length 5")
  }
  word_vec <- unlist(str_extract_all(word_str, ""))
  
  grey <- pattern_vec == 0
  yellow <- pattern_vec == 1
  green <- pattern_vec == 2
  
  possible_words <- five_solver(paste(word_vec[!grey],collapse = ""), 
                                sum(!grey),
                                paste(c(word_vec[grey],not_letters),collapse = ""),
                                words_full_df = words_full_df)
  
  # match pattern
  possible_words_matrix <- matrix(unlist(str_extract_all(possible_words, "")),
                                  ncol = 5, byrow = T)
  # green
  if (any(green)){
    green_matrix <- matrix(unlist(rep(word_vec[green], nrow(possible_words_matrix))),
                           nrow = nrow(possible_words_matrix),
                           byrow = TRUE)
    
    green_check_matrix <- possible_words_matrix[,green] == green_matrix
    green_check_vec <- apply(green_check_matrix,1,all)
    
    possible_words_matrix <- possible_words_matrix[green_check_vec,]
  }
  # yellow
  if (any(yellow)){
    if (length(possible_words_matrix) == 5){
      yellow_matrix <- word_vec[yellow]
      yellow_check_matrix <- possible_words_matrix[yellow] == yellow_matrix
      yellow_check_vec <- any(yellow_check_matrix)
      
      possible_words_matrix <- possible_words_matrix[!yellow_check_vec]
    } else {
      yellow_matrix <- matrix(unlist(rep(word_vec[yellow], nrow(possible_words_matrix))),
                              nrow = nrow(possible_words_matrix),
                              byrow = TRUE)
      
      yellow_check_matrix <- possible_words_matrix[,yellow] == yellow_matrix
      yellow_check_vec <- apply(yellow_check_matrix,1,any)
      
      possible_words_matrix <- possible_words_matrix[!yellow_check_vec,]
    }
  }
  
  # output
  if(length(possible_words_matrix)==5){
    out_word_list = paste(possible_words_matrix, collapse="")
  } else {
    out_word_list <- apply(possible_words_matrix,1,paste,collapse="")
  } 
  return(out_word_list)
}

wordle_help_multi <- function(word_list, match_pattern_list){
  if (length(word_list) != length(match_pattern_list)){
    stop("length of word list and pattern list must be the same")
  }
  
  for (round_i in 1:length(word_list)){
    if (round_i==1){
      out_word_list <- wordle_help(word_list[round_i], match_pattern_list[round_i])
    } else {
      out_word_df <- words5 %>%
        filter(word %in% out_word_list)
      out_word_list <- wordle_help(word_list[round_i], match_pattern_list[round_i],
                                   words_full_df = out_word_df)
    }
  }
  return(out_word_list)
}


###### WORDLE SOLVER #############
# wordle_help_multi(
#   c("spore","daunt","flick","clang","synod"),
#   c("00002","00000","01000","01000","00000")
# )
