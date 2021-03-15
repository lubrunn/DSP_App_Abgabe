##### additional emoji filter words (most emoji words were
# already marked in preprocess)
emoji_words <- c(
  "plead","scream", "social media", "steam", "nose" , "box", "circl", "whit",
  "black", "button","exo", "sad",
  "love", "good", "red", "happi","mu",
  "happi","excus","tongu","stick", "tear", "joy", "flag", "skin",  "smile",
  "heart","eye", "index", "medium", "laugh", "loud", "roll", "floor","mark", "exclam",
  "hand", "clap","dollar",
  "hot", "light","blow", "kiss","amulet", "head", "tree","speaker","symbol","money","point",
  "grin","bicep","flex","note","popper","fist","car","follow","retweet","year","ago",
  "social media","woman","voltag","star","ball","camera","man","ass","video","cake","cool",
  "fac","smil","see","evil","party","sweat","thumb","big","the","crying","fing",
  "crossed","god","watch","leaf","food","arrow", "hugg"

)




################################################################
####################### function for preprocessing
#'@export
#'@rdname term_freq_computers
word_freq_data_wrangler <- function(df, input_date1, input_date2,
                                    input_emo, emoji_words,
                                    search_term, input_lang,
                                    input_comp){


names(df) <- c("date", "language", "word", "N", "emo")

### stem the search term so it fits better to words we have
search_term <- corpus::stem_snowball(search_term, algorithm = tolower(input_lang))

df <-  df %>%
  filter(between(date, as.Date(input_date1), as.Date(input_date2)) &
                   language == tolower(input_lang))  %>%
  {if (input_emo == T) filter(., emo == F &
                                !grepl(paste(emoji_words, collapse = "|"), word) ) else .} %>% ##filter out emoji words if chosen

 {if (!is.null(input_comp)) filter(.,!grepl(corpus::stem_snowball(input_comp), word)) else .} %>% #filter out company name if chosen

  {if (search_term != "") filter(.,grepl(search_term, word)) else .} %>% # only keep what contain search term
  select(-emo)

return(df)
}



#############################################################
########################### compute top n
#
# word_freq_data_wrangler(df, input_date1, input_date2,
#                                     input_emo, emoji_words, search_term)

#'@export
#'@rdname term_freq_computers
df_filterer <- function(df, input_n){

df <- df %>%
  group_by( word) %>%
  summarise(n = sum(N)) %>%
  arrange(desc(n)) %>%
  top_n(input_n, n)

return(df)


}

###################################################################
############################ wordcloud

#'@export
#'@rdname term_freq_computers
word_cloud_plotter <- function(df, input_size){

  df    %>%
     wordcloud2::wordcloud2(size = input_size,shape = 'star',
                                              color = "random-light", backgroundColor = "grey")
}

#################################################################
############################# barplot

# term freq bar plot
term_freq_bar_plot <- function(df){
  df %>%

    ggplot(aes(reorder(x = word, n), y = n)) +
    geom_col() +
    coord_flip()


}


################################################################
#################### time series of word frequency per day
#'@export
#'@rdname term_freq_computers
word_filter_time_series_plotter <- function(df){


df %>%
   group_by(date) %>%
    summarise(n = sum(N)) %>%


  ggplot() +
  geom_line(aes(date, n))

}


