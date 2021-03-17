
##### here are all function for the word frequency tab



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
  "crossed","god","watch","leaf","food","arrow", "hugg", "cri", "tone"

)




################################################################
####################### function for preprocessing
#### takes df and filters accroding to user
#'@export
#'@rdname term_freq_computers
word_freq_data_wrangler <- function(df, input_date1, input_date2,
                                    input_emo, emoji_words,
                                    search_term, input_lang,
                                    input_comp){


names(df) <- c("date", "language", "word", "N", "emo")


### stem the search term so it fits better to words we have and convert to lower
search_term <- tolower(corpus::stem_snowball(search_term, algorithm = tolower(input_lang)))



## convert company term to lower
input_comp <- tolower(input_comp)

#### stem company inpptu,  control for company names that are two names
if (length(strsplit(input_comp, " ")[[1]]) > 1){
  input_comp <- corpus::stem_snowball(strsplit(input_comp, " ")[[1]], algorithm = tolower(input_lang))
  input_comp <- paste0(input_comp, collapse = "|")
} else {
  input_comp <- corpus::stem_snowball(input_comp, algorithm = tolower(input_lang))
}

### fitler for correct date
df <-  df %>%
  filter(between(date, as.Date(input_date1), as.Date(input_date2)) &
                   language == tolower(input_lang))  %>%
  {if (input_emo == T) filter(., emo == F &
                                !grepl(paste(emoji_words, collapse = "|"), word) ) else .} %>% ##filter out emoji words if chosen

 {if (input_comp != "NoFilter") filter(.,!grepl(input_comp, word)) else .} %>% #filter out company name if chosen

  {if (search_term != "") filter(.,grepl(search_term, word)) else .} %>% # only keep what contain search term
  select(-emo)

#### acount for empty df
if (dim(df)[1] == 0){
  return()
} else {
  return(df)
}


}

###### compute total number of unique words/bigrams
unique_words <- function(df){
  length(unique(df$word))
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
  head(input_n)

return(df)


}

###################################################################
############################ wordcloud

#'@export
#'@rdname term_freq_computers
word_cloud_plotter <- function(df, input_size = 1){

  df    %>%
     wordcloud2::wordcloud2(size = input_size,
                                              color = "random-light", backgroundColor = "#2b3e50")
}

#################################################################
############################# barplot

# term freq bar plot
term_freq_bar_plot <- function(df){
  ### remove scientifc notation
  options(scipen=999)

  #### show values in thousands
df$n <- df$n / 1000
### create plot
p <-   df %>%

    ggplot(aes(reorder(x = word, n), y = n, label = word)) +
    geom_col(width = 0.5, color = "#4e5d6c") +
  coord_flip() +
  labs(x = "",
       y = "N (in thousands)")+
  theme_classic() +
  theme(text = element_text(size=18)) +

  scale_y_continuous(expand = c(0, 0))

plotly::ggplotly(p,
                 tooltip = c("label", "y"))
}


################################################################
#################### time series of word frequency per day
#'@export
#'@rdname term_freq_computers
word_filter_time_series_plotter <- function(df){

## create plot
p <- df %>%
   group_by(date) %>%
    summarise(n = sum(N)) %>%


  ggplot() +
  geom_line(aes(date, n)) +
  theme_classic() +
  theme(text = element_text(size=18)) +
  labs(x = "Date",
       y = "N")


plotly::ggplotly(p)

}








