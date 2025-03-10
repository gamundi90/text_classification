######### COUNTERSPEECH TEXT CLASSIFIER   ############
#### Author: Toni Gamundí
#### Date: 09/10/2024


rm(list = ls())
library(tidyverse)
library(naniar)



###### DATA LOADING -------
df <- read_csv2("comments_rated01.csv")
df2 <- read_csv2("comments_rated02.csv")
df_list <- read_csv2("CountercommentsList.csv")

# Descriptives
#table(df$counter)
#table(df2$counter)
#summary(df_list$countercomment)

# Merging
#df1_2 <- full_join(df, df2, by = c("comment", "reply", "counter"))
#df_list <- df_list %>% 
#  mutate(counter = ifelse(countercomment >= 0.9, 1, 0)) # this is just an arbitrary threshold
#df_final <- full_join(df1_2, df_list, by = c("comment", "counter"))
#df_final <- df_final %>% 
#  select(comment, counter)

# Check NAs
#any_na(df_final) # There are NAs
#n_miss(df_final) # 10 NAs
#n_miss(df_final$comment) # 0 NAs in the comment
#n_miss(df_final$counter) # 10 NAs in the "counter" variable
#df_final <- df_final %>% 
#  filter(!is.na(counter)) %>% 
#  filter(!is.na(comment))
# Total sample of 2425 comments, smaller than I initially thought!
#df_final <- df_final %>%
#  mutate(id = row_number()) # to generate new variable with comment ID

#write.csv(df_final, "df_final.csv")

df_final <- read_csv("df_final.csv")
df_final <- df_final[ , -1]








############ 1. DATA PREPROCESSING  ----

# First, some inspection of the data (these are just some examples of the steps I followed, but I also went over all comments qualitatively)
df_with_numbers <- df_final %>%
  filter(str_detect(comment, "\\d")) 
df_with_replies <- df_final %>%
  filter(str_detect(comment, "\\@[\\w\\d]+")) # 54 comments (at least) containing mentions to other users
df_with_replies_no_at <- df_final %>%
  filter(str_detect(comment, "\\b[A-Za-z]+\\d+\\b")) # 17 comments containing mentions to other users


###### 1.2 Lowercasing + removing numbers ######
df_cleaned <- df_final %>% 
  mutate(comment_cleaned = str_to_lower(comment)) %>%
  filter(!str_detect(comment, "=€%-#5&78&%%4")) %>% # drop this comment as it has no substantive meaning!
  # Remove numbers if they do not provide crucial information (this is essentially qualitative work, no functions to detect them):
  # Let's first replace all those usernames that begin by an at (@) because they also contain numbers at the end of such username
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\@[\\w\\d]+", "user")) %>%
  # Now replace usernames that are most recurrent (without an @) for better performance of ML models:
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bhalbmond64\\b", "user")) %>% 
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bfreiluft33\\b", "user")) %>% 
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bwollweich123\\b", "user")) %>% 
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bfarbenfroh58\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\btonart99\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bbox56\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bpickleduser\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bsafran08\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bohrwurm4free\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bstadthase\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\buserlvid\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\buserredocs\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bshiny2016\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bshiny\\b", "user")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\bshiny\\s*2016\\b", "user")) %>% # because  one comment contains "shiny 2016" separated
  # Replace (standalone) numbers by the corresponding German word
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b10\\b", "zehn")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b15\\b", "fünfzehn")) %>% 
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b18\\b", "achtzehn")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b65\\b", "fünfundsechzig")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b20\\b", "zwanzig")) %>% 
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b21\\b", "einundzwanzig")) %>% 
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b30\\b", "dreißig")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b35\\b", "fünfunddreißig")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b70\\b", "siebzig")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b75\\b", "fünfundsiebzig")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b50\\b", "fünfzig")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b90\\b", "neunzig")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b100\\b", "hundert")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b200\\b", "zweihundert")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b100\\.000en\\b", "hunderttausenden")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b100\\.000\\b", "einhunderttausend")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b1000\\b", "tausend")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b1000x\\b", "tausendmal")) %>% 
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b2ten\\b", "zweiten")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b80er\\b", "achtziger jahre")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b377\\b", "paragraph dreihundertsiebenundsiebzig des indischen strafgesetzbuchs")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b98203\\b", "achtundneunzigtausendzweihundertdrei")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b218/219\\b", "zweihundertachtzehn bis zweihundertneunzehn")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\beine 1\\.\\b", "eine erste lösung")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b0\\b", "null")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b1\\b", "eins")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b2\\b", "zwei")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b3\\b", "drei")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b4\\b", "vier")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b5\\b", "fünf")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b6\\b", "sechs")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b7\\b", "sieben")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b8\\b", "acht")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\b9\\b", "neun"))
  # NB: We keep years because specific years can carry unique significance that could be lost if converted to word form
  # In fact, after qualitative inspection of the comments it turns out that they do carry significance!



###### 1.3 Removing stopwords in German using a pre-compiled stopwords list that we have customized ###### 

# Let's first supress comments in English
library(textcat) # to identify the language for each comment
df_cleaned$language <- textcat(df_cleaned$comment)
# table(df_cleaned$language)
# Let's drop comments in English -- which  we can include in robustness checks e.g. if we translate them using LLMs like GPT-4! By now, however, we'll drop them
english_comment_indices <- which(df_cleaned$language == "english")
#print(english_comment_indices) # to see what rows have comments in English
english_comments <- df_cleaned$comment[english_comment_indices]
for (i in seq_along(english_comments)) {
  cat("Row:", english_comment_indices[i], "- Comment:", english_comments[i], "\n\n")
} # to analyze which comments were classified as English by mistake
# rows/comments 523 and 1928 are in German!
rows_to_drop <- c(82, 103, 190, 278, 431, 613, 707, 801, 1409, 1466, 1665, 2075)
df_cleaned <- df_cleaned %>% 
  filter(!(row_number() %in% rows_to_drop))# Exclude these rows from the final dataset!

# Stopwords list package and customization of such list
library(lsa) 
stopwords_de <- lsa::stopwords_de
stopwords_de # as we can see, there are 370 stopwords in German in this list

## However, we should keep some of them as they can be **critical** for our purposes. Then, we could conduct robustness checks dropping them out.
## For counterspeech detection, where the context and subtle nuances of agreement (or disagreement) are crucial, more stopwords might need to be retained than in other types of text analysis! 
# Let's drop all those stopwords excluding these:
stopwords_de <- stopwords_de[!stopwords_de %in% c("bitte",
                                                  "gut",
                                                  "keine",
                                                  "keinem",
                                                  "keinen",
                                                  "keiner",
                                                  "keines",
                                                  "nein",
                                                  "nicht",
                                                  "mehr", #more
                                                  "wenig", #little
                                                  "wenige", #few
                                                  "viel", #much
                                                  "viele", #many
                                                  "genug", #enough
                                                  "und", #optional: robustness checks without it
                                                  "oder" #optional: robustness checks without it
)]
# More data cleansing and preparation
df_cleaned <- df_cleaned %>% 
  mutate(comment_cleaned = str_replace_all(comment_cleaned, 
                                           paste("\\b",
                                                 stopwords_de,
                                                 "\\b", 
                                                 collapse = "|",
                                                 sep = ""),
                                           " "
  )) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned,
                                           "\\bhätte\\b",
                                           " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned,
                                           "\\bmach\\b",
                                           " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned,
                                           "\\bmal\\b",
                                           " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned,
                                           "\\bnen\\b", # a colloquial abbreviation for "einen" in German, meaning "a"
                                           " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned,
                                           "\\bpc\\b",
                                           " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned,
                                           "\\bps\\b", #postscript
                                           " ")) %>%
  mutate(comment_cleaned = str_squish(comment_cleaned)) # removes whitespace at the start and end 



###### 1.4 Remove punctuations, special characters + replace emojis #####

# First, some qualitative inspection of data to check if there are special characters to replace
prueba <- df_cleaned %>% 
  filter(str_detect(comment_cleaned, "%"))  
prueba <- df_cleaned %>% 
  filter(str_detect(comment_cleaned, "\\?"))
prueba <- df_cleaned %>% 
  filter(str_detect(comment_cleaned, "/n"))
prueba <- df_cleaned %>% # to detect comments with these symbols, this is present in the commentsrated03.csv only!
  filter(str_detect(comment_cleaned, "⠀ ⠀ ⠀ ⠀ ⠀ ⠀ ⠀ ⠀ ⠀")) 
prueba <- df_cleaned %>% # to detect comments with these symbols
  filter(str_detect(comment_cleaned, "._."))

# replace/remove emojis, special characters, and punctuations by their equivalent in German words
df_cleaned <- df_cleaned %>%
  ###### replace emojis
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:\\(","stirnrunzeln")) %>% # Frowning, disapproval
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\=\\(","stirnrunzeln")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:-\\(","stirnrunzeln")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\=\\)","erröten")) %>% # Blush, go red
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:\\)","erröten")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:-\\)","erröten")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "☺", "erröten")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\^\\^", "erröten")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:-d","grinsen")) %>% # Smiling
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;-d","grinsen")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:d","grinsen")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;d","grinsen")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "xd","grinsen")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;\\)","zwinkern")) %>% # Winking
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;-\\)","zwinkern")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:p","zwinkern")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:-p","zwinkern")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;p","zwinkern")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;-p","zwinkern")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;-\\(","wütend")) %>% # Angry
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;\\(","wütend")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "-.-'","wütend")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "-.-","wütend")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:-\\*","kuss")) %>% #Kiss
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:\\*","kuss")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;-\\*","kuss")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;\\*","kuss")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\;-\\(","wütend")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:\\/","unbeeindruckt")) %>% # Unamused
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\:-\\/","unbeeindruckt")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "<3","herz")) %>% # Heart
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "❤","herz")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "\\*\\.\\*","herz")) %>%
  ###### replace special characters 
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "ä", "ae")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "ö", "oe")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "ü", "ue")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "ß", "ss")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "%", " prozent")) %>% #space added on purpose
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "€","euro")) %>%
  ###### remove punctuations
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "!")) %>% # for robustness checks we could leave it as it may be useful to understand the context
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\\?")) %>% # for robustness checks we could leave it as could be useful for counterspeech detection since it may question the previous comment!!
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "&")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\\.")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\\,")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\"")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, ":")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, ";")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "=")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\\*")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\\(")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\\)")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "/n")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "/")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "°")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "^_^")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\\+")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, ">~<")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "~")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "'")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "#")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "@")) %>% # be careful because users reply to others using @ -- however, we already replaced all usernames by user
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\\[")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "\\]")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "”")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "“")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "’")) %>%
  mutate(comment_cleaned = str_remove_all(comment_cleaned, "„")) %>% # since comments in German
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "-", " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "_", " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "<", " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, ">", " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "$", " ")) %>%
  mutate(comment_cleaned = str_replace_all(comment_cleaned, "◊", " ")) %>% 
  mutate(comment_cleaned = str_squish(comment_cleaned))

###### 1.5 Handle special cases: Remove empty strings + nonsignificant symbols + duplicates #####

# Remove duplicates
df_cleaned <- df_cleaned %>% 
  mutate(comment_cleaned = str_trim(comment_cleaned)) %>% # Remove leading/trailing spaces
  distinct(comment_cleaned, .keep_all = TRUE) #it keeps the first occurrence of each unique value in that column and removes the subsequent duplicates

# Verify no duplicates anymore
duplicates <- df_cleaned %>%
  filter(duplicated(comment_cleaned))

# Remove rows with empty strings or very short comments
df_cleaned <- df_cleaned %>%
  filter(comment_cleaned != "") %>% # empty strings
  filter(nchar(comment_cleaned) > 1) # very short comments with no substantial meaning. As robustness checks this could be up to 2 or 3!

###### 1.6 Lemmatization (robustness checks) and data inspection ######

library(udpipe)
ud_model <- udpipe_download_model(language = "german")
# Load the German language model
ud_model <- udpipe_load_model(ud_model$file_model)
# Annotate the text data
df_cleaned$id <- as.character(df_cleaned$id)
annotations <- udpipe_annotate(ud_model, 
                               x = df_cleaned$comment_cleaned, 
                               doc_id = df_cleaned$id)
annotations_df <- as.data.frame(annotations)
# Combine the lemmatized words back into sentences grouped by doc_id
lemmatized_comments <- annotations_df %>%
  group_by(doc_id) %>%
  summarise(comment_lemmatized = paste(lemma, collapse = " "))
# Merge the lemmatized comments back into the original dataframe
df_cleaned <- df_cleaned %>%
  left_join(lemmatized_comments, by = c("id" = "doc_id"))

# How balanced is our data (n for each class)?
table(df_cleaned$counter) # Quite unbalanced... Only 180 counterspeech comments. 
#write.csv(df_cleaned, "df_cleaned.csv")

###### 1.7 Tokenization ------

## Tokenization will be presented in the next section as it is part of the feature extraction process, although 
## some handbooks consider tokenization as part of the data preprocessing.




###### 1.8 Consistency in the Unit of Analysis ----


# Calculate the number of tokens (words) in each comment
df_cleaned$comment_length <- sapply(df_cleaned$comment_cleaned, function(comment) {
  strsplit(comment, "\\s+")[[1]] %>% length()
})

# Summary statistics of comment lengths
summary(df_cleaned$comment_length)
# Visualize the distribution of comment lengths
ggplot(df_cleaned, aes(x = comment_length)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Comment Lengths",
       x = "Number of Words",
       y = "Frequency")

# Set arbitrary thresholds
min_length <- 3  # Minimum number of words
max_length <- 40 # Maximum number of words
# Calculate the length of each comment
df_cleaned <- df_cleaned %>%
  mutate(comment_length = str_count(comment_cleaned, "\\w+")) # NB: We focus on comment_cleaned and not on comment because it has been preprocessed!

###### Load the final preprocessed/cleaned dataset -----
#write.csv(df_cleaned, "df_cleaned.csv")
df_cleaned <- read_csv("df_cleaned.csv")
#Flag comments for manual review instead of removing them
df_cleaned2 <- df_cleaned %>%
  mutate(flag_for_review = ifelse(comment_length < 3 | comment_length > 40, TRUE, FALSE))
df_cleaned_flag2 <- df_cleaned2 %>% 
  filter(flag_for_review == TRUE) # There are 136 comments that are too short or too long!

# Among these, how many are counterspeech?
table(df_cleaned_flag2$counter) 
# Among these 136 comments, 6 are counterspeech. Given the small number of counterspeech comments in the whole dataset, it seems critical to identify any pattern. Let's see:
df_cleaned_flag_large <- df_cleaned_flag2 %>% 
  filter(comment_length > 40) # 22 out of 136 are comments with over 40 words!
#write.csv(df_cleaned_flag_large, "df_cleaned_flag_large.csv")

df_cleaned_flag_small <- df_cleaned_flag2 %>% 
  filter(comment_length < 3) # A lot (114 out of 136) of comments have only 1 or 2 words!
#write.csv(df_cleaned_flag_small, "df_cleaned_flag_small.csv")

# Now, let's see -within each subset- how many counterspeech comments there are:
table(df_cleaned_flag_large$counter) # There are 2 comments that are counterspeech 
table(df_cleaned_flag_small$counter) # There are 4 comments that are counterspeech

# It would not be sensical to drop such comments from the final dataset because these contain valuable information for our purpose (classify comments as counterspeech)
# Moreover, it seems like the data preprocessing was quite aggresive, as can be seen in the short comments... Which makes me conclude the following:
####### It will be beneficial to experiment with different levels of preprocessing and evaluate their impact on the performance of the text classification model. 
# Idea: Once we have run all desired models, rerun those creating another script without such preprocessing transformations (e.g. with and without those very short and/or very long comments)!



###### 1.9 Ensuring Balanced Representation of Classes ----

table(df_cleaned$counter)

# 1: The dataset exhibits a significant class imbalance, with the non-counterspeech comments clearly outnumbering the counterspeech comments! About 12 times more than counterspeech comments...
# 2: With fewer examples of counterspeech, the model might not learn enough about the characteristics of counterspeech comments, leading to poor detection and a high rate of false negatives.
# 3: In the written version, I report all alternatives we have and assess their suitability.

# Random oversampling and SMOTE will be performed in Python #


############# 2. FEATURE EXTRACTION METHODS: VECTORIZATION ######

# Different text representations (via different feature extraction methods) will be presented in this section:

###### 2.1 Bag-of-Words #############


####### 2.1.1 Tokenization: split text into words -----

# Step 1: Tokenization + vocabulary creation
library(quanteda)
library(quanteda.textstats)
df_cleaned <- read_csv("df_cleaned.csv")
comments_cleaned <- df_cleaned$comment_cleaned # extract comment_cleaned column as a character vector
toks <- comments_cleaned %>% 
  tokens(what = "word", # tokenize comments: splitting the text into smaller units, i.e. words
         #remove_separators = T, # like \n
         #split_hyphens = F, # do not split e.g. "self-aware"
         verbose = T) # output messages to indicate the progress of the tokenization

head(toks)


# Step 2: Document-Feature Matrix (DFM) creation (this results in a Bag of Words representation)
library(stringr)
txt.mat <- dfm(toks) # it converts the tokens (toks) into a Document-Feature Matrix (DFM)
#txt.mat <- dfm_trim(txt.mat) # there is an option to filter by a minimum of words, discuss together
#txt.mat <- txt.mat[, str_length(colnames(txt.mat)) > 1] # to remove features (words in this case) that consist of only one character

save(txt.mat, file = "dfm_bag_of_words.RData") # saving the 'Bag of Words' DFM

# Step 4: Analyze the DFM and reflect on whether more preprocessing is needed
# inspect DFM
topfeatures(txt.mat, n = 15)
topfeatures(txt.mat[1, ]) # in individual documents
# inspect DFM visually
tstat <- textstat_frequency(txt.mat, n = 15)
ggplot(data = tstat,
       aes(x = feature,
           y = frequency)) +
  geom_point() +
  theme_bw()

####### 2.1.2 Tokenization + n-grams inclusion: split text into n-grams -----

# Step 1 + 2 + 3: Tokenization,  Vocabulary Creation and Matrix creation
tokens_ngrams2 <- tokens_ngrams(toks, n = 2)
dfm_ngrams2 <- dfm(tokens_ngrams2) # Create DFM with n-grams
# Step 4: Analyze the DFM and reflect on whether more preprocessing is needed
# Inspect the top features
topfeatures(dfm_ngrams2)
# Takeaways:
# 1:  Function word combinations like "und_nicht" or "gar_nicht" might still not add much semantic value
# 2:  While some bigrams provide context, others might still be too generic or not add significant value. For example, "er_will" (he wants) is common but might not be specific enough
# 3:  Examine the context in which these bigrams appear to determine if they genuinely contribute to the analysis. For instance, "frauen_und" might be relevant in discussions about gender, whereas "und_nicht" might not be as useful
# 4:  Additional preprocessing steps such as removing non-informative bigrams or applying part-of-speech tagging to focus on more significant phrases could be beneficial...
save(dfm_ngrams2, file = "dfm_ngrams2.RData")

# Step 1 + 2 + 3: Tokenization,  Vocabulary Creation and Matrix creation
tokens_ngrams3 <- tokens_ngrams(toks, n = 3)
dfm_ngrams3 <- dfm(tokens_ngrams3) # Create DFM with n-grams
# Step 4: Analyze the DFM and reflect on whether more preprocessing is needed
# Inspect the top features
topfeatures(dfm_ngrams3)
save(dfm_ngrams3, file = "dfm_ngrams3.RData")

#### Load data: BoW (trigram) ----
load("dfm_ngrams3.RData")
# Convert the DFM to a data frame using `convert()`
df_features3 <- convert(dfm_ngrams3, to = "data.frame")
# Add the target variable 'counter' to the features data frame
df_features3$counter <- df_cleaned$counter
# Check the structure of the final dataset
str(df_features3)
df_features3 <- df_features3[, -which(names(df_features3) == "doc_id")] # this column is not a feature and should be removed before training the machine learning model
write.csv(df_features3, "DFM_bow_trigrams.csv")

#### Load data: BoW (trigrams) with oversampling method ----
comments_cleaned_over <- df_cleaned_over$comment_cleaned # extract comment_cleaned column as a character vector
toks <- comments_cleaned_over %>% 
  tokens(what = "word", # tokenize comments: splitting the text into smaller units, i.e. words
         #remove_separators = T, # like \n
         #split_hyphens = F, # do not split e.g. "self-aware"
         verbose = T) # output messages to indicate the progress of the tokenization
tokens_ngrams3 <- tokens_ngrams(toks, n = 3)
txt.mat <- dfm(tokens_ngrams3) # it converts the tokens (toks) into a Document-Feature Matrix (DFM)
txt.mat <- txt.mat[, str_length(colnames(txt.mat)) > 1] # to remove features (words in this case) that consist of only one character
df3 <- convert(txt.mat, to = "data.frame")
df3$counter <- df_cleaned_over$counter
df3 <- df3[, -which(names(df3) == "doc_id")] # this column is not a feature and should be removed before training the machine learning model
write.csv(df3, "DFM_bow_trigrams_oversampling.csv")

###### 2.2 TF-IDF  ######
#### 2.2.1 Convert the simple DFM (unigram) to a TF-IDF representation
dfm_tfidf <- dfm_tfidf(txt.mat)
# Inspect the top features
topfeatures(dfm_tfidf, n = 15)
save(dfm_tfidf, file = "TF-IDF_ngrams1.RData")

####### 2.2.2 TF-IDF with n-grams (trigrams) ----
dfm_ngrams_tfidf3 <- dfm_tfidf(dfm_ngrams3) # TF-IDF with n-grams inclusion (trigrams)!
topfeatures(dfm_ngrams_tfidf3, n = 15)
### Takeaways:
# 1:  "weiss_nicht_ob" (does not know if) and "ob_mann_oder" (whether man or): These phrases might appear in questions or discussions involving uncertainties or hypothetical scenarios, possibly indicating debates or expressions of doubt.
# 2:  There are a number of contextual phrases: "egal_ob_mann" (doesn't matter if man), "schwule_und_lesben" (gays and lesbians), "frauen_beim_militaer" (women in the military), "leben_er_will" (he wants to live)
#     which may reflect personal narratives or stories, discussions about LGBTQ+ communities and around gender neutrality or equality, etc.
# 3:  These trigrams are not just frequent but also distinctive, meaning they are particularly characteristic of certain comments. Their high TF-IDF scores suggest these phrases do not appear uniformly across all documents but are more specific to certain topics or themes.
# 4:  This specificity makes these trigrams useful for distinguishing between different topics or identifying key themes in the dataset. They are particularly valuable for understanding discussions around gender, LGBTQ+ issues, and social roles.
# 5:  These trigrams can be crucial features in a text classification model aimed at identifying comments related to specific themes, such as gender identity, social roles, or LGBTQ+ issues.
# 6:  [NEXT STEP] considering the use of word/document embeddings or other NLP techniques could further enhance the model's ability to capture the nuances in these discussions.
save(dfm_ngrams_tfidf3, file = "TF-IDF_ngrams3.RData")

# Further analyses would include estimating TF and IDF separately, not crucial for the current project though!

###### 2.3 Word embeddings  ######

## See Macanovic & Przepiorka’s (2024) script called “B_3_Word_vector_model_preparation.ipynb” to do it using Python
#### In this project, I'll use Word2Vec, Doc2Vec, and OpenAI's ada and text-embedding-3-large for embeddings, using Python!





############# 3. EXPLORATORY DATA ANALYSIS -----
##### 3.1 Bag-of-Words (unigram) ----

## a) Word Frequency Analysis

# Calculate the frequency of each word (feature) in the DFM
library(quanteda)
library(quanteda.textstats)
word_freq <- textstat_frequency(dfm_bow)
# View the top most frequent words
head(word_freq, 10)
topfeatures(dfm_bow, n = 20)
topfeatures(dfm_bow[1, ]) # in individual documents
# inspect DFM visually
tstat <- textstat_frequency(dfm_bow, n = 15)
ggplot(data = tstat,
       aes(x = feature,
           y = frequency)) +
  geom_point() +
  theme_bw()

## b) Document Frequency (DF)
doc_freq <- colSums(dfm_bow > 0)
doc_freq_df <- data.frame(term = names(doc_freq), doc_freq = doc_freq)
head(doc_freq_df, 10)

## c) Lexical diversity for each document
lexical_diversity <- textstat_lexdiv(dfm_bow)
head(lexical_diversity) # Check the TTR: Type-Token Ratio
# 1: Many documents have a TTR of 1.0, which indicates that every word in the document is unique (i.e. there are no repeated words in these documents), which suggests that these documents are quite short or use varied language.
# 2: Some documents have slightly lower TTRs, so there are some repeated words, but the overall diversity remains high!

## d) Keyness: Which words are distinctive of one class over another?
# To explore which words are more characteristic of one class (i.e. counterspeech vs. non-counterspeech).
keyness_result <- textstat_keyness(dfm_bow, target = df_cleaned$counter) # keyness for 2 classes
head(keyness_result, 10) # top keyness features



############# 4. SUPERVISED MACHINE LEARNING MODELS + SOME CONSIDERATIONS-------

# For each of the following supervised ML models, we will use different feature extraction methods: 1) bag of words 2) TF-IDF (with different n-grams) 3) word embeddings and 4) document embeddings 
# in order to experiment with different feature extraction methods to determine which one works best for our classification problem!

## Macanovic & Przepiorka (2024): "When applying machine learning models to text classification, we do not necessarily need to run every model with every possible feature extraction method (Bag of Words, TF-IDF, token and document embeddings). 
#  However, it is good practice to experiment with different feature extraction methods to determine which one works best for our specific problem."
#  Why? Different feature extraction methods capture different aspects of the text, and some methods may work better for certain datasets or problems than others. 

# Before we get into matter, there are some important considerations to bear in mind:
# (1) Ensure that the unit of analysis is consistent across the dataset. If some comments are significantly longer or shorter, it might impact the model’s performance. 
# (2) Ensure that the dataset has a balanced representation of both classes (counterspeech and non-counterspeech) or use techniques like oversampling, undersampling, or class weighting if the dataset is imbalanced. 
# (3) Ensure that the test set is representative of the entire dataset.
# (4) If the data is imbalanced (i.e. one class significantly outnumbers the other), it is crucial to stratify the splits by class to ensure that the proportion of each class is the same in the training, validation, and test sets.
# (5) If the metadata is likely to have a significant impact on the outcome or label, it should be included. For example, if predicting whether a comment is counterspeech, metadata like the author’s identity, the length, the platform on which the comment was made, or the timestamp could be relevant. Treat metadata as additional features in the model.




######### Supervised ML models will be run in Python (see ml_classification.ipynb file) -----