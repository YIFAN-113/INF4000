library(tidyverse)
library(GGally)
library(ggplot2)
library(dplyr)
library(tidytext)
library(stringr)
library(tm)
library(SnowballC)

# Load dataset
songs<-read.delim(file = "songs.csv", header = TRUE, sep = "\t", quote = "")
artists<-read.delim(file = "artists.csv", header = TRUE, sep = "\t", quote = "")
acoustic_features<-read.delim(file = "acoustic_features.csv", header = TRUE, sep = "\t", quote = "")
lyrics<-read.delim(file = "lyrics.csv", header = TRUE, sep = "\t", quote = "")
tracks<-read.delim(file = "tracks.csv", header = TRUE, sep = "\t", quote = "")

# Describe the dataset
summary(songs)
summary(artists)
summary(acoustic_features)
summary(tracks)

#Filter outliers
acoustic_features<-acoustic_features%>%filter(time_signature>0&loudness<=0&tempo>0)
head(songs$artists)

# Get artist id
songs<-songs%>%mutate(artist_id = gsub("\\{\\'(.*?)\\':.*", "\\1", artists))
head(songs$artist_id)

# Convert followers to integers
artists$followers <- as.integer(artists$followers)
summary(artists)
artists <- artists %>% filter(!is.na(followers))

# Get the year the song was released
tracks$release_year <- substr(tracks$release_date,1,4)
head(lyrics)

# Rename column names to prevent confusion
songs<-songs%>%rename(song_pop = popularity)
artists<-artists%>%rename(artist_pop = popularity)

# Splitting datasets
tracks_sub <- tracks[,c("song_id","release_year")]
song_sub <- songs[,c("song_id","artist_id","song_type","song_pop")]
artist_sub <-artists[,c("artist_id","followers","artist_pop","artist_type","main_genre")]

# Inline merged datasets
song_all <- inner_join(acoustic_features, song_sub, by = "song_id")
song_all <- inner_join(song_all,tracks_sub,by="song_id")
song_all <- inner_join(song_all, artist_sub, by="artist_id")
song_all <- inner_join(song_all,lyrics,by="song_id")
summary(song_all)

# Recode the year of issue into year groups
song_all$release_year <- as.integer(song_all$release_year)
song_all <- song_all %>%
  mutate(year_group = paste0((release_year %/% 10) * 10, "-", (release_year %/% 10) * 10 + 9))


# Calculate the correlation matrix
cor_matrix <- cor(song_all[, c("song_pop", "duration_ms", "acousticness", "danceability", "energy", 
                               "instrumentalness", "liveness", "loudness", "speechiness", "valence", "tempo")])
round(cor_matrix,3)

# Converting the correlation matrix to long format, for graphing purposes
library(reshape2)
melted <- melt(cor_matrix)

# Correlation heat mapping
library(ggplot2)
p1 <- ggplot(melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features", fill = "Correlation")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 8),
        axis.text.y = element_text(hjust = 1,size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10))


# Preprocessing Lyrics Column
song_all <- song_all %>%
  mutate(lyrics = str_replace_all(lyrics, "\\\\n", "\n"),         # convert line breaks
         lyrics = str_replace_all(lyrics, "\\\\'", "'"),          # Fix escaped single quotes
         lyrics = str_replace_all(lyrics, "\\\\\"", "\""),        # Fix escaped double quotes
         lyrics = str_replace_all(lyrics, "\\[.*?\\]", ""),       # Remove structure tags
         lyrics = str_replace_all(lyrics, "\\d+", ""),            # Remove all numbers
         lyrics = str_replace_all(lyrics, "[^\\w\\s']", " "),     # Remove punctuation but retain single quotes
         lyrics = str_replace_all(lyrics, "\\s+", " "),           # Replace characters with spaces
         lyrics = str_replace_all(lyrics, "xa", ""),              # If this step is not performed,
         # the final word frequency result will have a large number of "xa", 
         # the exact reason for which is still unknown and may be some kind of special character.
         lyrics = str_trim(lyrics))                               # Remove extra spaces


# Creating a corpus
lyrics_corpus <- Corpus(VectorSource(song_all$lyrics))

# Lowercase
lyrics_corpus <- tm_map(lyrics_corpus, content_transformer(tolower))

# Remove Discontinued Words
lyrics_corpus <- tm_map(lyrics_corpus, removeWords, stopwords("en"))

# stem extraction
lyrics_corpus <- tm_map(lyrics_corpus, stemDocument)

# Creating a word matrix
dtm <- DocumentTermMatrix(lyrics_corpus)
print(dtm)

# Removal of sparse terms
dtm <- removeSparseTerms(dtm, 0.95)
print(dtm)

# Converting matrices to dataframes
dtm_data <- as.data.frame(as.matrix(dtm))
# Recode song_pop as a binary variable.
song_all$pop_num <- ifelse(song_all$song_pop >= 60,1,0)
# Add popularity identifier
dtm_data$pop_num <- song_all$pop_num

# Categorise songs according to whether they are popular or not
popular_songs <- dtm_data[dtm_data$pop_num == 1, -ncol(dtm_data)]
unpopular_songs <- dtm_data[dtm_data$pop_num == 0, -ncol(dtm_data)]

# Calculate the frequency of occurrence of a word
popular_word_freq <- colSums(popular_songs)
unpopular_word_freq <- colSums(unpopular_songs)

# Extract the top 10 words in each group
popular_words <- sort(popular_word_freq, decreasing = TRUE)[1:10]
unpopular_words <- sort(unpopular_word_freq, decreasing = TRUE)[1:10]

popular_words100 <- sort(popular_word_freq, decreasing = TRUE)[1:20]
unpopular_words100 <- sort(unpopular_word_freq, decreasing = TRUE)[1:20]

popular_df100 <- data.frame(Word = names(popular_words100), Frequency = popular_words100)
unpopular_df100 <- data.frame(Word = names(unpopular_words100), Frequency = unpopular_words100)

# Convert to data frame
popular_df <- data.frame(Word = names(popular_words), Frequency = popular_words)
unpopular_df <- data.frame(Word = names(unpopular_words), Frequency = unpopular_words)


library(patchwork)
# Plotting top words in popular songs
p2<-ggplot(popular_df, aes(x = Frequency, y = reorder(Word, Frequency))) +
  geom_bar(stat = "identity", fill = "salmon", width = 0.7) +
  geom_text(
    aes(label = Frequency),  
    hjust = 1, 
    size = 4,  
    color = "black",
    fontface = "bold"
  ) +
  labs(
    title = "Top 10 Words in Popular Songs",
    x = "Frequency",
    y = "Words",
    
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text.y = element_text(face = "italic") 
  )

# Plotting top words in unpopular songs
p3<-ggplot(unpopular_df, aes(x = Frequency, y = reorder(Word, Frequency))) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + 
  geom_text(
    aes(label = Frequency),  
    hjust = 1,    
    size = 4,       
    color = "black",
    fontface = "bold"
  ) +
  labs(
    title = "Top 10 Words in Unpopular Songs",
    x = "Frequency",
    y = "Words"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text.y = element_text(face = "italic") 
  )


# Filtering Missing Values
song_all<-song_all%>%filter(artist_type!="-")

# Draw violin plot
p4 <- ggplot(song_all, aes(artist_type, song_pop, group = artist_type)) +
  geom_violin(fill = "steelblue", alpha = 0.7) + 
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    color = "black",
    vjust = -0.5
  ) +
  labs(
    title = "Song pop grouped by artist type",
    caption = "song_all dataset",
    x = "Artist type",
    y = "Song pop"
  )


# Get top 1000 songs
top_1000_songs <- song_all %>%
  arrange(desc(song_pop)) %>%
  slice(1:1000)

# Get bottom 1000 songs
bottom_1000_songs <- song_all %>%
  arrange(song_pop) %>%
  slice(1:1000)

columns <- c("duration_ms", "acousticness", "danceability", "energy", 
             "instrumentalness", "liveness", "loudness", "speechiness", "valence", "tempo")

# Calculate the mean
pop_song_means <- sapply(top_1000_songs[, columns], mean, na.rm = TRUE)
print(pop_song_means)
unpop_song_means <- sapply(bottom_1000_songs[, columns], mean, na.rm = TRUE)
print(unpop_song_means)

library(ggradar)
library(scales)

# Standardised pop_song_means and unpop_song_means
max_values <- pmax(pop_song_means, unpop_song_means)
pop_song_norm <- pop_song_means / max_values
unpop_song_norm <- unpop_song_means / max_values
unpop_song_norm[7] <- max_values[7]/unpop_song_means[7]

max_values
pop_song_norm
unpop_song_norm

# Converted to dataframes
radar_data <- data.frame(
  category = columns,
  Popular = pop_song_norm,
  Unpopular = unpop_song_norm
)

# Converting data to long format
radar_data <- radar_data %>%
  pivot_longer(cols = -category, names_to = "Type", values_to = "Value")

# Reformatting data into a form suitable for radar charts
radar_plot_data <- radar_data %>%
  pivot_wider(names_from = category, values_from = Value)


# Drawing spider plot
p5 <- ggradar(
  radar_plot_data,
  grid.min = 0, 
  grid.mid = 0.5,  
  grid.max = 1,   
  values.radar = c("0", "0.5", "1"), 
  group.colours = c("red", "blue"), 
  group.line.width = 1.2, 
  group.point.size = 4, 
  axis.label.size = 3, 
  grid.label.size = 4, 
  background.circle.colour = "grey90", 
  gridline.mid.colour = "grey50", 
  legend.position = "top", 
  legend.text.size = 10,  
  fill = TRUE,      
  fill.alpha = 0.3   
)+
  ggtitle("Mean differences in acoustic features of popular and unpopular songs")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10) 
  )


# Combined Chart Output
p1_fixed <- wrap_elements(p1) + plot_layout(width = 5, height = 4)
p5_fixed <- wrap_elements(p5) + plot_layout(width = 5, height = 4)
p2_fixed <- wrap_elements(p2) + plot_layout(width = 5, height = 4)
p3_fixed <- wrap_elements(p3) + plot_layout(width = 5, height = 4)
p4_fixed <- wrap_elements(p4) + plot_layout(width = 10, height = 4)
(p1_fixed | p5_fixed)/
(p2_fixed | p3_fixed)/
(p4_fixed)