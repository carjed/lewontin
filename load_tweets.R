library(tidyverse)
library(tidytext)
library(topicmodels)
library(ldatuning)
library(tm)
library(qlcMatrix)
library(wordcloud)
library(plotly)
library(scales)

# custom stopword list used in Carlson & Harris, 2020
custom_stopwords <- c("https", "http", "tco", "gmailcom", "views", "love", "lover", "tweets",
                      "rts", "follow", "twitter", "endorsement", "fan", "james", "michael",
                      "andrew", "ryan", "chris", "matt", "och", "rt", "opinions", "paul",
                      "juan", "carlos", "luis", "jose", "maria", "jorge", "alex",
                      "endorsements", "account", "life", "john", "david", "social", "retweets", "amp",
                      stopwords(kind="en"), stopwords(kind="danish"), stopwords(kind="dutch"), 
                      stopwords(kind="finnish"), stopwords(kind="french"), stopwords(kind="german"),
                      stopwords(kind="hungarian"), stopwords(kind="italian"), stopwords(kind="norwegian"),
                      stopwords(kind="portuguese"), stopwords(kind="russian"), stopwords(kind="spanish"),
                      stopwords(kind="swedish"))

# load emoji lookup table
source("scrape_emoticons.R")

# modify emoji lookup table
lookup <- alltogether %>% 
  # ensure textualized emoji is separated by space
  mutate(Desc_Sub=tolower(paste0(" emoji", gsub("[+]|-| ", "", Description), " "))) %>% 
  # strip spaces for proper text -> emoji matching 
  mutate(Desc_Sub2=gsub(" ", "", Desc_Sub)) 

# emoji translation functions
emoji_to_text <- function(x) Reduce(function(x,r) gsub(lookup$Native[r],lookup$Desc_Sub[r],x,fixed=T),seq_len(nrow(lookup)),x)
text_to_emoji <- function(x) Reduce(function(x,r) gsub(lookup$Desc_Sub2[r],lookup$Native[r],x,fixed=T),seq_len(nrow(lookup)),x)

# load dataset of tweets referencing Lewontin
if(file.exists("data/20200827-20200904_lewontin_tweets.rds")){ 
    # for personal use, using fully hydrated Twitter data as collected
    rl_tweets <- list.files("data",
                          pattern="*tweets.rds", 
                          all.files=T, 
                          full.names=T) %>% 
    map_dfr(readRDS)
} else {
  # Twitter only allows public sharing of up to 50k status IDs, which must be "rehydrated" using the Twitter API;
  # this means that any tweets that have been deleted (or were from suspended/deleted accounts) since the original data scrape will be excluded.
  # 
  # This step will also require a Twitter developer API token
  status_ids <- read.table("data/status_ids.txt", header=T, colClasses=c("character"))
  rl_tweets <- rtweet::lookup_tweets(status_ids$status_id, token=token1)
}
  
# remove duplicate tweets in combined data, since scraping included overlapping date ranges
rl_tweets_filtered <- rl_tweets %>% 
  dplyr::filter(created_at < as.POSIXct("2021-05-25")) %>% 
  # dplyr::filter(!grepl("2021-06|2021-07", created_at)) %>% # ignore data with timestamp in June, 2021
  group_by(status_id) %>% 
  slice(1L)

# count tweets
nrow(rl_tweets_filtered)

# remove references to SSE Early Award
rl_tweets_filtered <- rl_tweets_filtered %>%
  dplyr::filter(!grepl("early award", tolower(text)))

# count remaining tweets
nrow(rl_tweets_filtered)

# index retweets in dataset
rt_dat <- rl_tweets_filtered %>% 
  # dplyr::filter(!grepl("award", tolower(text))) %>%
  rename(account=name, rt=retweet_screen_name) %>% 
  # left_join(dmp_df2 %>% dplyr::select(account, rt_topic=topic, wn_mean), by="account") %>% 
  mutate(rt=ifelse(is.na(rt), account, rt)) %>%
  # left_join(dmp_df2 %>% dplyr::select(rt=account, source_topic=topic), by="rt") %>%
  mutate(tweets=paste0(rt, ": ", text)) %>%
  group_by(tweets) %>% 
  arrange(created_at) %>% 
  mutate(order=row_number(), n=n()) %>% 
  # dplyr::filter(n>3) %>%
  ungroup() #%>%

#-----------------------------------------------------------------------------
# Fig. 4a: retweet timelines
#-----------------------------------------------------------------------------

fig4a <- rt_dat %>%
  ggplot(aes(x=as.Date(created_at), y=order, group=tweets, label=account))+
  geom_line(colour="grey80")+
  geom_point(aes(colour=rt, size=followers_count), alpha=0.5)+
  # scale_size(limits=c(0,0.5))+
  # scale_colour_manual(values=cols)+
  scale_y_log10(breaks=c(1,6,11,26,51,101), labels=c(0,5,10,25,50,100), expand=c(0.02,0))+
  #scale_x_date()+
  scale_x_date(breaks=date_breaks("2 week"))+ #as.Date(rt_dat$created_at[seq(1, nrow(rt_dat), 100)], "%Y-%m-%d"))+
  ylab("Retweet Number")+
  ggtitle("a")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size=12, angle=45, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=16),
        legend.position="none")+
  NULL





ggplotly(rt_dat_plot)

og_hist <- rl_tweets_filtered %>%
  dplyr::filter(is.na(retweet_screen_name)) %>%
  ggplot(aes(x=as.Date(created_at)))+
  geom_histogram(bins=18)+
  scale_x_date(breaks=date_breaks("2 week"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=6, angle=45, hjust=1),
        legend.position="none")

ggMarginal(rt_dat_plot, 
           type = "histogram",
           margins = 'x',
           colour = NA,
           fill = '#FFA500')

#-----------------------------------------------------------------------------
# Fig 4b: wordcloud of all tweets
#-----------------------------------------------------------------------------
removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)

tweets <- rt_dat %>%
  # dplyr::filter(lang=="en") %>%
  # mutate(description= paste0(name, " ", description)) %>%
  # mutate(description = emoji_to_text(description)) %>%
  mutate(text = removeURL(text)) %>%
  mutate(text = emoji_to_text(text)) %>%
  mutate(text = gsub("#", "hashtag", text)) %>%
  mutate(text = gsub("[[:punct:]]", "", text)) %>%
  summarise(doc=paste(text, collapse = " ")) %>%
  mutate(doc=iconv(doc, 'utf-8', 'ascii', sub=''))

tweets_tokenized <- tweets %>%
  unnest_tokens(word, doc)

tweets_word_counts <- tweets_tokenized %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words) %>%
  dplyr::filter(!(word %in% custom_stopwords)) %>%
  dplyr::filter(!(word %in% c("richard", "lewontin", "lewontin's", "lewontins"))) %>%
  mutate(word=gsub("hashtag", "#", word)) %>%
  mutate(word = text_to_emoji(word)) %>%
  dplyr::filter(n>=10)

set.seed(42)

tweets_word_counts %>%
  arrange(desc(n)) %>%
  head(150) %>%
  ggplot(
    aes(
      label = word, 
      size = n,
      color = factor(ntile(n, 5))))+
  # scale_colour_brewer(palette="Dark2", direction = -1)+
  scale_colour_manual(values=brewer.pal(8, "Dark2")[c(1,2,3,5,8)])+
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24)+
  theme_minimal()

 ggwordcloud(tweets_word_counts$word, tweets_word_counts$n)


 png("figures/tweet_wordcloud.png", width=6, height=6, units="in", res=300)
fig4b <- wordcloud(words = tweets_word_counts$word,
          freq = tweets_word_counts$n,
          min.freq = 1,
          max.words=200,
          random.order=FALSE,
          rot.per=0,
          scale=c(3.5,0.2),
          colors=brewer.pal(8, "Dark2"))
dev.off()


img <- readPNG("figures/tweet_wordcloud.png")
g <- rasterGrob(img, interpolate=TRUE)

img <- image_read("figures/fig4b_v2.png")

fig4b <- image_ggplot(img)+ggtitle("b")

grid.arrange(
  grobs = list(fig4a, fig4b),
  widths = c(2, 1)
)

#-----------------------------------------------------------------------------
# wordcloud of bios
#-----------------------------------------------------------------------------

bios <- rt_dat %>%
  # dplyr::filter(lang=="en") %>%
  # mutate(description= paste0(name, " ", description)) %>%
  # mutate(description = emoji_to_text(description)) %>%
  mutate(description = removeURL(description)) %>%
  mutate(description = emoji_to_text(description)) %>%
  mutate(description = gsub("#", "hashtag", description)) %>%
  mutate(text = gsub("[[:punct:]]", "", text)) %>%
  summarise(doc=paste(description, collapse = " ")) %>%
  mutate(doc=iconv(doc, 'utf-8', 'ascii', sub=''))

# tokenize, dropping stopwords
bios_tokenized <- bios %>%
  unnest_tokens(word, doc)

# apply default stopword list and count frequencies
word_counts <- bios_tokenized %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words) %>%
  dplyr::filter(!(word %in% custom_stopwords)) %>%
  dplyr::filter(!(word %in% c("richard", "lewontin", "lewontin's", "lewontins"))) %>%
  mutate(word = text_to_emoji(word)) %>%
  dplyr::filter(n>=10)

set.seed(1234) # for reproducibility 

png("figures/bio_wordcloud.png", width=6, height=6, units="in", res=300)
wordcloud(words = word_counts$word,
          freq = word_counts$n,
          min.freq = 1,
          max.words=200,
          random.order=FALSE,
          rot.per=0,
          scale=c(3.5,0.2),
          colors=brewer.pal(8, "Dark2"))
dev.off()

#-----------------------------------------------------------------------------
# wordcloud of original tweets
#-----------------------------------------------------------------------------
tweets_og <- rt_dat %>%
  dplyr::filter(is_retweet==FALSE) %>%
  # dplyr::filter(lang=="en") %>%
  # mutate(description= paste0(name, " ", description)) %>%
  # mutate(description = emoji_to_text(description)) %>%
  mutate(text = gsub("#", "hashtag", text)) %>%
  mutate(text = gsub("[[:punct:]]", "", text)) %>%
  summarise(doc=paste(text, collapse = " ")) %>%
  mutate(doc=iconv(doc, 'utf-8', 'ascii', sub=''))

tweets_og_tokenized <- tweets_og %>%
  unnest_tokens(word, doc)

tweets_og_word_counts <- tweets_og_tokenized %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words) %>%
  dplyr::filter(!(word %in% custom_stopwords)) %>%
  dplyr::filter(!(word %in% c("richard", "lewontin", "lewontin's", "lewontins"))) %>%
  mutate(word=gsub("hashtag", "#", word)) %>%
  dplyr::filter(n>=10)

png("figures/og_tweet_wordcloud.png", width=6, height=6, units="in", res=300)
wordcloud(words = tweets_og_word_counts$word,
          freq = tweets_og_word_counts$n,
          min.freq = 1,
          max.words=200,
          random.order=FALSE,
          rot.per=0,
          scale=c(3.5,0.2),
          colors=brewer.pal(8, "Dark2"))
dev.off()

l1972_tweet_handles <- rt_dat %>% 
  dplyr::filter(grepl("fallacy|diversity|race|racial|racism|racist|15%|85%|6.3%|variation", tolower(text))) %>% 
  pull(screen_name) %>% 
  unique