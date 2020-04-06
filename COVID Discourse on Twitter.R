#Packages required: quanteda, ggplot, tm, stm, rtweet, igraph, dplyr, 

## Scraping the dataset from twitter using rtweet (note: you need to be logged into twitter on a browser)

# While the amount of tweets you can scrape from the twitter API is rate-limited to 15000 every 15 minutes, 
# if you set retryonratelit = TRUE, rtweet will continue scraping until it reaches the number you set.
corona = search_tweets(q= "#coronavirus lang:en", n = 100000, 
                       include_rts = FALSE, type="recent", 
                       retryonratelimit = TRUE)

# To find the date window of the twitter dataset:
summary(corona$created_at) 

## Create a document feature matrix and a wordcloud:

# Create a corpus using the corpus() function in quanteda:
corona_corpus = corpus(corona, text_field = "text")

# Create a Document Feature Matrix in which the words are pre-processed:
corona_uni_dfm = dfm(corona_corpus, tolower = TRUE, stem = TRUE, remove = stopwords("english"),
                 remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, 
                 remove_twitter = TRUE)

# Remove select keywords
corona_uni_dfm= dfm(corona_uni_dfm, remove = c("coronavirus","covid", "corona", "amp"))

# Generate a word cloud for the unigram dfm:
textplot_wordcloud(corona_uni_dfm, min_count = 15, random_order = FALSE,
rotation = .25,color = RColorBrewer::brewer.pal(8, "RdYlBu"))

# All of this can be done with two word phrases (bigrams):
corona_bi_dfm= dfm(corona_corpus, tolower = TRUE, stem = TRUE, remove = stopwords("en")m
                   remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, 
                   remove_twitter = TRUE, ngrams=2)



## Run an LDA structured topic model (note: this will take some time). K = number of topics.
uni_stm = stm(corona_uni_dfm, K = 10, init.type = "LDA")

# In order to get a detailed description of the topics and word choices.
# I used the FREX words for my analysis.
labelTopics(uni_stm, n=10)


## Estimate the effect of topic on retweeting:

# Find the mean or median retweet count for tweets that get at least 1 retweet.
retweeted = 
  corona %>%
  filter (corona$retweet_count != 0)

summary(retweeted$retweet_count)

# Create a dummy variable for high_retweet. I used 12 as the cutoff point for high_retweet because
# 12 was the median retweet count for retweeted tweets in the data set.
corona_uni_dfm@docvars$high_retweet = 0
corona_uni_dfm@docvars$high_retweet[corona_uni_dfm@docvars$retweet_count > 12] = 1

# Processing dfm to match LDA output (i.e. dropping all empty documents). Made a new dfm just in case.
corona_uni_dfm_new = dfm_subset(coron_uni_dfm, ntoken(corona_uni_dfm) > 0)

# Estimate effect of high retweet:
retweet_est <- estimateEffect(formula = 1:10 ~ high_retweet, stmobj = uni_stm, 
                              metadata = docvars(corona_uni_dfm_new), uncertainty = "Global")

# Linear regression output of each topic:
summary(retweet_est)

# Name the topics:
topic_names = c("1. Trump/Presidential Politics", "2. Aid and Assistance", "3. New Cases and Deaths",
                "4. Predictions",  "5. Information flows", "6. PPE", "7. Hospitals", 
                "8. Stay Home/Social Distance","9. Imperatives", "10. India and Africa")

# Plot the estimated effect:
plot(retweet_est, covariate = "high_retweet", topics = 1:10,
     model = uni_stm, method = "difference",
     cov.value1 = 1, cov.value2 = 0,
     xlab = "Low .......................................................................... High",
     main = "Effect of topic on retweeting", 
     xlim = c(-.3, .3), verbose.labels = F, labeltype = "custom", custom.labels = topic_names)

# The same can process be done for influencers by using the followers_count variable in the original dataset.


## Network analysis of topics:

# Create a topic model correlation output object:
mod_out_corr = topicCorr(uni_stm)

#plot using igraph
plot(mod_out_corr)
