# Required libraries: ggthemes, sentimentr, dplyr, purr

# Create a dictionary to test:
my_dictionary = c("black lives matter vote justice police criminal crime panther malcolm shoot")

#Clean dictionary using function
CleanDictionary = function(x){
  x = tokens(x,  remove_numbers = TRUE, remove_punct = TRUE)
  x = tokens_wordstem(x)
  x = tokens_select(x, pattern = stopwords('en'), selection = 'remove')
  x = tokens_tolower(x)
  return(as.character(x)) 
}

my_dictionary_cleaned = CleanDictionary(my_dictionary)

#Create a corpus and dfm
troll_corpus = corpus(russian_trolls_sample)
troll_dfm = dfm(troll_corpus, tolower = TRUE, stem = TRUE, remove_twitter = TRUE,
                remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("english"))

#Generate number of times dictionary words are used in dfm subset by category
left_trolls_on_issue  = sum(troll_dfm[troll_dfm@docvars$account_category == "LeftTroll", 
                                      colnames(troll_dfm) %in% my_dictionary_cleaned])

#Generate total numnber of words in category
left_total = sum(troll_dfm[troll_dfm@docvars$account_category == "LeftTroll", colnames(troll_dfm)])

#Find and round proportion
left_prop_on_issue = round((left_trolls_on_issue/left_total), digits = 4)

#Create a df with results for each category type
results = data.frame(ideology=c("Left Trolls", "Right Trolls"),
                     wordprop =c(left_prop_on_issue,right_prop_on_issue))

# Plot results (I like some the Wes Anderson palette's from the WesAnderson ggtheme)
ggplot(results, aes(fill=ideology, x=ideology, y=wordprop))+
  geom_bar(stat="identity", show.legend = FALSE) + theme_bw() + 
  xlab("Account ideology") + 
  ylab("Proportion of tweets related to Black Lives Matter") +
  ggtitle("    Russian troll accounts using Black Lives Matter to target the Left or Right") +
  geom_text(aes(label=wordprop), vjust=1.6, color="white", size=3.5)+
  ylim(0,0.02)+
  scale_fill_manual(values=wes_palette(n=2, name="Royal1"))

## Sentiment Analysis

troll_text = get_sentences(russian_trolls_sample$text)

#Use sentimentby() in sentimentr to get sentiments by category.
troll_sentiment = sentiment_by(troll_text, by=russian_trolls_sample$account_category)

# Profanity analysis
troll_profanity=profanity_by(troll_text, by=russian_trolls_sample$account_category)

#Make a df object to plot
profanity_df = as.data.frame(troll_profanity)

# Bar plot. I sometimes like theme_economist().
ggplot(profanity_df, aes(fill=account_category, y=ave_profanity, x=account_category)) + 
  geom_bar(position="dodge", stat="identity", show.legend = FALSE)+
  ggtitle("      Average Profanity used per Tweet by Account Ideology     ") +
  theme_economist() +
  xlab("Account Ideology")+
  ylab("Average Profanity per Tweet")+
  scale_fill_manual(values=wes_palette(n=2, name="Royal1"))

#Emotion Analysis

#Use emotionby() to get the analysis based on emotions


anger = troll_emotion[troll_emotion$emotion_type == "anger"]

trust = troll_emotion[troll_emotion$emotion_type == "trust"]

fear = troll_emotion[troll_emotion$emotion_type == "fear"]

joy = troll_emotion[troll_emotion$emotion_type == "joy"]

disgust = troll_emotion[troll_emotion$emotion_type == "disgust"]


#Bind the specific emotions you want into a df
emotion_df = as.data.frame(rbind(joy,anger,fear,disgust,trust))

#Plot df
ggplot(emotion_df, aes(fill=account_category, y=ave_emotion, x=emotion_type)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Emotional Sentiment of Tweets based on Ideology Targeted     ") +
  theme_economist() +
  xlab("Emotion Type")+
  ylab("Average Emotion per Tweet")+
  scale_fill_manual(values=wes_palette(n=2, name="Royal1"))
