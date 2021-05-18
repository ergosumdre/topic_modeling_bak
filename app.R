library(future, lib.loc = "/usr/lib64/R/library")
library(RJSONIO, lib.loc = "/usr/lib64/R/library")
library(proxy, lib.loc = "/usr/lib64/R/library")
library(cli, lib.loc = "/usr/lib64/R/library")
library(rstudioapi, lib.loc = "/usr/lib64/R/library")
library(backports, lib.loc = "/usr/lib64/R/library")
library(LDAvis, lib.loc = "/usr/lib64/R/library")
library(rJava, lib.loc = "/usr/lib64/R/library")
library(shiny, lib.loc = "/usr/lib64/R/library")
library(mallet, lib.loc = "/usr/lib64/R/library")
library(dplyr, lib.loc = "/usr/lib64/R/library")
library("data.table", lib.loc = "/usr/lib64/R/library")
library("dplyr", lib.loc = "/usr/lib64/R/library")
library("stringr", lib.loc = "/usr/lib64/R/library")
library(tidyverse, lib.loc = "/usr/lib64/R/library")
library(tidytext, lib.loc = "/usr/lib64/R/library")
library(stm, lib.loc = "/usr/lib64/R/library")
library(furrr, lib.loc = "/usr/lib64/R/library")
library(waiter, lib.loc = "/usr/lib64/R/library")




ui <- shinyUI(
  fluidPage(
    use_waitress(),
    visOutput('myChart1')
  )
)

server <- shinyServer(function(input, output, session) {
  # call the waitress
  waitress <- Waitress$
    new(theme = "overlay-percent")$
    start() # start
  
  for(i in 1:10){
    waitress$inc(10) # increase by 10%
    Sys.sleep(.3)
  }
  text <- readr::read_csv("/srv/shiny-server/dr_nic/nic_model/NJDeptofHealth_tweets_2020_12_07.csv")
  text <- text %>% filter(is_retweet != TRUE)
  text <- text %>% select(text)
  text <- text %>% filter(lapply(text, str_count) > 5)
  clean_text <- text %>%
    mutate(text = coalesce(text),
           text = gsub("&#x27;|&quot;|&#x2F;", "'", text), ## weird encoding
           text = gsub("<a(.*?)>", " ", text),             ## links
           text = gsub("&gt;|&lt;|&amp;", " ", text),      ## html yuck
           text = gsub("&#[:digit:]+;", " ", text),        ## html yuck
           text = gsub("<[^>]*>", "", text),                    ## mmmmm, more html yuck
           text = gsub("@\\w+", "", text),
           text = gsub("https?://.+", "", text),
           text = gsub("\\d+\\w*\\d*", "", text),
           text = gsub("#\\w+", "", text),
           text = gsub("[^\x01-\x7F]", "", text),
           text = gsub("[[:punct:]]", " ", text),
           text = gsub("https://t.co/", " ", text),
           text = gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", text),
           text = gsub("[A-Za-z]{1,5}[.][A-Za-z]{2,3}/[A-Za-z0-9]+\\b", "", text),
           text = gsub("\n", " ", text),
           text = gsub("^\\s+", "", text),
           text = gsub("\\s+$", "", text),
           text = gsub("[ |\t]+", " ", text),
           postID = row_number())
  
  # The reminding code is from the following Author: Thomas Keller.
  #This is the work used for the "Open Data Visualizations and Analytics as Tools for Policy-Making" published by Government Information Quarterly
  
  ntext=1:nrow(clean_text) #jtext$sid
  jtext <- clean_text
  #train using mallet
  #mallet.import: This function takes an array of document IDs and text files (as character strings) and converts them into a Mallet instance list.
  mall.instance <- mallet.import(
    as.character(ntext),
    jtext$text,
    "/srv/shiny-server/dr_nic/nic_model/en.txt",
    FALSE,
    token.regexp="[\\p{L}]+")
  
  #set number of topic
  topic.model2=MalletLDA(num.topics=40)
  topic.model2$loadDocuments(mall.instance)
  vocab2=topic.model2$getVocabulary()
  word.freqs2=mallet.word.freqs(topic.model2)
  topic.model2$setAlphaOptimization(40,80)
  topic.model2$train(400)
  
  topic.words.m<-mallet.topic.words(topic.model2,smoothed=TRUE,normalized=TRUE)
  
  dim(topic.words.m)
  
  vocabulary2 <- topic.model2$getVocabulary()
  colnames(topic.words.m) <- vocabulary2
  
  doc.topics.m <- mallet.doc.topics(topic.model2, smoothed=T,
                                    normalized=T)
  
  
  doc.topics.df <- as.data.frame(doc.topics.m)
  doc.topics.df <- cbind(ntext, doc.topics.df)
  
  doc.topic.means.df <- aggregate(doc.topics.df[,2:ncol(doc.topics.df)],
                                  list(doc.topics.df[,1]),mean)
  
  
  
  phi <- mallet.topic.words(topic.model2, smoothed = TRUE, normalized = TRUE)
  phi.count <- t(mallet.topic.words(topic.model2, smoothed = TRUE, normalized = FALSE))
  
  topic.words <- mallet.topic.words(topic.model2, smoothed=TRUE, normalized=TRUE)
  topic.counts <- rowSums(topic.words.m)
  
  topic.proportions <- topic.counts/sum(topic.counts)
  
  vocab2 <- topic.model2$getVocabulary()
  
  doc.tokens <- data.frame(id=c(1:nrow(doc.topics.m)), tokens=0)
  for(i in vocab2){
    # Find word if word in text
    matched <- grepl(i, jtext$text)
    doc.tokens[matched,2] =doc.tokens[matched,2] +  1
  }
  
  
  output$myChart1 <- renderVis({
    # this process takes long time. just use a small sample first. Commenting this for now because I've ran this to create a json file.
    createJSON(phi = phi,
               theta = doc.topics.m,
               doc.length = doc.tokens$tokens,
               vocab = vocab2,
               term.frequency = apply(phi.count, 1, sum))
    
  })
  waitress$close()
  
  
  
})

shinyApp(ui = ui, server = server)
