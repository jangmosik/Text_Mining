setwd("C:/R_workspace")
getwd()

# library---------------------------------------------------------------------------------------------------------

library(rvest)
library(readr)
library(dplyr)
library(stringr)
library(textclean)
library(tidytext)
library(KoNLP)
library(tidyr)
library(widyr)
library(tidygraph)
library(ggraph)
library(showtext)
library(tibble)
library(ggwordcloud)
library(ggplot2)
library(tm)
library(topicmodels)

# 1번 뉴스--------------------------------------------------------------------------------------------------------

url <- "https://www.yna.co.kr/view/AKR20230615046300002?input=1195m"

news_html <- read_html(url)

news_title <- news_html %>% html_nodes("h1.tit") %>% html_text()

news_content <- news_html %>% html_nodes("#articleWrap") %>% html_text()

news_1 <- data.frame(Title = news_title, Content = news_content)

news_title <- news_1 %>% select(Title) %>% 
  mutate(Title = str_replace_all(Title, "[^가-힣]", " "),Title = str_squish(Title))

news_content <- news_1 %>% select(Content) %>% 
  mutate(Content = str_replace_all(Content, "[^가-힣]", " "),Content = str_squish(Content))

news_1 <- data.frame(Title = news_title, Content = news_content) %>% as_tibble()

# write.csv(news_1, "news_1.csv", row.names = FALSE)

# 2번 뉴스---------------------------------------------------------------------------------------------------------

url <- "https://newsis.com/view/?id=NISX20230615_0002339888&cID=15001&pID=15000"

news_html <- read_html(url)

news_title <- news_html %>% html_nodes("h1") %>% html_text()

news_content <- news_html %>% html_nodes("#content > div.articleView > div.view > div.viewer > article") %>% 
  html_text()

news_2 <- data.frame(Title = news_title, Content = news_content)

news_title <- news_2 %>% select(Title) %>% 
  mutate(Title = str_replace_all(Title, "[^가-힣]", " "),Title = str_squish(Title))

news_content <- news_2 %>% select(Content) %>% 
  mutate(Content = str_replace_all(Content, "[^가-힣]", " "),Content = str_squish(Content)) 

news_2 <- data.frame(Title = news_title, Content = news_content) %>% as_tibble()

# write.csv(news_2, "news_2.csv", row.names = FALSE)

# 3번 뉴스---------------------------------------------------------------------------------------------------------

url <- "https://www.mk.co.kr/news/economy/10761277"

news_html <- read_html(url)

news_title <- news_html %>%
  html_nodes("#container > section > div.news_detail_head_group.type_none_bg > section > div > div > div > h2") %>%
  html_text()

news_content <- news_html %>%
  html_nodes("#container > section > div.news_detail_body_group > section > div.min_inner > div.sec_body > div.news_cnt_detail_wrap") %>%
  html_text()

news_3 <- data.frame(Title = news_title, Content = news_content)

news_title <- news_3 %>% select(Title) %>% 
  mutate(Title = str_replace_all(Title, "[^가-힣]", " "),Title = str_squish(Title)) 

news_content <- news_3 %>% select(Content) %>% 
  mutate(Content = str_replace_all(Content, "[^가-힣]", " "),Content = str_squish(Content)) 

news_3 <- data.frame(Title = news_title, Content = news_content) %>% as_tibble()

# write.csv(news_3, "news_3.csv", row.names = FALSE)

# 4번 뉴스---------------------------------------------------------------------------------------------------------

url <- "https://www.edaily.co.kr/news/read?newsId=01843366635642048&mediaCodeNo=257&OutLnkChk=Y"

news_html <- read_html(url)

news_title <- news_html %>%
  html_nodes("#contents > section.center1080.position_r > section.aside_left > div.article_news > div.news_titles") %>%
  html_text()

news_content <- news_html %>%
  html_nodes("#contents > section.center1080.position_r > section.aside_left > div.article_news > div.newscontainer > div.news_body") %>%
  html_text()

news_4 <- data.frame(Title = news_title, Content = news_content)

news_title <- news_4 %>% select(Title) %>% 
  mutate(Title = str_replace_all(Title, "[^가-힣]", " "),Title = str_squish(Title))

news_content <- news_4 %>% select(Content) %>% 
  mutate(Content = str_replace_all(Content, "[^가-힣]", " "),Content = str_squish(Content))

news_4 <- data.frame(Title = news_title, Content = news_content) %>% as_tibble()

# write.csv(news_4, "news_4.csv", row.names = FALSE)

# 5번 뉴스---------------------------------------------------------------------------------------------------------

url <- "https://www.sedaily.com/NewsView/29QV5AQQWN"

news_html <- read_html(url)

news_title <- news_html %>%
  html_nodes("#v-left-scroll-in > div.article_head > h1") %>%
  html_text()

news_content <- news_html %>%
  html_nodes("#v-left-scroll-in > div.article_con > div.con_left > div.article_view") %>%
  html_text()

news_5 <- data.frame(Title = news_title, Content = news_content)

news_title <- news_5 %>% select(Title) %>% 
  mutate(Title = str_replace_all(Title, "[^가-힣]", " "),Title = str_squish(Title))

news_content <- news_5 %>% select(Content) %>% 
  mutate(Content = str_replace_all(Content, "[^가-힣]", " "),Content = str_squish(Content))

news_5 <- data.frame(Title = news_title, Content = news_content) %>% as_tibble()

# write.csv(news_5, "news_5.csv", row.names = FALSE)

# 6번 뉴스---------------------------------------------------------------------------------------------------------

url <- "https://view.asiae.co.kr/article/2023061509383266381"

news_html <- read_html(url)

news_title <- news_html %>%
  html_nodes("#container > div.content > div.cont_sub.cont02023 > div.area_title > h1") %>%
  html_text()

news_content <- news_html %>%
  html_nodes("#txt_area") %>%
  html_text()

news_6 <- data.frame(Title = news_title, Content = news_content)

news_title <- news_6 %>% select(Title) %>% 
  mutate(Title = str_replace_all(Title, "[^가-힣]", " "),Title = str_squish(Title))

news_content <- news_6 %>% select(Content) %>% 
  mutate(Content = str_replace_all(Content, "[^가-힣]", " "),Content = str_squish(Content))

news_6 <- data.frame(Title = news_title, Content = news_content) %>% as_tibble()

# write.csv(news_6, "news_6.csv", row.names = FALSE)

# 7번 뉴스---------------------------------------------------------------------------------------------------------

url <- "https://www.news1.kr/articles/5077944"

news_html <- read_html(url)

news_title <- news_html %>%
  html_nodes("#article_body_content > div.title > h2") %>%
  html_text()

news_content <- news_html %>%
  html_nodes("#articles_detail") %>%
  html_text()

news_7 <- data.frame(Title = news_title, Content = news_content)

news_title <- news_7 %>% select(Title) %>% 
  mutate(Title = str_replace_all(Title, "[^가-힣]", " "),Title = str_squish(Title))

news_content <- news_7 %>% select(Content) %>% 
  mutate(Content = str_replace_all(Content, "[^가-힣]", " "),Content = str_squish(Content))

news_7 <- data.frame(Title = news_title, Content = news_content) %>% as_tibble()

# write.csv(news_7, "news_7.csv", row.names = FALSE)

# 8번 뉴스---------------------------------------------------------------------------------------------------------


url <- "http://news.heraldcorp.com/view.php?ud=20230615000391"

news_html <- read_html(url)

news_title <- news_html %>%
  html_nodes("body > div.wrap > div.view_bg > div.view_area > div.article_wrap > div.article_top > ul > li.article_title.ellipsis2") %>%
  html_text()

news_content <- news_html %>%
  html_nodes("#articleText") %>%
  html_text()

news_8 <- data.frame(Title = news_title, Content = news_content)

news_title <- news_8 %>% select(Title) %>% 
  mutate(Title = str_replace_all(Title, "[^가-힣]", " "),Title = str_squish(Title))

news_content <- news_8 %>% select(Content) %>% 
  mutate(Content = str_replace_all(Content, "[^가-힣]", " "),Content = str_squish(Content))

news_8 <- data.frame(Title = news_title, Content = news_content) %>% as_tibble()

# write.csv(news_8, "news_8.csv", row.names = FALSE)

# 뉴스 완료---------------------------------------------------------------------------------------------------------

bind_news <- bind_rows(news_1,news_2,news_3,news_4,news_5,news_6,news_7,news_8) %>% select(Content)

token_news <- bind_news %>% unnest_tokens(input = Content, output = word, token = extractNoun) %>% 
  count(word, sort = T) %>% filter(str_count(word)>1)

top20_word <- token_news %>% head(20)

top20_word

ggplot(top20_word, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() + 
  geom_text(aes(label = n), hjust = -0.1) + labs(title = "뉴스 기사 단어 빈도", x=NULL, y= NULL)

ggplot(token_news,aes(label = word, size = n, col = n)) + geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(10,NA),range = c(3,30)) + scale_color_gradient(low = "#66aaf2", high = "#003EA1")

# 오즈비 분석-------------------------------------------------------------------------------------------------------

news_1oz <- news_1 %>% mutate(news_number = "1번 뉴스")
news_2oz <- news_2 %>% mutate(news_number = "2번 뉴스")
news_3oz <- news_3 %>% mutate(news_number = "3번 뉴스")
news_4oz <- news_4 %>% mutate(news_number = "4번 뉴스")
news_5oz <- news_5 %>% mutate(news_number = "5번 뉴스")
news_6oz <- news_6 %>% mutate(news_number = "6번 뉴스")
news_7oz <- news_7 %>% mutate(news_number = "7번 뉴스")
news_8oz <- news_8 %>% mutate(news_number = "8번 뉴스")

bind_newsoz <- bind_rows(news_1oz,news_2oz,news_3oz,news_4oz,news_5oz,news_6oz,news_7oz,news_8oz) %>% 
  select(Content,news_number)

token_newsoz <- bind_newsoz %>% unnest_tokens(input = Content, output = word, token = extractNoun)

freq_news <- token_newsoz %>% count(news_number, word) %>% filter(str_count(word)>1)

freq_news <- freq_news %>% bind_tf_idf(term = word, document = news_number, n = n) %>% arrange(-tf_idf)

top10_freq_news <- freq_news %>% group_by(news_number) %>% slice_max(tf_idf, n = 10, with_ties = F)

ggplot(top10_freq_news, aes(x = reorder_within(word, tf_idf, news_number), y = tf_idf, fill = news_number)) +
  geom_col(show.legend = F) + coord_flip() + facet_wrap(~news_number, scales = "free", ncol = 2) +
  scale_x_reordered() + labs(x = NULL)

# 감정 분석-------------------------------------------------------------------------------------------------------

dic <- read_csv("knu_sentiment_lexicon.csv")

dic <- dic %>% mutate(sentiment = ifelse(polarity >= 1, "pos", ifelse(polarity <= -1, "neg", "neu")))

word_comment <- bind_news %>% unnest_tokens(input = Content, output = word, token = "words", drop = F) %>% select(word)

word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity),0, polarity))

word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity >= 1, "pos", ifelse(polarity <= -1, "neg", "neu")))

top10_sentiment <- word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>%
  slice_max(n, n=10)

ggplot(top10_sentiment, aes(x=reorder(word,n), y = n, fill = sentiment)) + geom_col() + coord_flip() + 
  geom_text(aes(label = n), hjust = -0.1) + facet_wrap(~sentiment, scales = "free") + 
  labs(x = NULL)

# 토픽모델링-------------------------------------------------------------------------------------------------------

topic_news <- bind_newsoz %>% distinct(Content, .keep_all = T) %>% filter(str_count(Content, boundary("word")) >= 3)

topic_word <- topic_news %>% unnest_tokens(input = Content, output = word, token = extractNoun, drop = F) %>% 
  filter(str_count(word) > 1) %>% group_by(news_number) %>% distinct(word, .keep_all = T) %>% ungroup() %>% 
  select(news_number, word)

count_word <- topic_word %>% add_count(word) %>% filter(n <= 200) %>% select(-n)

count_word %>% count(word, sort = T) %>% print(n = 200)

stopword <- c("년간","경우","들이","하면","내외","때문","해달","이라")

count_word <- count_word %>% filter(!word %in% stopword) %>% mutate(word = recode(word,
                                                                                  "상담직원" = "상담원",
                                                                                  "적립식" = "적립",
                                                                                  "소영" = "김소영",
                                                                                  "원장" = "부위원장"))

count_word_doc <- count_word %>% count(news_number, word, sort = T)

dtm_comment <- count_word_doc %>% cast_dtm(document = news_number, term = word, value = n)

as.matrix(dtm_comment[1:8, 1:8])

lda_model <- LDA(dtm_comment, k = 8, methods = "Gibbs", control = list(seed = 1234))

term_topic <- tidy(lda_model, matrix = "beta")

terms(lda_model, 10) %>% data.frame()

doc_topic <- tidy(lda_model, matrix = "gamma")

doc_class <- doc_topic %>% group_by(document) %>% slice_max(gamma, n = 1)

news_topic <- bind_newsoz %>% left_join(doc_class, by = c("news_number" = "document"))

news_topic %>% select(news_number, topic)

top_terms <- term_topic %>% group_by(topic) %>% slice_max(beta, n = 6, with_ties = F) %>% 
  summarise(term = paste(term, collapse = ", "))

count_topic <- news_topic %>% count(topic)

count_topic_word <- count_topic %>% left_join(top_terms, by = "topic") %>% mutate(topic_names = paste("TOPIC", topic))

name_topic <- tibble(topic = 1:8,
                     name = c("1. 경찰, 공안, 뉴스 영상 내용",
                              "2. 금융, 급여, 기여금 금융 내용",
                              "3. 가정, 개인소득, 가구 가정 내용",
                              "4. 가정, 감격, 감독직 긍정적 내용",
                              "5. 검토, 공시, 공헌 법적 내용",
                              "6. 가정, 개인소득, 관심 내용",
                              "7. 가정, 계좌, 금리 내용",
                              "8. 검토, 공시, 공헌 법적 내용"))

top_term_topic <- term_topic %>% group_by(topic) %>% slice_max(beta, n = 10)

top_term_topic_name <- top_term_topic %>% left_join(name_topic, by = "topic")

ggplot(top_term_topic_name, aes(x=reorder_within(term, beta, name), y = beta, fill = factor(topic))) +
  geom_col(show.legend = F) + facet_wrap(~name, scales = "free", ncol = 2) + coord_flip() + scale_x_reordered()
