#######################################################
# 3. 8/22/2017 Text variables from concatenating text columns from #2 plus country, development objectives and secondary/tertiaty specializations
# Yukun prepared the file for all WB
#######################################################

### read and prepare data -----------
library(data.table)

skills_wb <- fread("data/skill_df.csv")
# contains 3 columns:
# specializations, employee background and project description

# filter by TAC employees for now, as the corpus becomes too big to handle otherwise. 
#upi_tac <- filter(data_tsne, Region == "TAC")$main_object
#write.csv(upi_tac,"data/upi_tac.csv",row.names = FALSE)
upi_tac <- read.csv("data/upi_tac.csv")
upi_tac <- as.numeric(upi_tac$x)
skills_wb <- filter(skills_wb, Employee_UPI_9 %in% upi_tac)

# upi to employee name
#upi_name <- select(data_tsne, upi=main_object,name=Country)
#write.csv(upi_name,"data/upi_name.csv",row.names = FALSE)
upi_name <- read.csv("data/upi_name.csv")

# First task: concatenate all into 1 big text
skills_wb <- group_by(skills_wb,Employee_UPI_9) %>%
  summarise(Employee_All_Skills = paste(specializations, Employee_bg,Project_description, collapse = " ")) %>%
  distinct(Employee_UPI_9,Employee_All_Skills) %>%
  as.data.frame()

#### Create a Corpus off a character vector ---------------
library(tm)
# simple Corpus (fast but can't seem to assign UPI names to their IDs)
#skillCorp <- SimpleCorpus(VectorSource(skills_efi$Employee_Skill))
# full Corpus reading from data.frame to be able to keep UPI as IDs to merge back to original data
skillCorp = VCorpus(DataframeSource(skills_wb), readerControl = list(reader = readTabular(mapping = list(content = "Employee_All_Skills", id = "Employee_UPI_9"))))

# make transformations
skillCorp <- tm_map(skillCorp, stripWhitespace) # remove extra white spaces
skillCorp <- tm_map(skillCorp, removeWords, stopwords("english")) # remove stop words
skillCorp <- tm_map(skillCorp, removeNumbers)
skillCorp <- tm_map(skillCorp, removePunctuation)
skillCorp <- tm_map(skillCorp, content_transformer(tolower))
#skillCorp <- tm_map(skillCorp, stemDocument) # stemming

# filter out connectors like: "and", "amp"
f <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
skillCorp2 <- tm_map(skillCorp, f, " and | amp ")
skillCorp2 <- tm_map(skillCorp2, f, "•")

#### Create TDM --------------------
#skillCorp3 <- Corpus(VectorSource(skillCorp2)) # create a simpler Corpus
tdm <- DocumentTermMatrix(skillCorp2)
#rownames(tdm) <- skillNames
inspect(tdm)

## Filter out least frequent words ------------
freqWords <- findFreqTerms(tdm, 20) # find words repeated at least X times
#freqWords <- freqWords[which(nchar(freqWords)>3)]
tdm <- as.matrix(tdm)
tdm <- tdm[,freqWords]

## Keep words with high variation coefficient ---------------
# meaning they are uncommon to most UPIs thus will help us cluster them up
tdm2 <- summarise_all(as.data.frame(tdm), funs(sd(.)/mean(.))) %>%
  gather(word,sdev) %>%
  filter(sdev > 2.5)
tdm <- tdm[,tdm2$word]

## Compute correlation matrix
#tdm_df <- as.data.frame(tdm)
scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
tdm_scaled <- scale01(tdm)
corr_matrix <- round(cor(tdm_scaled),2)
#write.csv(corr_matrix, "data/corr_matrix.csv")

# return highly correlated words to reduce the size of the bag of words and avoid linearly correlated words
# For our purpose, we need all the words as the "input word" is the reference to obtain highly correlated words. 
# So, keep the top_corr variable to 0 for now.
top_corr <- 0 # [0: return all words, 1: return empty data.frame]

corr_high <- as.data.frame(corr_matrix)
corr_high$wordA <- row.names(corr_high)
corr_high <- gather(corr_high, wordB, distance, -wordA) %>% 
  filter(abs(distance) > top_corr) %>%
  arrange(wordA,wordB)

# corr_high <- data.frame()
# row_i <- 1
# for (i in 1:(nrow(corr_matrix)-1)){
#   for (j in i:nrow(corr_matrix)){
#     if (abs(corr_matrix[i,j]) > top_corr){
#       #print(paste0(row.names(corr_matrix)[i],"-",row.names(corr_matrix)[j]))
#       corr_high[row_i,1] <- row.names(corr_matrix)[i]
#       corr_high[row_i,2] <- row.names(corr_matrix)[j]
#       corr_high[row_i,3] <- corr_matrix[i,j]
#       row_i <- row_i + 1 
#     }
#   }
# }
# names(corr_high) <- c("wordA","wordB","distance")
# corr_high_reverse <- select(corr_high, wordA = wordB,wordB = wordA, distance)
# corr_high <- bind_rows(corr_high,corr_high_reverse) %>% arrange(wordA,wordB,distance)

######################################################################################################
######################################################################################################

##### Test scenario 1 ######
# start with an input word. 
# Rank UPIs according to the input word
##########################
input_word <- "tourism"

# list of related words and their distance to the input word
word_weights <- filter(corr_high, grepl(input_word,wordA)) %>% 
  distinct(wordB,distance) %>% 
  arrange(desc(distance))
names(word_weights) <- c("word","weight")

# easier to work with data.frame rather than with matrices
tdm_df <- as.data.frame(tdm)
tdm_df$upi <- row.names(tdm_df)
tdm_df <- gather(tdm_df, word,frequency, -upi) %>% 
  #mutate(frequency = ifelse(frequency>0,1,0)) # relative weight of word frequency
  #mutate(frequency = scale01(frequency)) # relative weight of word frequency
  group_by(upi) %>%
  mutate(freq_total = sum(frequency)) %>%
  ungroup() %>%
  mutate(frequency = frequency/freq_total) %>%
  select(-freq_total) %>%
  arrange(upi,desc(frequency),word)

# attach weights and calculate scores.
score_df <- merge(tdm_df,word_weights, by="word") %>%
  group_by(upi) %>%
  mutate(score = weighted.mean(frequency,weight)) %>%
  ungroup() %>%
  distinct(upi,score) %>%
  arrange(desc(score))

# check top score UPI
score_check <- filter(tdm_df, upi == score_df$upi[1], frequency > 0)
skill_check <- filter(skills_wb, Employee_UPI_9 %in% score_df$upi[1:10]) %>% 
  select(upi = Employee_UPI_9, skill = Employee_All_Skills) %>%
  merge(upi_name, by="upi") %>%
  merge(score_df, by="upi") %>% 
  arrange(desc(score))


##### Test scenario 2 ######
# start with multiple input words. 
##########################
input_word <- "data analysis trade"

# list of related words and their distance to the input word
word_weights <- filter(corr_high, grepl(paste(strsplit(input_word," ")[[1]],collapse = "|"),wordA)) %>% 
  group_by(wordB) %>%
  filter(distance == max(distance)) %>%
  distinct(wordB, .keep_all=TRUE) %>%
  select(word = wordB, weight = distance) %>%
  arrange(desc(weight))


# easier to work with data.frame rather than with matrices
tdm_df <- as.data.frame(tdm)
tdm_df$upi <- row.names(tdm_df)
tdm_df <- gather(tdm_df, word,frequency, -upi) %>% 
  #mutate(frequency = ifelse(frequency>0,1,0)) # relative weight of word frequency
  #mutate(frequency = scale01(frequency)) # relative weight of word frequency
  group_by(upi) %>%
  mutate(freq_total = sum(frequency)) %>%
  ungroup() %>%
  mutate(frequency = frequency/freq_total) %>%
  select(-freq_total) %>%
  arrange(upi,desc(frequency),word)

# attach weights and calculate scores.
score_df <- merge(tdm_df,word_weights, by="word") %>%
  group_by(upi) %>%
  mutate(score = weighted.mean(frequency,weight)) %>%
  ungroup() %>%
  distinct(upi,score) %>%
  arrange(desc(score))

# check top score UPI
score_check <- filter(tdm_df, upi == score_df$upi[1], frequency > 0)

skill_check <- filter(skills_wb, Employee_UPI_9 %in% score_df$upi[1:10]) %>% 
  select(upi = Employee_UPI_9, skill = Employee_All_Skills) %>%
  merge(upi_name, by="upi") %>%
  merge(score_df, by="upi") %>% 
  arrange(desc(score))








