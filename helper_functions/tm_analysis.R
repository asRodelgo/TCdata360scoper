# Dummify text variables in talent

#### prepare data ----------------
#skills_efi <- talent_EFI[,c("Employee_UPI_9","Employee_Skill")]
#skills_efi$Employee_Skill <- as.character(skills_efi$Employee_Skill)
#skills_efi <- as.character(talent_EFI$Employee_Skill)
#write.csv(skills_efi, "data/skills_efi.csv", row.names = FALSE)
#### read data -------------------
skills_efi <- read.csv("data/skills_efi.csv")
skills_efi$Employee_Skill <- as.character(skills_efi$Employee_Skill)

# load tm package
library(tm)

#### Create a Corpus off a character vector ---------------

# simple Corpus (fast but can't seem to assign UPI names to their IDs)
#skillCorp <- SimpleCorpus(VectorSource(skills_efi$Employee_Skill))
# full Corpus reading from data.frame to be able to keep UPI as IDs to merge back to original data
skillCorp = VCorpus(DataframeSource(skills_efi), readerControl = list(reader = readTabular(mapping = list(content = "Employee_Skill", id = "Employee_UPI_9"))))

# make transformations
skillCorp <- tm_map(skillCorp, stripWhitespace) # remove extra white spaces
skillCorp <- tm_map(skillCorp, removeWords, stopwords("english")) # remove stop words
skillCorp <- tm_map(skillCorp, removeNumbers)
skillCorp <- tm_map(skillCorp, removePunctuation)
skillCorp <- tm_map(skillCorp, content_transformer(tolower))
#skillCorp <- tm_map(skillCorp, stemDocument) # stemming

# filter out connections like: "and", "amp"
f <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
skillCorp2 <- tm_map(skillCorp, f, " and | amp ")
skillCorp2 <- tm_map(skillCorp2, f, "•")

#### Create TDM --------------------
#skillCorp3 <- Corpus(VectorSource(skillCorp2)) # create a simpler Corpus
tdm <- DocumentTermMatrix(skillCorp2)
#rownames(tdm) <- skillNames
inspect(tdm)
findFreqTerms(tdm, 5) # find words repeated at least 5 times

# calculate frequency of words
freqTop <- sort(colSums(as.matrix(tdm)), decreasing=TRUE)

# save the matrix with rows as UPI and column names as words
write.csv(as.matrix(tdm), "data/termDocMatrix.csv")

# Find terms correlated to a selected term. Ex: gender, agribusiness, analytics, etc.
findAssocs(tdm,"entrepreneurship",0.4)








