setwd("C:/Users/hp/Desktop/R projects/Virtual Internship")
getwd()
install.packages("dplyr")
library(data.table)
require(ggplot2)
library(ggplot2)
library(ggmosaic)
library(readr)
transactionData <- fread("QVI_transaction_data.csv")
customerData <- fread("QVI_purchase_behaviour.csv")
# Exploratory data analysis
# The first step in any analysis is to first understand the data. Let's take a look
# at each of the datasets provided.
# ### Examining transaction data
# We can use `str()` to look at the format of each column and see a sample of the
# data. As we have read in the dataset as a `data.table` object, we can also run
# `transactionData` in the console to see a sample of the data or use
# `head(transactionData)` to look at the first 10 rows.

head(transactionData)

# Let's check if columns we would expect to be numeric are in numeric form and date
# columns are in date format.
# ```{r Examining transaction data}
# #### Examine transaction data
# We can see that the date column is in an integer format. Let's change this to a
# date format.
# ```{r Convert DATE to date format}
# #### Convert DATE column to a date format
# #### A quick search online tells us that CSV and Excel integer dates begin on 30
# Dec 1899

transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# We should check that we are looking at the right products by examining PROD_NAME.
# ```{r Summary of PROD_NAME}
# #### Examine PROD_NAME

summary(transactionData$PROD_NAME)

# Looks like we are definitely looking at potato chips but how can we check that
# these are all chips? We can do some basic text analysis by summarising the
# individual words in the product name.
# ```{r Further examine PROD_NAME}
# #### Examine the words in PROD_NAME to see if there are any incorrect entries
# #### such as products that are not chips
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), "
                                           ")))
setnames(productWords, 'words')

# As we are only interested in words that will tell us if the product is chips or
# not, let's remove all words with digits and special characters such as '&' from our
# set of product words. We can do this using `grepl()`

# # Over to you! Remove digits, and special characters, and then sort the distinct
# words by frequency of occurrence.
# #### Removing digits
No.digits <- gsub("\\w*[0-9]+\\w*", "", setnames(productWords, 'words'))
No.digits

#### Removing special characters
No.specialchar.digits <- gsub("[[:punct:]]", " ", No.digits)
No.specialchar.digits

#### Let's look at the most common words by counting the number of times a word
# appears and
#### sorting them by this frequency in order of highest to lowest frequency
install.packages("tm")
library("tm")
??tm
install.packages("qdap")
library(qdap)
library(dplyr)

str(productWords)
transactionData %>%
  count(PROD_NAME, sort = TRUE)

Product_Names <- transactionData$PROD_NAME
head(Product_Names)

Product_Names_Source <- VectorSource(Product_Names)
Product_Names_Source

Product_Names_Source_Corpus <- VCorpus(Product_Names_Source)
Product_Names_Source_Corpus

Product_Names_Source_Corpus[[5]]

content(Product_Names_Source_Corpus[[5]])

install.packages("SnowballC")
freqterms.tdm <- TermDocumentMatrix(Product_Names_Source_Corpus, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(1, Inf)))) 
ft <- findFreqTerms(freqterms.tdm, lowfreq = 100, highfreq = Inf)
ft
as.matrix(freqterms.tdm[ft,])
ft.tdm <- as.matrix(freqterms.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE)
# There are salsa products in the dataset but we are only interested in the chips
# category, so let's remove these.
# ```{r}
# #### Remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

# Next, we can use `summary()` to check summary statistics such as mean, min and max
# values for each feature to see if there are any obvious outliers in the data and if
# there are any nulls in any of the columns (`NA's : number of nulls` will appear in
#                                            the output if there are any nulls).
# ```{r initial summary}
# #### Summarise the data to check for nulls and possible outliers

summary(transactionData)

# There are no nulls in the columns but product quantity appears to have an outlier
# which we should investigate further. Let's investigate further the case where 200
# packets of chips are bought in one transaction.
# ```{r }
# #### Filter the dataset to find the outlier
# # Over to you! Use a filter to examine the transactions in questionstep-1
require(dplyr)
step_1 <- select(transactionData, PROD_QTY, LYLTY_CARD_NBR)
step_1
step_2 <- filter(step_1, PROD_QTY == 200)
step_2

# There are two transactions where 200 packets of chips are bought in one transaction
# and both of these transactions were by the same customer.
# ```{r}
# #### Let's see if the customer has had other transactions
# # Over to you! Use a filter to see what other transactions that customer made.
step_3 <- filter(step_2, LYLTY_CARD_NBR == 226000)
step_3
# It looks like this customer has only had the two transactions over the year and is
# not an ordinary retail customer. The customer might be buying chips for commercial
# purposes instead. We'll remove this loyalty card number from further analysis.
# ```{r}
# #### Filter out the customer based on the loyalty card number
step_4 <- transactionData[transactionData$LYLTY_CARD_NBR !=22600, ]
step_4

# That's better. Now, let's look at the number of transaction lines over time to see
# if there are any obvious data issues such as missing data.
# ```{r}
# #### Count the number of transactions by date
# # Over to you! Create a summary of transaction count by date.
step_4 %>%
  distinct(DATE)
step_4 %>%
  count(DATE)

# There's only 364 rows, meaning only 364 dates which indicates a missing date. Let's
# create a sequence of dates from 1 Jul 2018 to 30 Jun 2019 and use this to create a
# chart of number of transactions over time to find the missing date.

original_dates <- seq(as.Date("2018-07-01"), to= as.Date("2019-06-30"), by = "days")
str(original_dates)

df= step_4 %>% full_join(original_dates,by="DATE")
df
