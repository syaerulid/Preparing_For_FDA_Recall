install.packages('rlang')
install.packages('tidyverse')
library(tidytext)
library(tidyr)
library(lubridate)
install.packages('readxl')
library(readxl)
install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)
install.packages('lessR')
library(lessR)
install.packages('writexl')
library(writexl)
install.packages("data.table")
library(data.table)
install.packages("wordcloud")
library(wordcloud)


#Import Dataset
df_fda <- read_excel("F:/Self Learning/Tableu - Udemy/Data gov/FDA_Product_Recalls.xlsx")

# Data Understanding
str(df_fda)

# In this data we notice that FEI Number, Center Classification Year, 
# Center Classification Month Year,Event ID and Product ID have character type data
# we need to change them to number type. Also almost all column have spaces as separator for column name, we need to change to underscore(_)

# change name to underscore and lowercase
# Iterate over the column names of the dataframe
new_column_names <- sapply(colnames(df_fda), function(col_name) {
  new_name <- gsub(" ", "_", col_name)  # Replace spaces with underscores
  tolower(new_name)  # Convert to lowercase
})

# Assign the new column names to the dataframe
colnames(df_fda) <- new_column_names

# Change fei_number to Numeric Type
df_fda$fei_number <- gsub("[^0-9]", "", df_fda$fei_number)
df_fda$fei_number <- as.numeric(df_fda$fei_number)

# Change event_id to numeric format
df_fda$event_id <- as.numeric(df_fda$event_id)

# change product id to numeric format
df_fda$product_id <- as.numeric(df_fda$product_id)

# Change center_classification_year to numeric format
df_fda$center_classification_year <- as.numeric(df_fda$center_classification_year)

#1. checking if there is any duplicated and missing data
sum(is.na(df_fda))
sum(duplicated(df_fda))

#2. which year has the most recall in last 10 years?

# create filtered year
df_count_year <- df_fda %>%
  filter(center_classification_year != 2023) %>% 
  group_by(center_classification_year) %>% 
  summarise(total_cases = n ())

# average distribution of year
average_year <- mean(df_count_year$total_cases)

# create the barchart
years_cases = ggplot(df_count_year, aes(x = center_classification_year, y = total_cases)) +
  geom_col() +
  geom_text(aes(label = total_cases), vjust = -0.5) +
  labs(x = 'Year') +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2), labels = c("2012", "2014", "2016", "2018", "2020", "2022")) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  geom_hline(yintercept = average_year, color = "red", linetype = "dashed")  +
  annotate("text", x = 2012, y = average_year, label = paste("Average:", round(average_year, 0)),
           color = "blue", vjust = -1, hjust = -6) +
  labs(title = "Distribution of FDA Recall in Last 10 Years")

years_cases

#3. how many FEI_Number that US federal printed for last 10 years?
# we create filter_df first
filtered_df <- df_fda %>% 
  filter(center_classification_year != 2023) %>% 

n_distinct(filtered_df$fei_number) # 8293

fei_df <- filtered_df %>% 
  group_by(fei_number) %>% 
  summarise(total_case = n())

# 4. how was the division of product_type for the last 10 years?
# we count distinct values first of product_type

df_count_product <- filtered_df %>% group_by(product_type) %>% summarise(total_product = n())
df_count_product
# create the piechart based on df_count_product
counts_product_type <- factor(c(rep("Biologics", 10783),
                          rep("Devices", 29083),
                          rep("Drugs", 14703),
                          rep("Food/Cosmetics", 23601),
                          rep("Tobacco + Veterinary", 3075)))
duct <- data.frame(counts_status)
cols <- hcl.colors(length(levels(counts_product_type)), "RedOr")
PieChart(counts_product_type, data = duct, hole = 0.4, fill = cols, labels_cex = 1, 
         main = "Product Type Distribution", labels_color = "black")

# bulan apa yang paling banyak direcall selama 10 tahun terakhir?
filtered_df$month <- month(filtered_df$center_classification_date, label = TRUE)

# group by bulan dan summarize
month_yes <- filtered_df %>% 
  group_by(month) %>% 
  summarise(total_month = n())

# create custom colors
month_colors <- c("grey49","grey49","grey49","grey49","red3","red3","red3","red3","grey49","grey49","grey49","grey49")
month_yes$color <- month_colors

# create barchart
ggplot(month_yes, aes(x = month, y = total_month, fill = color)) + 
  geom_col() +  scale_fill_manual(values=unique(month_colors)) +
  geom_text(aes(label = total_month), vjust = -0.5) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Distribution of FDA Recall by Month") + 
  guides(fill = guide_none())

#5. minggu apa yang paling banyak di recall selama 10 tahun terakhir? atau hari apa? bagaimana distribusinya?
# create new column name day_of_the_week
filtered_df$day_of_the_week <- NULL

# create the values of day of the week
wday(filtered_df$center_classification_date, week_start = 1)

# append the values to null column
filtered_df$day_of_the_week <- wday(filtered_df$center_classification_date, week_start = 1)

# create group by for day_of_the_week and summarise them
day_week <- filtered_df %>% 
  group_by(day_of_the_week) %>%
  summarise(total_day = n ())

View(day_week)

# create custom labels
day_labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# create the bar chart
ggplot(day_week, aes(x = factor(day_of_the_week,levels = 1:7), y = total_day)) + 
  geom_col() + geom_text(aes(label = total_day), vjust = -0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(labels = day_labels) +
  labs(title = "Distribution of Day for FDA Recall")

#convert day number to text in filtered_df dataframe
filtered_df <- filtered_df %>% 
  mutate(literal_day = case_when(
    day_of_the_week == 1 ~ "Monday",
    day_of_the_week == 2 ~ "Tuesday",
    day_of_the_week == 3 ~ "Wednesday",
    day_of_the_week == 4 ~ "Thursday",
    day_of_the_week == 5 ~ "Friday",
    day_of_the_week == 6 ~ "Saturday",
    day_of_the_week == 7 ~ "Sunday",
    TRUE ~ NA_character_
  ))

# apakah ada pattern bulanan dan harinya dimana fda melakukan recall?
# kombinasi
df_kombinasi <- filtered_df %>% 
  unite(trending_date, month, literal_day, sep = " ")

# summarise 
date_trend <- df_kombinasi %>% 
  group_by(trending_date) %>% 
  summarise(trend = n())

# top 10
top_10_date <- date_trend %>% 
  top_n(10, trend)

# reorder by month
top_10_date$trending_date <- factor(top_10_date$trending_date, 
                          levels = c("Apr Monday","May Thursday","Jul Wednesday","Aug Tuesday","Aug Thursday","Aug Friday","Oct Thursday","Nov Tuesday","Dec Tuesday","Dec Wednesday"))

# create barchart
ggplot(data = top_10_date, aes(x = trending_date, y = trend)) + geom_col() +
  geom_text(aes(label = trend), vjust = -0.5) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  labs(title = "Pattern FDA Date and Month in Last 10 Years")

# combine

# combine month + day of the week
df_combine <- filtered_df %>% 
  unite(trend_date, month, day_of_the_week, sep = " ")

# summarise 
trend_fda <- df_combine %>% 
  group_by(trend_date) %>% 
  summarise(trend = n ())

# summarise top 10
top_10_trend <- trend_fda %>% 
  top_n(10, trend)


# reorder by month
top_10_trend$trend_date <- factor(top_10_trend$trend_date, 
                                  levels = c("Apr 1","May 4","Jul 3","Aug 2","Aug 4","Aug 5","Oct 4","Nov 2","Dec 2","Dec 3"))

# create barchart
ggplot(data = top_10_trend, aes(x = trend_date, y = trend)) + geom_col() +
  geom_text(aes(label = trend), vjust = -0.5) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  labs(title = "Pattern FDA Date and Month in Last 10 Years")

# count companies
company_counts <- filtered_df %>% 
  group_by(recalling_firm_name) %>%
  summarise(count_recalls = n()) 

# top 10 companies
top_10_companies <- company_counts %>% 
  top_n(10, count_recalls)

# Top 10 Companies Recalled by FDA in Last 10 Years
ggplot(top_10_companies, aes(x = reorder(recalling_firm_name, +count_recalls), y = count_recalls)) + 
  geom_col() + geom_text(aes(label = count_recalls), vjust = -0.5) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), axis.title.x = element_blank()) + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + 
  labs(title = "Top 10 Recalled Companies by FDA in Last 10 Years")

# create custom stopwords
default_stopwords <- stop_words$word
custom_stopwords <- c("products","due","recalled","recall","product")

# combine stopwords
all_stopwords <- c(default_stopwords, custom_stopwords)

# Perform text mining using column 13 only
recall_bigrams <- filtered_df %>% 
  unnest_tokens(bigram, reason_for_recall, token = "ngrams", n = 2) %>% 
  filter(!is.na(bigram))

# sort the bigrams
recall_bigrams %>% 
  count(bigram, sort = TRUE)

# divide the bigrams into 2 words separated
bigrams_separated <- recall_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")
View(bigrams_separated)

# filtered the bigrams so it doesn't contain unecessary words
bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% all_stopwords) %>% 
  filter(!word2 %in% all_stopwords)
View(bigrams_filtered)

# count the separated bigrams that already filtered
bigrams_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
View(bigrams_counts)

# united the separated word from bigram
bigrams_united <- bigrams_counts %>% 
  unite(bigram, word1, word2, sep = " ")

View(bigrams_united)

# create barchart
# top 10 bigrams
top_10_bigrams <- bigrams_united %>% 
  top_n(10, n)

View(top_10_bigrams)

# create barchart
ggplot(data = top_10_bigrams, aes(x = reorder(bigram, +n), y = n, fill = reorder(bigram, +n))) + 
  geom_col() + coord_flip() + 
  scale_fill_manual(values=c("grey","grey","grey","grey","grey","grey","grey","grey","grey","red")) +
  geom_text(aes(label = n), hjust = 1.3) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  guides(fill = guide_none()) +
  labs(title = "Top 10 Reason why Product being Recalled by FDA in Last 10 Years")

# list companies that product inspected contamined with listeria bacteria
subset_data <- filtered_df[filtered_df$reason_for_recall %like% "listeria" | filtered_df$reason_for_recall %like% "monocytogenes", ]
# group by so we can count it
listeria_affected <- subset_data %>% 
  group_by(recalling_firm_name) %>% 
  summarise(listeria_count = n ())

# view top 10 company that produce product contamined with listeria
top_10_listeria <- listeria_affected %>% 
  top_n(10, listeria_count)

# create a chart
ggplot(data = top_10_listeria, aes(x = recalling_firm_name, y = listeria_count)) + geom_col() + coord_flip() +
  geom_text(aes(label = listeria_count), hjust = 1.3) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  labs(title = "Top 10 Firm that have Product contain Listeria Monocytogenes")