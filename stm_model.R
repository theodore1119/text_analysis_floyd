library(stm)
library(readxl)

data <- read_excel("all_floyd_cleaned_final.xlsx")
data$party <- factor(data$party, levels = c("R","I","D"))
data_1 <- subset(data, subset = !is.na(text_no_stopword))

processed <- textProcessor(data_1$text_no_stopword, metadata = data) 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

set.seed(1000)
first_stm <- stm(documents = out$documents, vocab = out$vocab, K = 10, prevalence = ~party, max.em.its = 75, data = out$meta, init.type = "Spectral", verbose = FALSE)
plot(first_stm)

predict_topics <- estimateEffect(formula = 1:10 ~ party, stmobj = first_stm, metadata = out$meta, uncertainty = "Global")
plot(predict_topics, covariate = "party", topics = c(1,2,3,4,5,6,7,8,9,10), model = first_stm, method = "difference", cov.value1 = "D", cov.value2 = "R", xlab = "More Republican                                                                                        More Democrat", labeltype = "custom", custom.labels = c("Topic 1","Topic 2","Topic 3","Topic 4","Topic 5","Topic 6","Topic 7","Topic 8","Topic 9","Topic 10"))

labelTopics(first_stm, c(3,4,5,8))

# Democrat topics
findThoughts(first_stm, texts = data_1$text, n=5, topics = 4)
findThoughts(first_stm, texts = data_1$text, n=5, topics = 5)
# Republican topics
findThoughts(first_stm, texts = data_1$text, n=5, topics = 3)
findThoughts(first_stm, texts = data_1$text, n=5, topics = 8)

# Contrast in words between Topics 4 and 8
plot(first_stm, type = "perspectives", topics = c(8,4))

# Compare only the Republicans and Democrats
data_2 <- subset(data_1, subset = party %in% c("R","D"))
processed_2 <- textProcessor(data_2$text_no_stopword, metadata = data_2) 
out_2 <- prepDocuments(processed_2$documents, processed_2$vocab, processed_2$meta)

# words in Topic 3
first_stm_content <- stm(out_2$documents, out_2$vocab, K = 10, prevalence = ~party, content = ~party, max.em.its = 75, data = out_2$meta, init.type = "Spectral", verbose = FALSE)
plot(first_stm_content, type ="perspectives", topics = 3)