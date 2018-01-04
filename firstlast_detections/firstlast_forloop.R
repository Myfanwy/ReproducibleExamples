library(ybp)
head(all69khz_grouped)
d <- all69khz_grouped

d_new <- as.data.frame(matrix(nrow = length(unique(d$TagID)), ncol = 3))
names(d_new) = c("TagID", "FirstStation", "FinalStation")
  head(d_new)
tag.ids <- unique(d$TagID)
for (i in 1:length(tag.ids)) {
  d_new$TagID <- tag.ids[i]
  d_new$FirstStation[i] <- d[ d$DateTimeUTC[i] == min(d$DateTimeUTC[i]), 4 ]
  d_new$FinalStation <- d[d$DateTimeUTC[i] == max(d$DateTimeUTC[i]), 4 ]
}

library(dplyr)
tagtest = sample(d$TagID, 1, replace = FALSE)
test <- filter(d, TagID %in% tagtest)
head(test)
testfirst <- test[test$DateTimeUTC == min(test$DateTimeUTC), 4]
testlast <- test[test$DateTimeUTC == max(test$DateTimeUTC), 4]

d_new <- as.data.frame(matrix(nrow = length(unique(test$TagID)), ncol = 3))
names(d_new) = c("TagID", "FirstStation", "FinalStation")
head(d_new)
tag.ids <- unique(test$TagID)
for (i in 1:tag.ids) {
  d_new$TagID <- tag.ids[i]
  d_new$FirstStation[i] <- test[test$DateTimeUTC[i] == min(test$DateTimeUTC[i]), 4 ]
  d_new$FinalStation <- test[test$DateTimeUTC[i] == max(test$DateTimeUTC[i]), 4 ]
}


dp <- d %>% 
  group_by(TagID, year) %>% 
  arrange(DateTimeUTC) %>% 
  summarise(first = Station[DateTimeUTC == min(DateTimeUTC)],
            last = Station[DateTimeUTC == max(DateTimeUTC)])

dp
