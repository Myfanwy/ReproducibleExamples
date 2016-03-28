load('/Users/myfanwy/Dropbox/BlogPosts/rawposts_rmd/data-2016-03-28/sample_fishdata.RData')
head(d)
d$DateTimeUTC <- ymd_hms(d$DateTimeUTC)
d$TagID <- as.character(d$TagID)
str(d)

d2 <- d %>% 
  arrange(DateTimeUTC) %>% 
  group_by(TagID, Station) %>% 
  filter(!duplicated(Station))
d2 # this dataframe is now a complete record of where each fish was detected at least once.

enchist <- with(d2, table(TagID, Station)) # 
enchist

ctab <- apply(enchist, 1, paste0, collapse="") # creates factored atomic vector with TagIDs as one level, and the collapsed encounter history as the other

data.frame(TagID=names(ctab),ch=ctab) # this is the object you would save as the .inp file for MARK.

## Begin
zm <- d # just to differentiate from code above
tag.list = as.character(unique(zm$TagID)) # create a vector of all tags (codes) detected
sta.list = as.character(unique(zm$Station)) # make a vector of the station names

# create empty data frame for filling encounter history later
enc.hist = as.data.frame(matrix(rep(NA,(length(tag.list)*length(sta.list))),
                                length(tag.list), length(sta.list)))
colnames(enc.hist) = sta.list
rownames(enc.hist) = tag.list

# fill in data frame using a for-loop:
for (i in 1:length(sta.list))
{
  sub <- zm[zm$Station == sta.list[i],] #subset datos down to just the station you're currently looping
  subtags <- unique(sub$TagID) #creates vector of tags present at that station
  enc.hist[,i] <- tag.list %in% subtags #fills in the column of enc.hist with True or False if that tag is seen or not
}
head(enc.hist) # you now have a matrix with TRUE (1)/FALSE (0) for encounters

## Finally, use logical syntax to convert TRUE to '1' and FALSE to '0'
enc.hist[enc.hist==TRUE] <- 1
enc.hist[enc.hist==FALSE] <- 0
enc.hist

# Now collapse to encounter histories for RMARK:
rmarktab <- apply(enc.hist, 1, paste0, collapse="")
rmarktab
data.frame(TagID = names(rmarktab), ch = rmarktab)