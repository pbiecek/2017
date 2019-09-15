# Best 250 series
# http://www.imdb.com/chart/toptv/
## (1) read the new data with archivist

library(archivist)
series2017 <- aread("mi2-warsaw/RLadies/arepo/45aa16dc4dbf0d87e3e40eb9dc9d18ae")

## (2) or read the old data with Pogromcy Danych

library(PogromcyDanych)
serialeIMDB

## (3) or scrap the data from IMDB database

library(rvest)
library(dplyr)

# read links and titles 
page <- read_html("http://www.imdb.com/chart/toptv/")
series <- html_nodes(page, ".titleColumn a")
titles <- html_text(series)
links <- html_attr(series, "href")
codes <- sapply(strsplit(links, split = "/"), `[`, 3)

# read details for series
allSeries <- lapply(seq_along(codes), function(i) {
  tab <- read_html(paste0("http://www.imdb.com/title/",codes[i],"/epdate?ref_=ttep_ql_4")) %>%
    html_node("table") %>%
    html_table()
  data.frame(Serie = titles[i], tab[,1:4], Season = gsub(tab[,1], pattern="\\..*", replacement=""))
})

# put all together
series2017 <- do.call(rbind, allSeries)
series2017$UserVotes <- as.numeric(gsub(series2017$UserVotes, pattern = "[^0-9]", replacement = ""))
series2017 %>% 
  group_by(Serie) %>%
  mutate(id = seq_along(Serie)) %>%
  ungroup() -> series2017

##
## Time for some datavis
##
selected <- c("Gra o tron", "Breaking Bad", 
              "Sherlock", "Westworld")

dat <- series2017 %>%
  dplyr::filter(Serie %in% selected)

ggplot(dat, aes(id, UserRating, color=Season)) + 
  geom_point(aes(size=UserVotes, shape=Season)) +
  geom_smooth(se=FALSE, color="black") + 
  scale_shape_discrete(solid=FALSE)+
  scale_color_manual(values = c("red4", "red1", "grey", "black", "blue1", "blue4")) +
  facet_grid(Serie ~ .) +
  theme_light() + 
  ggtitle("User ratings for selected TV series") + 
  xlab("Episode No") + 
  scale_y_continuous(trans = "log2", breaks = c(8,9,10))


ggplot(dat, aes(id, UserRating)) + 
  geom_point(aes(color=Season, size=UserVotes)) +
  geom_smooth(se=FALSE, color="grey30") + 
  facet_grid(Serie~.) +
  theme_light() + theme(legend.position="none") +
  ggtitle("User ratings for selected TV series") + 
  xlab("Episode No")


#
# Let's practice
#

mySer <- dplyr::filter(dat, Serie == "Breaking Bad")
head(mySer)

ggplot(mySer, aes(x = id, y = UserRating)) +
  geom_point() +
  geom_smooth(color="red", se = FALSE) + 
  geom_smooth(method = "lm") 
  

ggplot(mySer, aes(x = id, y = UserRating,
                  size = UserVotes, shape = Season)) +
  geom_point() +
  geom_smooth(color="red", se = FALSE) 

ggplot(mySer, aes(x = id, y = UserRating,
                  size = UserVotes, color = Season)) +
  geom_point() +
  geom_smooth(se = FALSE) 

ggplot(mySer, aes(x = id, y = UserRating,
                  size = UserVotes, shape = Season)) +
  geom_point() +
  geom_smooth(color="red", se = FALSE) 

pl <- ggplot(dat, aes(x = id, y = UserRating,
                  size = UserVotes)) +
  geom_point(aes(color = Season)) +
  geom_smooth(color="red", se = FALSE) 

pl + scale_color_manual(values = c("red4", "red1", "blue4", "blue1", "green4", "green1"))

pl + scale_color_brewer(type="seq", palette = 5)

pl + scale_x_continuous(breaks = seq(5,65,5), 
                        limits = c(-20,100),
                        labels = LETTERS[1:13])

pl + coord_flip()

pl + coord_polar()



ggplot(mySer, aes(x = Season, y = UserRating,
                  color = Season)) +
  geom_boxplot()+
  geom_point(position = position_jitter(width = 0.2, height = 0)) +
  geom_smooth(color="red", se = FALSE) 


ggplot(mySer, aes(x = Season, y = UserRating,
                  color = Season)) +
  geom_boxplot()+
  geom_point(position = position_jitter(width = 0.2, height = 0)) +
  geom_smooth(color="red", se = FALSE) +
  theme_bw() + ggtitle("My plot") +
  theme(title = element_text(size = 20))





ggplot(mySer, aes(x = Season, fill = UserRating > 8.5)) +
  geom_col()

ggplot(mySer, aes(x = Season, fill = UserRating > 8.5)) +
  geom_bar(position = "fill")

ggplot(mySer, aes(x = Season, fill = UserRating > 8.5)) +
  geom_bar(position = "dodge")


 pl <- ggplot(mySer, aes(x = id, y = UserRating, 
                  size = UserVotes, color = Season)) +
  geom_point() +
  geom_smooth(aes(group = Season), method = "lm", 
              se = F, color="red")

 pl + theme_classic()

ggplot(mySer, aes(x = id, y = UserRating, 
                  size = UserVotes)) +
  geom_point(aes(color = Season)) +
  geom_smooth(method = "lm", se = F) +
  scale_size_continuous(range=c(1,10))



ggplot(dat, aes(x = id, y = UserRating, 
                  size = UserVotes)) +
  geom_point(aes(color = Season)) +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~Serie, scales = "free_x")





ggplot(mySer, aes(x = id, y = UserRating)) +
  geom_point(aes(shape = UserVotes), size=10)
ggplot(mySer, aes(x = id, y = UserRating)) +
  geom_point(aes(shape = Season), size=10)

ggplot(mySer, aes(x = id, y = UserRating)) +
  geom_point(size=10)

pl <- ggplot(mySer, aes(x = id, y = UserRating)) +
  geom_point(size=10) +
  geom_smooth(se=FALSE) +
  geom_smooth(method = "lm", se=FALSE, color="red") 
  
ggplot(mySer, aes(x = id, y = UserRating)) +
  geom_point(size=10) + 
  geom_boxplot()

ggplot(mySer, aes(x = id, y = UserRating, color = Season)) +
  geom_boxplot() +
  geom_smooth(se=F) +
  geom_point(size=2, alpha = 0.2) +
  theme_bw()
  

ggplot(dat, aes(x = id, y = UserRating, color = Season)) +
  geom_boxplot() +
  geom_smooth(se=F) +
  geom_point(size=2, alpha = 0.2) +
  coord_



pl + ggtitle("My new title")

ggplot(mySer, aes(x = id, 
                      y = UserRating,
                      size=UserVotes)) +
  geom_point(aes(color = Season)) +
  geom_smooth() +
  geom_smooth(aes(color = Season)) +
  theme_bw()
   
ggplot(mySer, aes(x = Season, 
                      fill = UserRating > 8.5)) +
  geom_bar(position = "dodge")


ggplot(mySer, aes(x = Season, 
                      y = UserRating)) +
  geom_point(position = "jitter") 

