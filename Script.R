tuesdata <- tidytuesdayR::tt_load('2021-09-14')
tuesdata <- tidytuesdayR::tt_load(2021, week = 38)

billboard <- tuesdata$billboard

beats <- tuesdata$audio_features
View(billboard)
View(beats)

# Are songs ranked to be number one generally more cheerful?

billboard_1  <- billboard %>% 
  mutate(no1 =  (peak_position  == 1),
         week_id = mdy(week_id)) %>% #  change week_id data type to "date"
  group_by(song_id) %>% 
  arrange(week_position, .by_group = TRUE) %>% 
  filter(!duplicated(song_id)) %>% 
  ungroup() %>% 
  select(song_id,no1)

# joining beats data and joining it to billboard_1
tracks <- billboard_1 %>% 
  left_join(beats, by = "song_id")  

#remove "N/A" from data set
mean(is.na(tracks$valence)) # checking % of missing values in valence column

map_dbl(tracks, \(x) mean(is.na(x))) # checking % of missing values in all columns

ggplot(tracks, aes(x = valence,
                   y = no1)) + 
  geom_boxplot()
  theme_minimal()
  
t.test(valence ~ no1,
       data = tracks)

# outcome:
#Welch Two Sample t-test

#data:  valence by no1
#t = -2.5422, df = 1092.6, p-value = 0.01115
#alternative hypothesis: true difference in means between group FALSE and group TRUE is not equal to 0
#95 percent confidence interval:
#  -0.03577254 -0.00460695
#sample estimates:
#  mean in group FALSE  mean in group TRUE 
#0.6009221           0.6211119 

#No1 songs are more valent but the different is small.