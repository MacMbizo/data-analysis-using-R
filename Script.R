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
