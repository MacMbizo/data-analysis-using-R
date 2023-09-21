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
  filter(!duplicated(song_id))

