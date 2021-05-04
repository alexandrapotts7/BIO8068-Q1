# Question 1 for BIO8068 Module ----
# Bird Song Acoustic 

# Installing packages needed for the analysis ----
library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(warbleR)
library(leaflet)
library(stringr)

# using nes8010.R for the PCA analysis later on

library(vegan)
source("nes8010.R") 

# checking the birdsong initially from the xeno canto database ----
# limit to the UK and also length of clip from 5 - 25 seconds

barn_owl_call <- query_xc(qword = 'Tyto alba cnt:"united kingdom" type:call len:5-25', download = FALSE)

# barn owl has 66 recordings

robin_call <- query_xc(qword = 'Erithacus rubecula cnt:"united kingdom" type:call len:5-25', download = FALSE)

# better with 47 calls

herring_gull_call <- query_xc(qword = 'Larus argentatus cnt:"united kingdom" type:call len:5-25', download = FALSE)

# 49 recordings found

# mapping the calls 
map_xc(barn_owl_call, leaflet.map = TRUE)
map_xc(robin_call, leaflet.map = TRUE)
map_xc(herring_gull_call, leaflet.map = TRUE)

# now can download for analysis ----
# conversion is also needed from MP3 files to wav files

# Create subfolders in your RStudio Project for each of the bird species ----
dir.create(file.path("barn_owl_calls"))
dir.create(file.path("robin_calls"))
dir.create(file.path("herring_gull_calls"))

# Download the .MP3 files into three separate sub-folders ----
query_xc(X = barn_owl_call, path="barn_owl_calls")
query_xc(X = robin_call, path="robin_calls")
query_xc(X = herring_gull_call, path="herring_gull_calls")

# now time to tidy up the files and rename for each of the three species ----

# part of tidyverse
library(stringr) 

# barn owl calls
old_files <- list.files("barn_owl_calls", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-calls_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# robin calls
old_files <- list.files("robin_calls", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-calls_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# herring gull calls
old_files <- list.files("herring_gull_calls", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-calls_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# creating new sub folders so R doesnt get confused with the folders

# barn owls
dir.create(file.path("barn_owl_audio"))
file.copy(from=paste0("barn_owl_calls/",list.files("barn_owl_calls")),
          to="barn_owl_audio")

# robin 
dir.create(file.path("robin_audio"))
file.copy(from=paste0("robin_calls/",list.files("robin_calls")),
          to="robin_audio")

# herring gulls
dir.create(file.path("herring_gull_audio"))
file.copy(from=paste0("herring_gull_calls/",list.files("herring_gull_calls")),
          to="herring_gull_audio")


# need to change from mp3 files to wav ----

# barn owls
mp32wav(path="barn_owl_audio", dest.path="barn_owl_audio")
unwanted_mp3 <- dir(path="barn_owl_audio", pattern="*.mp3")
file.remove(paste0("barn_owl_audio/", unwanted_mp3))

# robin
mp32wav(path="robin_audio", dest.path="robin_audio")
unwanted_mp3 <- dir(path="robin_audio", pattern="*.mp3")
file.remove(paste0("robin_audio/", unwanted_mp3))

# herring gull
mp32wav(path="herring_gull_audio", dest.path="herring_gull_audio")
unwanted_mp3 <- dir(path="herring_gull_audio", pattern="*.mp3")
file.remove(paste0("herring_gull_audio/", unwanted_mp3))

# visualising and analysing each bird call ----

# barn owl calls visualised ----

barn_owl_wav <- readWave("barn_owl_audio/Tytoalba-calls_25594.wav")
barn_owl_wav

# plotting
oscillo(barn_owl_wav)

# zooming in
oscillo(barn_owl_wav, from = 0.2, to = 0.8)


# robin calls visualised ----

robin_wav <- readWave("robin_audio/Erithacusrubecula-calls_148706.wav")
robin_wav

# plotting
oscillo(robin_wav)

# zooming in
oscillo(robin_wav, from = 0.2, to = 0.8)


# herring gull visualised ----

herring_gull_wav <- readWave("herring_gull_audio/Larusargentatus-calls_122446.wav")
herring_gull_wav

# plotting
oscillo(herring_gull_wav)

# zooming in
oscillo(herring_gull_wav, from = 0.2, to = 0.8)


# creating spectrogram for barn owl
SpectrogramSingle(sound.file = "barn_owl_audio/Tytoalba-calls_25594.wav", min.freq = 1000, 
                  max.freq = 5000, Colors = "Colors") 

# gg plot version
cu <- ggspectro(barn_owl_wav, flim=c(1,5.5)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)

plot(cu)

# creating spectrogram for robin
SpectrogramSingle(sound.file = "robin_audio/Erithacusrubecula-calls_148706.wav", min.freq = 1000, 
                  max.freq = 5000, Colors = "Colors") 

# creating spectrogram for herring gull
SpectrogramSingle(sound.file = "herring_gull_audio/Larusargentatus-calls_122446.wav", min.freq = 1000, 
                  max.freq = 5000, Colors = "Colors") 


# MFCC ----

# create one folder called bird audio for the next section

dir.create(file.path("bird_audio"))
file.copy(from=paste0("barn_owl_audio/",list.files("barn_owl_audio")),
          to="bird_audio") %>%  
  file.copy(from=paste0("robin_audio/",list.files("robin_audio")),
            to="bird_audio") %>% 
  file.copy(from=paste0("herring_gull_audio/",list.files("herring_gull_audio")),
            to="bird_audio")

bird_mfcc <- MFCCFunction(input.dir = "bird_audio",
                          max.freq=7000)


dim(bird_mfcc)

# now time for the PCA analysis ----
bird_pca <- ordi_pca(bird_mfcc[, -1], scale=TRUE)
summary(bird_pca)


# plotting the PCA ----
bird_sco <- ordi_scores(bird_pca, display="sites")
bird_sco <- mutate(bird_sco, group_code = bird_mfcc$Class)

ggplot(bird_sco, aes(x=PC1, y=PC2, colour=group_code)) + geom_chull(alpha=0.5) +
  scale_color_discrete(name = "Species Calls",
                       labels = c("Robin", "Herring Gull", "Barn Owl")) +
  geom_point() 
