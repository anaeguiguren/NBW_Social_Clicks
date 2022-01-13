#               Initial Exploration
# Copy the files flagged in field comments with raspberries, 
# patterns, and/or social behaviours. 

install.packages("tuneR")
library(tuneR)

# 1. read in field comments table -----
comm <- read.csv("Raw_Data/NBW_aug_2021_listening_forms.csv", header = T)

#get only flagged comments
flagged_comm<-comm[which(comm$Potential_for_screening=="yes"),]

#fix date time format
flagged_comm$UTC_posix <- as.POSIXct(flagged_comm$UTC,
                                     format = "%Y-%m-%d %H:%M:%S",
                                     tz = "UTC")


# 2. obtain recording file names----
recs<-list.files(path = "/Volumes/NBW_audio/NBW_2021_08_audio_backup_b", recursive = T, full.names = T)

recs<- as.data.frame(recs)

# 3. obtain frecording file timestamps----

# extract only file names (excluding folder names)
recs$wav_names <- sub(".*/", "", recs$recs)

x <- recs$wav_names

date_time_info <- regmatches(x, gregexpr("[[:digit:]]+", recs$wav_names)) 

#create new columns for dates
recs$year   <- NA
recs$month  <- NA
recs$day    <- NA
recs$hour   <- NA
recs$minute <- NA
recs$seconds <- NA


for(i in seq_along(recs$wav_names)){
  recs$year[i]<-substr(date_time_info[[i]][2], 1,4)
  recs$month[i]<-substr(date_time_info[[i]][2], 5,6)
  recs$day[i]<-substr(date_time_info[[i]][2], 7,8)
  
  recs$hour[i]<-substr(date_time_info[[i]][3], 1,2)
  recs$minute[i]<-substr(date_time_info[[i]][3], 3,4)
  recs$seconds[i]<-substr(date_time_info[[i]][3], 5,6)  
}

# convert to a date-time object

recs$Date <- paste(recs$year, recs$month, recs$day, 
                        sep = '-')
recs$Time <- paste(recs$hour, recs$minute, recs$seconds, 
                        sep = ":")

recs$DateTime <- paste(recs$Date, recs$Time, sep = " ")

recs$DateTime <- as.POSIXct(recs$DateTime, 
                                 format = "%Y-%m-%d %H:%M:%S", 
                                 tz = "UTC")

# find file duration
library("tuneR")

recs$duration_min <- NA


for( i in seq_along(recs$duration_min)){
  try({readfile<-recs$recs[i]
  audio<-readWave(readfile, header=TRUE)
  recs$duration_min[i] <- round((audio$samples / audio$sample.rate)/60, 2)})
}

#find end time
recs$DateTime_end<- recs$DateTime + recs$duration_min*60



# 3. match file names to comments -----
# get files within which comments fall

# get interval information
library(lubridate)
rec_intervals        <- as.list(interval(recs$DateTime, recs$DateTime_end))


head(recs$DateTime)
i<-2

flagged_comm$wav_file_name <- NA
flagged_comm$wav_file_path <- NA

for(i in seq_along(flagged_comm$wav_file_name)){
  flagged_comm$wav_file_name[i] <- recs$wav_names[which(recs$DateTime<=flagged_comm$UTC_posix[i]&recs$DateTime_end>=flagged_comm$UTC_posix[i])] 
  flagged_comm$wav_file_path[i] <- recs$recs[which(recs$DateTime<=flagged_comm$UTC_posix[i]&recs$DateTime_end>=flagged_comm$UTC_posix[i])] 
  }




# 4. copy flagged files to computer (temporarily) ------



# 4. create a folder that contains only these wav files ------

#  place new folder path and name here

folder <- "/Users/anacristinaeguigurenburneo/Desktop/NBW_raspberry_files"



dir.create(folder)#creates a folder where all files will be stored


file.copy(flagged_comm$wav_file_path, folder)
beep(8)

# *manually transfer them to Hard Drive, since I don't have enough USB 
# adapters


# 5. save table with observations-----

write.csv(flagged_comm, "Processed_Data/Raspberries_and_patterns_screening.csv")









