library("here")

import_files <- list.files(here("data", "import"))

Species_covered <- c()

for(file in import_files){
  detections <- read.csv(here("data", "import", file))

  # Filtering out low-confidence identifications
  detections <- detections[detections$Confidence > 0.55, ]

  #Next to identify the species with enough calls to do the analysis - technically 3 would do
  bird_frequency <- table(detections$Common.name)
  Species_to_analyse <- unlist(labels(bird_frequency[bird_frequency>2]))
  filtered_detections <- detections[detections$Common.name %in% Species_to_analyse,]

  #Converting to strptime with minute bins
  filtered_detections$Detection.start_strptime <- strptime(filtered_detections$Detection.start, "%Y-%m-%d %H:%M:%OS") #Converts to strptime
  filtered_detections$Detection.start_strptime <- cut(filtered_detections$Detection.start_strptime, breaks = "min")  #cut converts it to minutes

  #Creating the basic df to attach results to
  timestamps <- unique(unlist(labels(table(filtered_detections$Detection.start_strptime))))
  frequency_table <- data.frame("Timestamps" = as.POSIXct(timestamps, tz = "GMT", "%Y-%m-%d %H:%M:%OS"))

  # Next, to run a count for each species for each timestamp.
  for(Species in Species_to_analyse){
    species_table <- table(filtered_detections[filtered_detections$Common.name == Species,]$Detection.start_strptime)
    frequency_table <- cbind(frequency_table, species_table)
    colnames(frequency_table)[colnames(frequency_table) == 'Freq'] <- Species
    frequency_table <- subset(frequency_table, select = -Var1)
  }

  colnames(frequency_table) <- chartr(" ", "_", colnames(frequency_table))

  # Fix midnight issue 

  NA_timestamps <- which(is.na(frequency_table$Timestamps))

  for(x in NA_timestamps){
    frequency_table[x, "Timestamps"] <- frequency_table[x+1, "Timestamps"] - 60
  }

  rm(NA_timestamps)

  days_covered <- unique(format(frequency_table$Timestamps, "%d/%m/%Y"))

  Daily_dataset <- data.frame("Day_Minutes" = seq(0, 1439, 1)) #Creates a base dataset to merge data to

  for(day in days_covered){
    day_data <- frequency_table[format(frequency_table$Timestamps, "%d/%m/%Y") == day,] #subsets the data to a particular day

    Day_call_totals <- data.frame(t(colSums(day_data[,2:ncol(day_data)]))) # Calculates the total calls for that day
    Selected_species <- colnames(Day_call_totals[, Day_call_totals > 100]) # Filters to only birds with over 100 calls in the day
    Selected_species <- chartr(".", "_", Selected_species) # Replaces the full stops with underscores in the names

    Species_covered <- unique(c(Species_covered, Selected_species))

    for(Species in Species_covered){
      if(exists(paste0(Species, "_call_dataset")) == FALSE){ # Checks to see if the species df is present
        if(file.exists(here("data", "species_call_data", paste0(Species, "_call_dataset.csv"))) == TRUE){ # If the df isn't present, it checks if it exists
          eval(parse(text = paste0(Species, "_call_dataset <- read.csv(here(\"data\", \"species_call_data\", paste0(Species, \"_call_dataset.csv\")))"))) # If it does exist, it imports it
        } else {
          # If the file doesn't exist, this creates a new df
          eval(parse(text = paste0(Species, "_call_dataset <- Daily_dataset")))
        }
      }
      # Once done, it iteratively adds to the df.
      Species_subset <- day_data[, c("Timestamps", Species)] 

      Species_subset$Day_Minutes <- (as.numeric(format(Species_subset$Timestamps, "%H"))*60)+(as.numeric(format(Species_subset$Timestamps, "%M")))
      Species_subset <- Species_subset[, c("Day_Minutes", Species)]

      # And must rename the column from species (which is denoted by the file) to the relevant date
      colnames(Species_subset) <- c("Day_Minutes", day)

      # And then making sure it has the empty rows too, if no data is present
      merge_species_subset <- merge(Daily_dataset, Species_subset, by = "Day_Minutes", all.x = TRUE)

      eval(parse(text = paste0(Species, "_call_dataset <- merge(", Species, "_call_dataset, Species_subset, by = \"Day_Minutes\", all.x = TRUE)")))

      # And saves the dataset if the last day has been added, and the last file has been completed
      if(file == import_files[length(import_files)]){
        if(day == days_covered[length(days_covered)]){
          for(saving_species in Species_covered){
            eval(parse(text = paste0("write.csv(", saving_species, "_call_dataset, here(\"data\", \"species_call_data\", paste0(saving_species, \"_call_dataset.csv\")))")))
          }
        }
      }
    }
  }

  # And then move the file thats just been processed to the processed folder
  #file.rename(here("data", "import", file), here("data", "processed", file))
}
