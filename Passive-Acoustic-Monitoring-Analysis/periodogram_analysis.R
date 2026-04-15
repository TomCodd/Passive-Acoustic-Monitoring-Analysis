import_files <- list.files(here("data", "species_call_data"))

for(file in import_files){
  species <- chartr("_", " ", gsub("_call_dataset.csv", "", x = file))
  
  
  df <- read.csv(here("data", "species_call_data", file), row.names = NULL)
  daytime_df <- df[360:1200, 2:ncol(df)]
  daytime_df <- daytime_df[ , colSums(is.na(daytime_df))<660] # Means at least 3 hours of recordings


  # plot(1,1, type = 'l', xlim = c(0.0167, 0.5), ylim = c(1, 1000), lwd = 2, xlab = 'frequency', ylab = 'power', main = paste0(species, " revisit Power Spectrum"))

  col_list <- colnames(daytime_df)

  for(i in 2:length(col_list)){
    daytime_df_subset <- na.omit(daytime_df[,c(1, i)])
    column_name <- gsub("X", "", colnames(daytime_df_subset[2]))

    x <- daytime_df_subset[,1]
    y <- daytime_df_subset[,2]

    if(sum(y)==0){
      next
    }

    # All of this stuff is the same as what the pgram does, the results are the same. 
    dx <- ((max(x)-min(x))/length(x))
    xout <- (min(x)+((0:length(x))*dx))

    y.new <- approx(x,y,xout)
    #results <- spec.pgram(daytime_df, plot = FALSE)
    results_2 <- spec.pgram(ts(y.new$y, deltat = dx), demean = TRUE, detrend = TRUE, plot = TRUE, spans = 10, main = paste0(species, " revisit periodogram"))
    #lines(results$freq, results$spec[,2], lwd = 2)
    #plot(1,1, type = 'l', xlim = c(0.0167, 0.5), ylim = c(1, 1000), lwd = 2, xlab = 'frequency', ylab = 'power')

    # lines(results_2$freq, results_2$spec, lwd = 1)
  }
}

European_goldfinch_df <- read.csv(here("data", "species_call_data", "European_Goldfinch_call_dataset.csv"))


European_goldfinch_df_daylight_March <- European_goldfinch_df[360:1200, 2:3]
European_goldfinch_df_daylight_April <- European_goldfinch_df[360:1200, c(2,5)]


fft_result <- fft(European_goldfinch_df_daylight_March[,2])
plot(Mod(fft_result), main = "European Goldfinch revisit FFT plot")

fft_result_April <- fft(European_goldfinch_df_daylight_April[,2])
plot(Mod(fft_result_April), main = "European Goldfinch revisit FFT plot (April)")



# results_3 <- spec.pgram(European_goldfinch_df_daylight[,c(1,2)], spans = 10, demean = TRUE, detrend = TRUE, main = "European Goldfinch revisit periodogram")

acf(European_goldfinch_df_daylight_March, demean = TRUE, main = "European Goldfinch revisit Autocorrelation function")
acf(European_goldfinch_df_daylight_April, demean = TRUE, main = "European Goldfinch revisit Autocorrelation function")