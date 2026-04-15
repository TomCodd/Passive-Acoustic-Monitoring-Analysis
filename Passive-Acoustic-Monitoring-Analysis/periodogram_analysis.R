library(here)

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
    results_2 <- spec.pgram(ts(y.new$y, deltat = dx), demean = TRUE, detrend = TRUE, plot = TRUE, spans = 10, main = paste0(species, " revisit periodogram (", column_name, ")"))

    acf(y, demean = TRUE, main = paste0(species, " revisit Autocorrelation function (", column_name, ")"))

    fft_result <- fft(y)
    plot(Mod(fft_result), main = paste0(species, " revisit FFT plot (", column_name, ")"))

  }
}
