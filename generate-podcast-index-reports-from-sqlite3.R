#!/usr/bin/env Rscript
# If missing then install pacman
if (!require("pacman")) install.packages("pacman")
# install and load everything else with pacman
# Dependancies ubuntu:
# $ sudo apt install libxml2-dev 
pacman::p_load(
	DBI,
	tidyverse,
	data.table,
	dplyr,
	lubridate,
	stringr,
	loggit,
	openssl,
	reshape,
	rlang
)
# RSQLite only needs to be installed, not loaded...
if (!require("RSQLite")) install.packages("RSQLite")
if (dbCanConnect(RSQLite::SQLite(),"./data_sets/podcastindex_feeds.db")){
	# built reporting folder structure
	if(!dir.exists("./explore")) {dir.create("./explore")}
	if(!dir.exists("./report")) {dir.create("./report")}
	if(!dir.exists("./report/jpg")) {dir.create("./report/jpg")}
	if(!dir.exists("./report/png")) {dir.create("./report/png")}
	if(!dir.exists("./report/jpg/histogram")) {dir.create("./report/jpg/histogram")}
	if(!dir.exists("./report/png/histogram")) {dir.create("./report/png/histogram")}
	if(!dir.exists("./report/jpg/boxplot")) {dir.create("./report/jpg/boxplot")}
	if(!dir.exists("./report/png/boxplot")) {dir.create("./report/png/boxplot")}
	
	dbPodcast <- dbConnect(RSQLite::SQLite(),"./data_sets/podcastindex_feeds.db")
	dfPodcast <- DBI::dbReadTable(dbPodcast,"podcasts")
	# column headers are: paste(names(dfPodcast),collapse=",")
	# id,url,title,lastUpdate,link,lastHttpStatus,dead,contentType,itunesId,originalUrl,
	# itunesAuthor,itunesOwnerName,explicit,imageUrl,itunesType,generator,newestItemPubdate,
	# language,oldestItemPubdate,episodeCount,popularityScore,createdOn,updateFrequency,
	# chash,host,newestEnclosureUrl
	dtPodcast_length_mask <- data.table(apply(X=dfPodcast,MARGIN=2,FUN=str_length))
	dtPodcast_length_mask_means <- colMeans(dtPodcast_length_mask)
	dtPodcast_length_mask_medians <- apply(dtPodcast_length_mask, 2, median)
	dtPodcast_length_mask_min <- apply(dtPodcast_length_mask, 2, min)
	dtPodcast_length_mask_max <- apply(dtPodcast_length_mask, 2, max)
	dtPodcast_length_mask_sd <- apply(dtPodcast_length_mask, 2, sd)
	#Inter-quartile-range IQR(x) = quantile(x, 3/4) - quantile(x, 1/4)
	dtPodcast_length_mask_iqr <- apply(dtPodcast_length_mask, 2, IQR)
	dtPodcast_length_mask_summary <- summary(dtPodcast_length_mask)
	dtPodcast_length_mask_count <- apply(dtPodcast_length_mask, 2, length)

	.interPercent_of_column <- function(dataVals,columnName,excludeMinPercent=.1,excludeMaxPercent=.1){
	# Returns the named column 
		dataWindow <- dataVals %>% select(all_of(columnName))
                if (excludeMaxPercent>0) {
			percentUpper <- sort(
					dataWindow[,get(columnName)]
				)[((100-excludeMaxPercent)/100)*length(
					dataWindow[,get(columnName)]
				)]

		} else {
			percentUpper <- max(dataWindow[,get(columnName)])
		}
                if(excludeMinPercent>0){
			percentLower <-
				sort(
					dataWindow[,get(columnName)]
				)[((excludeMinPercent)/100)*length(
					dataWindow[,get(columnName)]
				)]
		} else {
			percentLower <- min(dataWindow[,get(columnName)])
		}
		dataWindow %>%
			filter(eval(parse(text=columnName)) <= percentUpper) %>%
			filter(eval(parse(text=columnName)) >= percentLower)
	}	
	.hist_of_column <- function(dataVals, columnName){
		hist(.interPercent_of_column(dataVals,columnName,excludeMinPercent=0)[,get(columnName)],
			breaks=100,
			main=paste0("Histogram for length of ", columnName, " in podcastindex-org"),
			xlab=paste0("Charater length for field: ",columnName," (excluding top %0.1)")
		)
	}
	.boxplot_of_column <- function(dataVals, columnName){
		boxplot(.interPercent_of_column(dataVals,columnName,excludeMinPercent=0)[,get(columnName)],
			breaks=100,
			xlab=paste0("Charater length for field: ",columnName," (excluding top %0.1)"),
			main=paste0("Histogram for length of ", columnName, " in podcastindex-org")
		)
	}
	# only build histo and box plots when IQR > 2
	dtPodcast_length_mask_plot_columns <- names(
		dtPodcast_length_mask_iqr %>% 
			Filter(f=function(x) x > 2 )
	)
	# generate refined graphs for report
	pdf("./report/Summary graphs.pdf")
	data_to_chart <- dtPodcast_length_mask %>% 
		select(all_of(dtPodcast_length_mask_plot_columns))
	# Using a for loop to write out histo charts, and box plots instead of re-writing functions for apply...
	for (plotColumnName in dtPodcast_length_mask_plot_columns) {
		.hist_of_column(data_to_chart,plotColumnName)
	}
	for (plotColumnName in dtPodcast_length_mask_plot_columns) {
		.boxplot_of_column(data_to_chart,plotColumnName)
	}
	dev.off()

	#write out to an image, we need a new file path for each report
	.hist_of_column2png <- function(data,columnName){
	        png(paste0("./report/png/histogram/",columnName,".png"))
                .hist_of_column(data,columnName)
		dev.off()
	}
	.hist_of_column2jpg <- function(data,columnName){
	        jpeg(paste0("./report/jpg/histogram/",columnName,".jpg"))
                .hist_of_column(data,columnName)
		dev.off()
	}
	.boxplot_of_column2png <- function(data,columnName){
	        png(paste0("./report/png/boxplot/",columnName,".png"))
                .boxplot_of_column(data,columnName)
		dev.off()
	}
	.boxplot_of_column2jpg <- function(data,columnName){
	        jpeg(paste0("./report/jpg/boxplot/",columnName,".jpg"))
                .boxplot_of_column(data,columnName)
		dev.off()
	}
        # Using a for loop to write out histo charts, and box plot images instead of re-writing functions for apply...
        for (plotColumnName in dtPodcast_length_mask_plot_columns) {
		.hist_of_column2png(data_to_chart,plotColumnName)
		.hist_of_column2jpg(data_to_chart,plotColumnName)
		.boxplot_of_column2png(data_to_chart,plotColumnName)
		.boxplot_of_column2jpg(data_to_chart,plotColumnName)
        }

} else {
	message('Failed to connect with: RSQLite::SQLite(),"./data_sets/podcastindex_feeds.db')
}
