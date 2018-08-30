###Joining occurrences scripts )GBIF + 1001genomes
#!USE CSV FILES 
inputDir <- "Z:/CLIMATE_CHANGE"
occDir <- paste0(inputDir,"/","occurrences"); if(!file.exists(occDir)){dir.create(occDirs)}

#require(maptools);data(wrld_simpl)

occurrences <- as.data.frame(read.csv(paste0(occDir,"/","occurrence.txt"),sep="\t"))
occurrences <- occurrences[which(!is.na(occurrences$decimalLatitude)),]
occurrences <- as.data.frame(cbind(occurrences$gbifID,occurrences$decimalLongitude,occurrences$decimalLatitude))
occurrences$Source <- NA; occurrences$Source <- rep("GBIF",nrow(occurrences))

genomes_1000_csv <- as.data.frame(read.csv(paste0(occDir,"/","1001Genomes.csv"),sep="|"))
genomes_1000_csv <- as.data.frame(cbind(genomes_1000_csv$CS_number,genomes_1000_csv$longitude,genomes_1000_csv$latitude))
genomes_1000_csv$Source <- NA; genomes_1000_csv$Source <- rep("genomes_1001",nrow(genomes_1000_csv))

final_file <- rbind(genomes_1000_csv, occurrences)
colnames(final_file) <- c("id","longitude","latitude","source")

write.csv(final_file,paste0(occDir,"/","final_occurrences.csv"),quote = F,row.names=F)

# plot(occurrences$decimalLongitude,occurrences$decimalLatitude)
# plot(wrld_simpl,add=T)

