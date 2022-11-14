library(dplyr)

# Adding spatial lag using distance in km between locations (calculated from coordinates)

protest.data <- read.csv("./data/final_weekly_data.csv")
dist.matrix <- read.csv("./data/dist_matrix_geoid.csv")

# Column and row names into correct format
geoID <- dist.matrix[,1]
	dist.matrix <- as.matrix(dist.matrix[,-1])
		rownames(dist.matrix) <- colnames(dist.matrix) <- as.character(geoID)
      dist.matrix <- dist.matrix[order(rownames(dist.matrix)),order(colnames(dist.matrix))]

protest.data$GEOID10_x <- as.character(protest.data$GEOID10_x)
		protest.data <- arrange(protest.data,GEOID10_x,Year.Week)
			protest.data$indicator <- protest.data$Protest.


weeks <- unique(protest.data$Year.Week)

output <- data.frame(matrix(NA,nrow=1,ncol=3)) # empty dataframe to be filled with protest data using spatial lag
	names(output) <- c("GEOID10_x","sp_protest","Year.Week")

#Adding spatial lag and populating dataframe (distance matrix * protest occurrence)
for(i in 1:length(weeks)){
temp.week <- weeks[i]
	temp.data <- protest.data[protest.data$Year.Week==temp.week,]
		temp.ids <- intersect(temp.data$GEOID10_x,rownames(dist.matrix))
			if(length(temp.ids)>1){
				temp.matrix <- dist.matrix[temp.ids,temp.ids]
					temp.value <- temp.data$indicator[which(temp.data$GEOID10_x %in% temp.ids)]
						temp.spatial <- temp.matrix %*% temp.value   # matrix multiplication
							temp.out <- data.frame(temp.ids,temp.spatial,temp.week)
								names(temp.out) <- c("GEOID10_x","sp_protest","Year.Week")
									output <- rbind(output,temp.out)
					}
	}
output <- output[-1,]

# Joining spatial lag data to protest data 
protest.data <- left_join(protest.data,output,by=c("GEOID10_x"="GEOID10_x","Year.Week"="Year.Week"))

write.csv(protest.data,file="./data/weekly_data_lagged.csv")
