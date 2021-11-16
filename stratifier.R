#' Generate stratified train, test and validation datasets given a dataset and a column
#'
#' This function takes in a dataset(data), a column in the dataset(col_to_stratify)
#' It would also take in the percent of the data to be created as a training data(train_percent)
#' A seed could be provided. If not, the number of rows is considered the seed
#'
#' @param (data, col_to_stratify, label, seed = c(), train_percent = 0.8)
#' @return A list with train, test and validation datasets for the given lable
#' @export

require(stats)

stratifier <- function(data, col_to_stratify, seed = c(), train_percent = 0.8) {
	labels <- unique(data[,col_to_stratify])
	data_list <- list()
	freq <- table(data[,col_to_stratify])
	if(sum(freq < round(nrow(mtcars)*0.2)) > 0) {
		print("ERROR: Cannot stratify with the current data and train_percent")
		return(list())
	} else {
		for(i in 1:length(labels)){
			data_list[[i]] <- random_rows_selector(
				data = data, col_to_stratify = col_to_stratify, label = labels[i], seed = c(), train_percent = 0.8
			)
		}
		train_data <- do.call(
			rbind,
			list(
				data_list[[1]][[1]],
				data_list[[2]][[1]]
			)
		)
		test_data <- do.call(
			rbind,
			list(
				data_list[[1]][[2]],
				data_list[[2]][[2]]
			)
		)
		validation_data <- do.call(
			rbind,
			list(
				data_list[[1]][[3]],
				data_list[[2]][[3]]
			)
		)
		if(length(labels) > 2) {
			for(i in 3:length(labels)){
				train_data <- do.call(
					rbind,
					list(
						train_data,
						data_list[[i]][[1]]
					)
				)
				test_data <- do.call(
					rbind,
					list(
						test_data,
						data_list[[i]][[2]]
					)
				)
				validation_data <- do.call(
					rbind,
					list(
						validation_data,
						data_list[[i]][[3]]
					)
				)
			}
		}

		output <- list(train_data, test_data, validation_data)

		return(output)
	}
}
