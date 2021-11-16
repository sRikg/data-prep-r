#' Generate stratified train, test and validation datasets given a dataset, a column and a label
#'
#' This function takes in a dataset(data), a column in the dataset(col_to_stratify) and the labels in the colum(label)
#' It would also take in the percent of the data to be created as a training data(train_percent)
#' A seed could be provided. If not, the number of rows is considered the seed
#'
#' @param (data, col_to_stratify, label, seed = c(), train_percent = 0.8)
#' @return A list with train, test and validation datasets for the given lable
#' @export

require(stats)

random_rows_selector <- function(data, col_to_stratify, label, seed = c(), train_percent = 0.8){
	if ((train_percent <= 0) || (train_percent >= 1)) {
		print("ERROR: Train and Test percentages is infeasible")
	} else {
		if(!col_to_stratify %in% names(data)) {
			print("ERROR: col_to_stratify is not in the provided data")
		} else {
			data1 <- data[data[,col_to_stratify] == label, ]
			n_rows <- nrow(data1)

			if(length(seed)) set.seed(seed) else set.seed(n_rows)
			train_idxs <- unique(round(rnorm(
				n_rows,
				mean = n_rows/2,
				sd = n_rows
			)))

			if(sum(train_idxs > n_rows) > 1 || sum(train_idxs < 0) > 1){
				train_idxs <- train_idxs[
					train_idxs > 0 & train_idxs <= n_rows
				]
			}

			if(length(train_idxs) < n_rows*train_percent){
				n_missing_idxs <- round(n_rows*train_percent) - length(train_idxs)
				set.seed(n_rows+1)
				missing_idxs <- unique(round(rnorm(
					n_missing_idxs*10,
					mean = n_rows/2,
					sd = n_rows
				)))
				train_idxs <- unique(c(train_idxs, missing_idxs[missing_idxs > 0 & missing_idxs <= n_rows]))
			}

			if(length(train_idxs) > n_rows*train_percent) train_idxs <- train_idxs[1:round(n_rows*train_percent)]

			non_train_idxs <- (c(1:n_rows))[-train_idxs]
			test_idxs <- non_train_idxs[1:round(length(non_train_idxs)/2)]
			validation_idxs <- non_train_idxs[(round(length(non_train_idxs)/2)+1) : length(non_train_idxs)]

			output <- list(data1[train_idxs,], data1[test_idxs,], data1[validation_idxs,])

			return(output)
		}
	}
}
