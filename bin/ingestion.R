downloadFiles <- function(train, test){
        train_file = "data/train.csv"
        test_file = "data/test.csv"
        
        if(!file.exists(train_file))
                download.file(url = train, destfile = train_file)
        if(!file.exists(test_file))
                download.file(url = test, destfile = test_file)
        
        return (c(train_file, test_file))
}

createDataset <- function(file){
        df = read.csv(file)
        return (df)
}

filterVariables <- function(dataset){
        kurtosis_var <- grep("kurtosis*", names(dataset))
        skewness_var <- grep("skewness*", names(dataset))
        max_var <- grep("max_*", names(dataset))
        min_var <- grep("min_*", names(dataset))
        amplitude_var <- grep("amplitude_*", names(dataset))
        variation_var <- grep("var_*", names(dataset))
        avg_var <- grep("avg_*", names(dataset))
        stddev_var <- grep("stddev_*", names(dataset))
        tst_var <- grep("*timestamp*", names(dataset))
        window_var <- grep("*window*", names(dataset))
        other_var <- c(1, 2)
        
        exclude <- c(kurtosis_var, skewness_var, max_var, 
                     min_var, amplitude_var, variation_var,
                     avg_var, stddev_var, tst_var,
                     window_var, other_var)
        
        df <- dataset[, -exclude]
        
        return (df)
}