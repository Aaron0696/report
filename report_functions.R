# global variables ---------------------------------------------------
# these vectors are used to print out the diagnostics list at the end of the report
# vector to store the names of variables with non-numeric responses when they are supposed to be numeric
nnlist <- c()
# vector to store the names of variables that are highly skewed
skews <- c()

# function to create descriptive statistics table for continuous variables
# input: a vector of values
# output: a list containing a table of summary statistics and any non-numeric elements detected in the vector
destable.cts <- function(colname, data)
{
  # extract the data as a temporary vector
  myvec <- data[[colname]]
  # isolate the non-numeric responses
  nonnum <- myvec[is.na(as.numeric(myvec))]
  # if there are no non-numeric response, return a signal that all responses are numeric
  if(length(nonnum) == 0)
  {
    nonnum <- NA
  } else
  {
    # i is from the main loop in the .Rmd file
    # is a string representing the variable name
    nnlist <<- c(nnlist, colname)
  }
  # get the descriptive summary, round it to 2dp
  temp <- myvec %>% as.numeric(myvec) %>% describe() %>% round(2)
  # change titles to upper case
  names(temp) <- str_to_upper(names(temp))
  # remove row names
  rownames(temp) <- NULL
  # remove certain columns and create new column NAs that indicates the number of NA valuesin the vector
  # NA values include strings as coerced by as.numeric()
  temp <- temp %>% 
    select(-VARS, -MAD, -TRIMMED, -SE) %>%
    # NA is anything that is non-numeric
    mutate(NACount = sum(is.na(myvec)))
  # detect any variable that has absolute skew > 1
  if(abs(temp$SKEW) > 1)
  {
    # i is from the loop
    skews <<- c(skews, colname)
  }
  # make descriptive statistics into a table
  mytable <- kable(temp, align = "c")
  # save output as a list
  out <- list(table = mytable,
              nonnum = nonnum)
  return(out)
}

# function to create a frequency table
# input: a vector of values
# output: a table of frequency information
freqtab <- function(colname, data)
{
  # extract the data as a temporary vector
  myvec <- data[[colname]]
  # generate frequency counts
  counts <- table(myvec, useNA = "always") %>% data.frame()
  names(counts) <- c("Detected Categories","Frequency")
  # generate relative proportion
  counts[["Relative Proportions (%)"]] <- round((counts$Frequency/sum(counts$Frequency)) * 100, 2)
  
  # add row for total
  # change to character from factor so a new string can be added into the dataframe
  counts$`Detected Categories` <- as.character(counts$`Detected Categories`)
  counts[nrow(counts) + 1,] <- c("Total", sum(counts$Frequency), sum(counts$`Relative Proportions (%)`))
  
  # make a table
  mytable <- kable(counts, align = "c")
  return(mytable)
}

# function to create a histogram
# input: a dataframe and the column name of the variable in the dataframe
# output: a histogram
histo <- function(colname, data)
{
  # extract the data as a temporary dataframe
  temp <- data.frame(var = as.factor(data[[colname]]))
  # histogram
  plot(
    ggplot(data = temp, aes_string(x = "var")) +
      geom_histogram(stat = "count", fill = "violetred") + ylab("Count") +
      theme_classic() + 
      theme(axis.text = element_text(angle = 0, size = 20),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20),
            legend.position = "none")
  )
}

# function to create a density plot
# input: a dataframe and the column name of the variable in the dataframe
# output: a density plot
dens <- function(colname, data)
{
  # extract the data as a temporary dataframe
  temp <- data.frame(var = as.numeric(data[[colname]]))
  
  plot(
    ggplot(data = temp, aes_string(x = "var")) +
      geom_density(size = 0.9, color = "violetred", alpha = 0.7, lty = "dashed") + ggtitle("Density") +
      theme_classic() + 
      theme(axis.text = element_text(size = 18),
            axis.title = element_blank(), 
            title = element_text(size = 20),
            legend.position = "none")
  )
}

# function to create a boxplot
# input: a dataframe and the column name of the variable in the dataframe
# output: a boxplot
boxp <- function(colname, data)
{
  # extract the data as a temporary dataframe
  temp <- data.frame(var = as.numeric(data[[colname]]))
  
  plot(
    ggplot(data = temp, aes_string(x = "var")) +
      geom_boxplot(varwidth = T) + ggtitle("Boxplot") +
      theme_classic() + 
      ylim(c(-2,2)) +
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            title = element_text(size = 20),
            legend.position = "none")
  )
}