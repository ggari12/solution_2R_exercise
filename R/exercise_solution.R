################################################################################
## Getting started with R
# SOLUTION: Exercise 1
################################################################################

library(tidyverse)

# 1. Import the iris dataset into a variable named df_iris.

write_csv(iris, file = "outputs/df_iris.csv")

# Read the imported df_iris data & assigning it to "df_iris_data" and use the pipe operator to add steps 2 to 5.

df_iris_data <- read_csv("inputs/df_iris.csv", 
                                        show_col_types = FALSE) |>  # read in the data stored in the inputs folder of the project.
    select(starts_with("Petal"), "Species") |>                      # 2. Subset the dataset to keep with columns that start with Petal and Species
    rename_with(~sub("^Petal.", "", .x), starts_with("Petal.")) |>  # 3. Rename columns that start with Petal by removing Petal. from the columns. 
                                                                    #    Maintaining only Length and Width
    filter(Length > 1.2) |>                                         # 4. Subset dataset to keep only rows where Length is greater than 1.2
    mutate(Length_Category =                                        # 5. Create a new column of Length_Category with ranges < 3 “less_than_3”,
               case_when(Length < 3 ~ "less_than_3",                #    <= 5 “btn_3_and_5”, > 5 “greater_than_5”
                         Length <= 5 ~ "btn_3_and_5",
                         Length > 5 ~ "greater_than_5"))

# 6. Create a bar graph indicating the Species and colour the graph based on Length_Category.    
# geom_bar color by category:

df_iris_data |> 
ggplot(aes(x = Species)) +                      # x or y column provided
    geom_bar(aes(fill = Length_Category)) +     # uses stat_count() by default
    theme_bw()

# 7. Calculate the mean Length for each Species and store the result in the df_mean_length variable.

df_mean_length <- df_iris_data |> 
    group_by(Species) |> 
    summarise(`Mean Length`  = mean(Length))

# 8. Create a bar graph for the result from the previous step.
# customizing the graph:

ggplot(df_mean_length, aes(x = `Mean Length`, y = Species)) +
    geom_col(fill = "blue") +
    theme_bw()

# customizing the graph:

ggplot(df_mean_length, aes(x = `Mean Length`, y = Species)) +
    geom_col(fill = "blue") +
    theme_bw() +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(face = "bold", size=12),
          axis.text.y = element_text(face = "bold", size=12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          axis.title = element_text(size=12)) +
    ylab(label = "")  

# 9. Calculate the proportions based on Species and Length_Category. Export this result into a csv file.
# calculate the proportions:

df_proportion_length <- df_iris_data |> 
    group_by(Species, Length_Category) |> 
    summarize(count = n()) |> 
    mutate(prop = count/sum(count))

# Write output csv dataset out

write_csv(df_proportion_length, "outputs/proportion_length.csv")

################################################################################
