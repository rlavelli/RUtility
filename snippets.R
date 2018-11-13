# Useful snippets of code

# git remote add origin https://github.com/rlavelli/RUtility.git

# git config remote.origin.url git@github.com:rlavelli/RUtility.git
# git pull -u origin master
# git push -u origin master

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# use of possibly() which helps dealing with errors
possible_my_function <- possibly(my_function, otherwise = NA_real_)
numbers_with_error <- list(1, 2, 3, "spam", 4) # wrong data type

map(numbers_with_error, possible_my_function) # executes with NA

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# nice plot for visualize missing data
missing_data <- df %>% summarise_all(funs(sum(is.na(.))/n())) # use all variables
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()




