# Useful snippets of code

# git remote add origin https://github.com/rlavelli/RUtility.git

# git config remote.origin.url git@github.com:rlavelli/RUtility.git
# git pull -u origin master
# git push -u origin master

possible_my_function <- possibly(my_function, otherwise = NA_real_)
numbers_with_error <- list(1, 2, 3, "spam", 4) # wrong data type

map(numbers_with_error, possible_my_function) # executes with NA




