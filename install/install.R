# 26/9/2025

# After updating to R 4.5.1 
# install.packages("https://github.com/paulnorthrop/stat0002/raw/master/install/stat0002.zip", 
#                  repos = NULL)
# Does not work any more
# I don't know why
# If I donwload the .zip file and install locally then it works, but only if I
# leave out the repos = NULL part, which is inferred within the function

# Therefore, I will use the workaround of 
# 1. downloading the file first
# 2. then installing locally

# Windows

download.file(url = "https://github.com/paulnorthrop/stat0002/raw/master/install/stat0002.zip",
              destfile = paste0(getwd(), "stat0002.zip"))
install.packages("stat0002.zip")

# Apple MacOS

download.file(url = "https://github.com/paulnorthrop/stat0002/raw/master/install/stat0002.tgz",
              destfile = paste0(getwd(), "stat0002.tgz"))
install.packages("stat0002.tgz")

# Linux

install.packages("remotes")
remotes::install_github("paulnorthrop/stat0002")
