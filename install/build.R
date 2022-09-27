# Creating the built packages

path <- paste0(getwd(), "/install")

# source: .tar.gz
devtools::build(manual = TRUE)

# Windows: .zip
# To ensure that a doc directory with the vignettes in it we create the
# binary zip file from the source tar.gz file
devtools::build("../stat0002_1.0.0.tar.gz", binary=TRUE)

# Now move the files to the install directory
file.copy(from = "../stat0002_1.0.0.tar.gz", to = "install/stat0002.tar.gz",
          overwrite = TRUE)
file.copy(from = "../stat0002_1.0.0.zip", to = "install/stat0002.zip",
          overwrite = TRUE)
# Remove the originals
file.remove("../stat0002_1.0.0.tar.gz")
file.remove("../stat0002_1.0.0.zip")

# Mac osx: .tgz
# Submit .tar.gz to macOS builder at
#         https://mac.r-project.org/macbuilder/submit.html
# Follow the link
# Save results.tar.bz2 to install/
# WinSCP results.tar.bz2 to karl
# tar -xvjf results.tar.bz2 in unix shell
# Move to directory big-sur-arm64/bin.4.2 (or similar)
# Copy back stat0002_1.0.0.tgz (or similar)

##### Not needed any more  ...

# NB. macOS builder doesn't offer tcltk
# A temporary solution is to remove dependence on rpanel (and tkrplot)
# 1. Add ^.*_movie\.R$ to .Rbuildingignore
# 2. Remove rpanel, smovie (Imports) and tkrplot (Suggests) from DESCRIPTION
# 3. build(path = path, manual = TRUE) again
# 4. Submit to macOS builder

# Obviously, I need to reverse 1. and 2. to create the .tar.gz and .zip files
# People who have macs with software that can install from .tar.gz could do that/
