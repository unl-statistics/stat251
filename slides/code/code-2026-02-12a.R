
# Install a python package
library(reticulate)
# On windows you might have to install your own version of python (check if python is in
# a location with 'AppData/Microsoft' in the path name)
# in the terminal: where python

install_python(version="3.14")
py_install(c("skimpy", "numpy"))


