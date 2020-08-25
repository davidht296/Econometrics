# File:    Clean up
# Author:  David Harris, davidht296@gmail.com
# Date:    2020/03/25

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
