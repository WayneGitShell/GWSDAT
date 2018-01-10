

# This is the place in Shiny to define global _constant_ variables. 
# Note that these variables become locked if the software is packaged. 
# 
# The binding can be unlocked, but instead of doing this rather complicated step
# I put all non-constant variables into the server() function (non-global) and pass them to 
# the functions that needs it.
#

coord_units <- c("metres", "feet")
conc_units  <- c("ng/l", "ug/l", "mg/l", "Level", 
                 "metres",  # for GW (groundwater depth)
                 "mm",      # for NAPL thickness
                 "pH")
conc_flags  <- c("", "E-acc", "Omit", "NotInNAPL", "Redox")
conc_header <- list("WellName", "Constituent", "SampleDate", "Result", "Units", "Flags")
well_header <- list("WellName", "XCoord", "YCoord", "Aquifer")
