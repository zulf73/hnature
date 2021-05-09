# Mpc units
C <-c(1,1,1,1)
radius_universe <- 3075.69
radius_solar <- 4.5e12
volume_universe <- C[4]*radius_universe^4
volume_solar <- C[4]*radius_solar^4
time_years <- 3e12
seconds_in_year <-3.16e7
volume_per_seconds <- volume_universe/( time_years * seconds_in_year)
vs_per_seconds <- volume_solar/( time_years * seconds_in_year)

