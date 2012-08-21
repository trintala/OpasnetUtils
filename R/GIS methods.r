GIS.Exposure <- function(Concentration, LO, LA, distx = 10.5, disty = 10.5) {
	LaPerKm <- dtheta.dy(earth.radius)
	LoPerKm <- dphi.dx(LA, earth.radius)
	
	# Population
	Population <- function(LO, LA, distx = distx, disty = disty, LaPerKm = LaPerKm, LoPerKm = LoPerKm) {
		GetPopLocs <- function(series_id = NULL) {
			dsn <- "heande_base"
			ident = "Heande3182"
			use.utf8 = TRUE
			apply.utf8 = FALSE
			return(opbase.old.locations.read(dsn, ident, use.utf8, apply.utf8))
		}
		
		GetPopData <- function(include = NULL, exclude = NULL, series_id = NULL, iterations = NULL) {
			dsn <- "heande_base"
			ident = "Heande3182"
			use.utf8 = TRUE
			apply.utf8 = FALSE
			return(opbase.old.data.read(dsn, ident))
		}
		
		pop.locs <- GetPopLocs()
		
		# selection where latitude falls within disty km of given coordinates
		pop.slice.la <- pop.locs[
			pop.locs$ind == "Latitude", "loc_id"
		][
			pop.locs[
				pop.locs$ind == "Latitude", "loc"
			] < LA + disty * LaPerKm & 
			pop.locs[
				pop.locs$ind == "Latitude", "loc"
			] > LA - disty * LaPerKm
		] 
		# selection where longitude is beyond distx, inverse selection is required by the current database structure
		pop.slice.lo.inverse <- pop.locs[
			pop.locs$ind == "Longitude", "loc_id"
		][
			pop.locs[
				pop.locs$ind == "Longitude", "loc"
			] > LO + distx * LoPerKm | 
			pop.locs[
				pop.locs$ind == "Longitude", "loc"
			] < LO - distx * LoPerKm
		] 
		
		Population <- tidy(GetPopData(include = pop.slice.la, exclude = pop.slice.lo.inverse)) 
		
		Population$Longitude <- as.numeric(as.character(Population$Longitude))
		Population$Latitude <- as.numeric(as.character(Population$Latitude))
		return(Population)
	}
	
	Population <- Population(LO, LA)
	
	Population$LObin <- cut(Population$Longitude, breaks = LO + (-distx:distx) * LoPerKm)
	Population$LAbin <- cut(Population$Latitude, breaks = LA + (-disty:disty) * LaPerKm)
	
	Population <- new(
		"ovariable",
		output = Population
	)
	
	Population@marginal <- colnames(temp@output) %in% c("Iter", "LObin", "LAbin")
	
	# Assume dx dy given in meters
	Concentration@output$LObin <- cut(Concentration$dx / 1000 * LoPerKm + LO, breaks = LO + (-distx:distx) * LoPerKm)
	Concentration@output$LAbin <- cut(Concentration$dy / 1000 * LaPerKm + LA, breaks = LA + (-disty:disty) * LaPerKm)
	
	temp <- Population * Concentration
	
	temp <- tapply(temp, cols = c("LObin", "LAbin"), sum)
	
	return(temp)
}