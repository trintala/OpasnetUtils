# General GIS related functions

# Earth radius: quadratic mean or root mean square approximation of the average great-circle  
# circumference derives a radius of about 6372.8 km (Wikipedia).

earth.radius <- 6372.8

central.angle <- function(theta1, phi1, theta2, phi2) {
	2 * asin((sin((theta1 - theta2) / 2)^2 + cos(theta1) * cos(theta2) * sin((phi1 - phi2) / 2)^2)^0.5)
}

dtheta.dy <- function(r) 1 / r # spherical coordinate theta derived with respect to y as projected on the spherical surface

dphi.dx <- function(r, theta) 1 / (cos(theta) * r) # spherical coordinate phi derived with respect to x as projected on the spehrical surface

##############################
# GIS.Exposure
##########################
# Exposure computes using a given concentration matrix and protected population data from Heande
#########################################

GIS.Exposure <- function(Concentration.matrix, LO, LA, distx = 10.5, disty = 10.5, resolution = 1) {
	LaPerKm <- dtheta.dy(earth.radius)
	LoPerKm <- dphi.dx(earth.radius, LA)
	
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
	
	Population$LObin <- cut(Population$Longitude, breaks = LO + seq(-distx, distx, resolution) * LoPerKm)
	Population$LAbin <- cut(Population$Latitude, breaks = LA + seq(-disty, disty, resolution) * LaPerKm)
	
	Population <- new(
		"ovariable",
		output = Population
	)
	
	Population@marginal <- colnames(temp@output) %in% c("Iter", "LObin", "LAbin")
	
	temp <- Population * Concentration
	
	temp <- tapply(temp, cols = c("LObin", "LAbin"), sum)
	
	return(temp)
}

######################################################
# GIS.Concentration.matrix 
##############################################
# Computes a concentration matrix from given emission and coordinates, based on random sampling PILTTI source-receptor-matrices.
##################################

GIS.Concentration.matrix <- function(Emission, LO, LA, distx = 10.5, disty = 10.5, resolution = 1, N = 1000, ...) { # Emission unit should be Mga^-1
	LaPerKm <- dtheta.dy(earth.radius)
	LoPerKm <- dphi.dx(earth.radius, LA)
	
	# PILTTI source-receptor-matrices
	
	PILTTI.matrix <- tidy(op_baseGetData("opasnet_base", "Op_en5797")) # unit: ugm^-3/Mga^-1
	
	PILTTI.matrix$dy <- as.numeric(as.character(PILTTI.matrix$dy))
	PILTTI.matrix$dx <- as.numeric(as.character(PILTTI.matrix$dx))
	
	colnames(PILTTI.matrix)[colnames(PILTTI.matrix)=="Result"] <- "PILTTI.matrixResult"
	
	# Sampling
	
	ID.list <- tapply(1:nrow(PILTTI.matrix), PILTTI.matrix[,c("Kaupunki", "Vuosi", "Tyyppi")], list)
	ID.list.samples <- sample(ID.list, N, replace = TRUE)
	ID.sample.lengths <- sapply(ID.list.samples, length)
	ID.vec <- unlist(ID.list.samples)
	PILTTI.matrix <- PILTTI.matrix[ID.vec,]
	PILTTI.matrix$Iter <- rep(1:N, each = ID.sample.lengths)
	
	# Assume dx dy given in meters
	PILTTI.matrix$LObin <- cut(PILTTI.matrix$dx / 1000 * LoPerKm + LO, breaks = LO + seq(-distx, distx, resolution) * LoPerKm)
	PILTTI.matrix$LAbin <- cut(PILTTI.matrix$dy / 1000 * LaPerKm + LA, breaks = LA + seq(-disty, disty, resolution) * LaPerKm)
	
	PILTTI.matrix <- new(
		"ovariable",
		name = "PILTTI.matrix",
		output = PILTTI.matrix,
		marginal = colnames(PILTTI.matrix) %in% c("LObin", "LAbin", "Iter")
	)
	
	out <- PILTTI.matrix * Emission
	return(out)
}