# General GIS related functions

# Earth radius: quadratic mean or root mean square approximation of the average great-circle  
# circumference derives a radius of about 6372.8 km (Wikipedia).

earth.radius <- 6372.8

central.angle <- function(theta1, phi1, theta2, phi2) {
	2 * asin((sin((theta1 - theta2) / 2)^2 + cos(theta1) * cos(theta2) * sin((phi1 - phi2) / 2)^2)^0.5)
}

# difference in theta per difference of surface projected y on a spherical surface in degrees / km
dtheta.dy <- function(r) 1 / r * 180 / pi

# difference in phi per difference of surface projected x on a spherical surface in degrees / km
dphi.dx <- function(r, theta) 1 / (cos(theta) * r) * 180 / pi

##############################
# GIS.Exposure
##########################
# Exposure computes using a given concentration matrix and protected population data from Heande
#########################################

GIS.Exposure <- function(Concentration.matrix, LO, LA, distx = 10.5, disty = 10.5, resolution = 1) {
	LaPerKm <- dtheta.dy(earth.radius)
	LoPerKm <- dphi.dx(earth.radius, LA)
	
	# Population
	
	Population <- function(LO, LA, LaPerKm, LoPerKm, distx = 10.5, disty = 10.5) {
		GetPopLocs <- function(...) {
			return(opbase.old.locations.read("heande_base", "Heande3182", use.utf8 = TRUE, apply.utf8 = FALSE, ...))
		}
		
		GetPopData <- function(...) {
			return(opbase.old.read("heande_base", "Heande3182", use.utf8 = TRUE, apply.utf8 = FALSE, ...))
		}
		
		pop.locs <- GetPopLocs()
		
		# selection where latitude falls within disty km of given coordinates
		pop.slice.la <- pop.locs$loc_id[
			pop.locs$ind == "Latitude" & 
			pop.locs$loc < LA + disty * LaPerKm & 
			pop.locs$loc > LA - disty * LaPerKm
		]
		# selection where longitude is beyond distx, inverse selection is required by the current database structure
		pop.slice.lo <- pop.locs$loc_id[
			pop.locs$ind == "Longitude" &
			pop.locs$loc < LO + distx * LoPerKm &
			pop.locs$loc > LO - distx * LoPerKm
		]
		
		pop.slice.lo.inverse <- pop.locs$loc_id[
			pop.locs$ind == "Longitude" &
			!pop.locs$loc_id %in% pop.slice.lo
		]
		
		Population <- tidy(GetPopData(include = pop.slice.la, exclude = pop.slice.lo.inverse)) 
		
		Population$Longitude <- as.numeric(as.character(Population$Longitude))
		Population$Latitude <- as.numeric(as.character(Population$Latitude))
		return(Population)
	}
	
	Population <- Population(LO, LA, LaPerKm, LoPerKm)
	
	Population$LObin <- cut(Population$Longitude, breaks = LO + seq(-distx, distx, resolution) * LoPerKm)
	Population$LAbin <- cut(Population$Latitude, breaks = LA + seq(-disty, disty, resolution) * LaPerKm)
	
	Population <- new(
		"ovariable",
		output = Population
	)
	
	Population@marginal <- colnames(Population@output) %in% c("Iter", "LObin", "LAbin")
	
	temp <- Population * Concentration.matrix
	
	temp <- oapply(temp, cols = c("LObin", "LAbin"), sum)
	
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
	PILTTI.matrix$Iter <- rep(1:N, times = ID.sample.lengths)
	
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