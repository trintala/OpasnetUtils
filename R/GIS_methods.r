########################################################################
# This section contains a couple of general GIS related functions
########################################################################

# Earth radius: quadratic mean or root mean square approximation of the average great-circle  
# circumference derives a radius of about 6372.8 km (Wikipedia).

earth.radius <- 6372.8

# Mathematical function to calculate the central angle of a great circle defined by it's endpoints given as spherical coordinates 
# excluding the radius which is constant by definition. 

central.angle <- function(theta1, phi1, theta2, phi2) {
	2 * asin((sin((theta1 - theta2) / 2)^2 + cos(theta1) * cos(theta2) * sin((phi1 - phi2) / 2)^2)^0.5)
}

# Difference in theta per difference of surface projected y on a spherical surface in degrees / km
dtheta.dy <- function(r) 1 / r * 180 / pi

# Difference in phi per difference of surface projected x on a spherical surface in degrees / km
dphi.dx <- function(r, theta) 1 / (cos(theta * pi / 180) * r) * 180 / pi

##############################
# GIS.Exposure
##########################
# Exposure computes exposure using a given concentration matrix and protected population data from Heande.
# Inputs: 
#	Concentration.matrix - A matrix containing spatially dependent concentratio data; 
#	LO & LA - coordinates of the center of the concentration matrix; 
#	distx & disty - maximum displacement from center of concentration matrix, assumed symmetrical, defaults to 10.5 km (PLTTI matrix);
#	resolution - resolution of concentration matrix, length of side of grid element which are assumed squares, defaults to 1 km (PILTTI matrix)
# Output:
#	An ovariable containing result of Population * Concentration. Output and marginal slots are defined. Spatial information lost in 
# 	summation (though there is an easy way around it). 
#########################################

GIS.Exposure <- function(
	Concentration.matrix, 
	dbug = FALSE,
	...
) {
	bounds <- unique(Concentration.matrix@output[c("LAbin", "LObin")])
	LAlower <- NA
	LAupper <- NA
	LOlower <- NA
	LOupper <- NA
	koord_lower <- list()
	koord_upper <- list()
	a <- 1
	
	Population <- data.frame()
	for (i in 1:nrow(bounds)) {
		if (dbug) {
			cat("Internal loop", a, "start time:", as.character(Sys.time()), ".\n")
			a <- a+1
		}
		tmp <- strsplit(as.character(bounds$LAbin[i]), ",")[[1]]
		LAlower[i] <- as.numeric(substring(tmp[1], 2, nchar(tmp[1])))
		LAupper[i] <- as.numeric(substring(tmp[2], 1, nchar(tmp[2])-1))
		tmp <- strsplit(as.character(bounds$LObin[i]), ",")[[1]]
		LOlower[i] <- as.numeric(substring(tmp[1], 2, nchar(tmp[1])))
		LOupper[i] <- as.numeric(substring(tmp[2], 1, nchar(tmp[2])-1))
		
		# Multiple database reads --> extremely slow
		
		#koord_lower[[i]] <- koordGT(LAlower[i], LOlower[i])
		#koord_upper[[i]] <- koordGT(LAupper[i], LOupper[i])
		
		# Use error handling in case no data found within bounds
		#pop <- tryCatch(
		#	tidy(
		#		opbase.data(
		#			"Op_en2949", 
		#			subset = "2012",
		#			range = list(
		#				XKOORD = c(koord_lower[[i]]$E, koord_upper[[i]]$E), #XKOORD = c(LOlower, LOupper),
		#				YKOORD = c(koord_lower[[i]]$N, koord_upper[[i]]$N)#YKOORD = c(LAlower, LAupper)
		#			)
		#		)
		#	), 
		#	error = function(...) return(NULL)
		#)
		#if (!is.null(pop)) {
		#	pop <- merge(bounds[i,], pop)
		#	
		#	Population <- rbind(Population, pop)
		#}
	}
	
	koord_lower <- koordGT(LAlower, LOlower)
	koord_upper <- koordGT(LAupper, LOupper)
	XKOORD <- c(min(c(koord_lower$E, koord_upper$E)), max(c(koord_lower$E, koord_upper$E)))
	YKOORD <- c(min(c(koord_lower$N, koord_upper$N)), max(c(koord_lower$N, koord_upper$N)))
	
	if (dbug) cat(XKOORD, YKOORD, "\n")
	
	Population <- tryCatch(
		tidy(
			opbase.data(
				"Op_en2949", 
				subset = "2012",
				range = list(
					XKOORD = XKOORD, #XKOORD = c(LOlower, LOupper),
					YKOORD = YKOORD #YKOORD = c(LAlower, LAupper)
				)
			)
		), 
		error = function(...) return(NULL)
	)
	
	if (nrow(Population) == 0) stop('No population data at these coordinates.')
	if (dbug) cat(nrow(Population), "\n")
	
	Population$LObin <- NA
	Population$LAbin <- NA
	
	a <- 1
	
	for (i in 1:nrow(bounds)) {
		cond <- (
			Population$XKOORD > koord_lower$E[i] & 
			Population$XKOORD <= koord_upper$E[i] &
			Population$YKOORD > koord_lower$N[i] & 
			Population$YKOORD <= koord_upper$N[i]
		)
		if (dbug) {
			cat("Bound", a, "matching rows:", sum(cond), "\n")
			a <- a+1
		}
		Population[cond, "LObin"] <- as.character(bounds[i, "LObin"])
		Population[cond, "LAbin"] <- as.character(bounds[i, "LAbin"])
	}
	
	colnames(Population)[colnames(Population)=="Result"] <- "PopulationResult"
	
	Population$PopulationResult <- ifelse(Population$PopulationResult < 0, 0, Population$PopulationResult)
	
	Population <- Ovariable("Population", output = Population, marginal = colnames(Population) %in% c("Iter", "LObin", "LAbin", "HAVAINTO"))
	
	if(dbug) {
		cat(colnames(Concentration.matrix@output), "\n")
		cat(colnames(Population@output), "\n")
	}
	
	# Calculating exposure. 
	out <- Population * Concentration.matrix
	
	return(out)
}

######################################################
# GIS.Concentration.matrix 
##############################################
# Computes a concentration matrix from given emission and coordinates, based on random sampling PILTTI source-receptor-matrices.
# Inputs:
#	Emission - emission of substance in Mga^-1; 
#	LO & LA - coordinates where emission occurs; 
#	distx & disty - maximum displacement in kilometers from center of desired matrix, assumed symmetrical, 
#		defaults to 10.5 km (PLTTI matrix);
#	resolution - resolution of desired matrix (length in kilometers of side of grid element which are assumed squares), 
#		defaults to 1 km (PILTTI matrix);
#	N - number of iterations to be run
# Output:
#	An ovariable containing spatially dependent concentration data. Output and marginal slots are defined. 
##################################

GIS.Concentration.matrix <- function(
	Emission, 
	LO, 
	LA, 
	distx = 10.5, 
	disty = 10.5, 
	resolution = 1, 
	N = 1000, 
	dbug = FALSE, 
	...
) {
	LaPerKm <- dtheta.dy(earth.radius)
	LoPerKm <- dphi.dx(earth.radius, LA)
	
	# PILTTI source-receptor-matrices
	
	PILTTI.matrix <- tidy(op_baseGetData("opasnet_base", "Op_en5797", ...), objname = "PILTTI.matrix") # unit: ugm^-3/Mga^-1
	
	if (N == 0) {
		PILTTI.matrix <- as.data.frame(as.table(tapply(PILTTI.matrix[["PILTTI.matrixResult"]], PILTTI.matrix[,c("dx", "dy")], mean)))
		colnames(PILTTI.matrix)[colnames(PILTTI.matrix) == "Freq"] <- "PILTTI.matrixResult"
		#PILTTI.matrix <- PILTTI.matrix[,c("dy", "dx", "PILTTI.matrixResult")]
	} else {
		# Sampling; first make lists containing row numbers of individual matrices defined in the data. 
		ID.list <- tapply(1:nrow(PILTTI.matrix), PILTTI.matrix[,c("Kaupunki", "Vuosi", "Tyyppi")], list)
		# Then randomly pick N elements of that list to a new list. 
		ID.list.samples <- sample(ID.list, N, replace = TRUE)
		# For which we find the length of each individual list.
		ID.sample.lengths <- sapply(ID.list.samples, length)
		# Take all the values in the list and make one big vector out of it. 
		ID.vec <- unlist(ID.list.samples)
		# Use that vector to select corresponding rows from the original data. 
		PILTTI.matrix <- PILTTI.matrix[ID.vec, c("dx", "dy", "PILTTI.matrixResult")]
		# Add iteration indicator by repeating the numbers 1 to N according to the lengths of the list elements. 
		PILTTI.matrix$Iter <- rep(1:N, times = ID.sample.lengths)
	}
	
	PILTTI.matrix$dy <- as.numeric(as.character(PILTTI.matrix$dy))
	PILTTI.matrix$dx <- as.numeric(as.character(PILTTI.matrix$dx))
	
	# dx and dy in PILTTI matrix is given in meters
	PILTTI.matrix$LObin <- cut(PILTTI.matrix$dx / 1000 * LoPerKm + LO, breaks = LO + seq(-distx, distx, resolution) * LoPerKm)
	PILTTI.matrix$LAbin <- cut(PILTTI.matrix$dy / 1000 * LaPerKm + LA, breaks = LA + seq(-disty, disty, resolution) * LaPerKm)
	
	PILTTI.matrix <- new(
		"ovariable",
		name = "PILTTI.matrix",
		output = PILTTI.matrix,
		marginal = colnames(PILTTI.matrix) %in% c("LObin", "LAbin", "Iter")
	)
	
	if(dbug) {
		cat(colnames(PILTTI.matrix@output), "\n")
		cat(colnames(Emission@output), "\n")
	}
	# Calculate concentratios based on emission and source-receptor-matrix
	out <- PILTTI.matrix * Emission
	return(out)
}



deg2rad <- function(x) {x*2*pi/360}
rad2deg <- function(x) {x*360/(2*pi)}

# Muunnosfunktiot koordinaattiprojektioille
# ETRS89-TM35FIN, geodeettisista tasokoordinaateiksi ja takaisin
# Lähde: JHS 154, 6.6.2008
# http://www.jhs-suositukset.fi/web/guest/jhs/recommendations/154
# 2013-04-29/JeH, loukko (at) loukko (dot) net
# http://www.loukko.net/koord_proj/
# Vapaasti käytettävissä ilman toimintatakuuta.

# Muuntaa desimaalimuotoiset leveys- ja pituusasteet ETRS-TM35FIN -muotoisiksi tasokoordinaateiksi
# Esim.
# koordGT(60.385106872, 19.848136769) --> array(2) { ["N"]=> float(106256.3597039) ["E"]=> float(6715706.37704) }

koordGT <- function(lev_aste, pit_aste) {
	
	# Vakiot
	f = 1 / 298.257222101		# Ellipsoidin litistyssuhde
	a = 6378137					# Isoakselin puolikas
	lambda_nolla = 0.471238898	# Keskimeridiaani (rad), 27 astetta
	k_nolla = 0.9996			# Mittakaavakerroin
	E_nolla = 500000			# Itäkoordinaatti
	
	# Kaavat
	
	# Muunnetaan astemuotoisesta radiaaneiksi
	fii = deg2rad(lev_aste)
	lambda = deg2rad(pit_aste)
	
	n = f / (2-f)
	A1 = (a/(1+n)) * (1 + (n^2/4) + (n^4/64))
	e_toiseen = (2 * f) - f^2
	e_pilkku_toiseen = e_toiseen / (1 - e_toiseen)
	h1_pilkku = (1/2)*n - (2/3)*n^2 + (5/16)*n^3 + (41/180)*n^4
	h2_pilkku = (13/48)*n^2 - (3/5)*n^3 + (557/1440)*n^4
	h3_pilkku =(61/240)*n^3 - (103/140)*n^4
	h4_pilkku = (49561/161280)*n^4
	Q_pilkku = asinh(tan(fii))
	Q_2pilkku = atanh(sqrt(e_toiseen) * sin(fii))
	Q = Q_pilkku - sqrt(e_toiseen) * Q_2pilkku
	l = lambda - lambda_nolla
	beeta = atan(sinh(Q))
	eeta_pilkku = atanh(cos(beeta) * sin(l))
	zeeta_pilkku = asin(sin(beeta)/(1/cosh(eeta_pilkku)))
	zeeta1 = h1_pilkku * sin( 2 * zeeta_pilkku) * cosh( 2 * eeta_pilkku)
	zeeta2 = h2_pilkku * sin( 4 * zeeta_pilkku) * cosh( 4 * eeta_pilkku)
	zeeta3 = h3_pilkku * sin( 6 * zeeta_pilkku) * cosh( 6 * eeta_pilkku)
	zeeta4 = h4_pilkku * sin( 8 * zeeta_pilkku) * cosh( 8 * eeta_pilkku)
	eeta1 = h1_pilkku * cos( 2 * zeeta_pilkku) * sinh( 2 * eeta_pilkku)
	eeta2 = h2_pilkku * cos( 4 * zeeta_pilkku) * sinh( 4 * eeta_pilkku)
	eeta3 = h3_pilkku * cos( 6 * zeeta_pilkku) * sinh( 6 * eeta_pilkku)
	eeta4 = h4_pilkku * cos( 8 * zeeta_pilkku) * sinh( 8 * eeta_pilkku)
	zeeta = zeeta_pilkku + zeeta1 + zeeta2 + zeeta3 + zeeta4
	eeta = eeta_pilkku + eeta1 + eeta2 + eeta3 + eeta4
	
	# Tulos tasokoordinaatteina
	N = A1 * zeeta * k_nolla
	E = A1 * eeta * k_nolla + E_nolla
	
	return(data.frame(N, E))
}


# koordTG 
# Muuntaa ETRS-TM35FIN -muotoiset tasokoordinaatit desimaalimuotoisiksi leveys- ja pituusasteiksi
# koordTG(106256.35958, 6715706.37705) --> array(2) { ["lev"]=> float(60.38510687197) ["pit"]=> float(19.848136766751) }

koordTG<- function(N, E) {
	
	# Vakiot	
	f = 1 / 298.257222101				# Ellipsoidin litistyssuhde
	a = 6378137									# Isoakselin puolikas
	lambda_nolla = 0.471238898	# Keskimeridiaani (rad), 27 astetta
	k_nolla = 0.9996						# Mittakaavakerroin
	E_nolla = 500000						# Itäkoordinaatti
	
	# Kaavat
	n = f / (2-f)
	A1 = (a/(1+n)) * (1 + (n^2/4) + (n^4/64))
	e_toiseen = (2 * f) - f^2
	h1 = (1/2)*n - (2/3)*n^2 + (37/96)*n^3 - (1/360)*n^4
	h2 = (1/48)*n^2 + (1/15)*n^3 - (437/1440)*n^4
	h3 =(17/480)*n^3 - (37/840)*n^4
	h4 = (4397/161280)*n^4
	zeeta = N / (A1 * k_nolla)
	eeta = (E - E_nolla) / (A1 * k_nolla)
	zeeta1_pilkku = h1 * sin( 2 * zeeta) * cosh( 2 * eeta)
	zeeta2_pilkku = h2 * sin( 4 * zeeta) * cosh( 4 * eeta)
	zeeta3_pilkku = h3 * sin( 6 * zeeta) * cosh( 6 * eeta)
	zeeta4_pilkku = h4 * sin( 8 * zeeta) * cosh( 8 * eeta)
	eeta1_pilkku = h1 * cos( 2 * zeeta) * sinh( 2 * eeta)
	eeta2_pilkku = h2 * cos( 4 * zeeta) * sinh( 4 * eeta)
	eeta3_pilkku = h3 * cos( 6 * zeeta) * sinh( 6 * eeta)
	eeta4_pilkku = h4 * cos( 8 * zeeta) * sinh( 8 * eeta)
	zeeta_pilkku = zeeta - (zeeta1_pilkku + zeeta2_pilkku + zeeta3_pilkku + zeeta4_pilkku)
	eeta_pilkku = eeta - (eeta1_pilkku + eeta2_pilkku + eeta3_pilkku + eeta4_pilkku)
	beeta = asin((1/cosh(eeta_pilkku)*sin(zeeta_pilkku)))
	l = asin(tanh(eeta_pilkku)/(cos(beeta)))
	Q = asinh(tan(beeta))
	Q_pilkku = Q + sqrt(e_toiseen) * atanh(sqrt(e_toiseen) * tanh(Q))
	
	#for (kierros in 1:5) {
	#	Q_pilkku = Q + sqrt(e_toiseen) * atanh(sqrt(e_toiseen) * tanh(Q_pilkku))
	#}
	
	# Tulos radiaaneina
	fii = atan(sinh(Q_pilkku))
	lambda = lambda_nolla + l
	
	# Tulos asteina
	fii = rad2deg(fii)
	lambda = rad2deg(lambda)
	
	return(data.frame(LA = fii, LO = lambda))
}