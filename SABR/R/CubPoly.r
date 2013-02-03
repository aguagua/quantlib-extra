CubPoly <- function(a, b, r, v, vol, fwd, t){
	# Polynomial coefficients:
	A <- ((1 - b)^2*t)/(24*fwd^(2 - 2*b))
	B <- (r*b*v*t)/(4*fwd^(1 - b))
	C <- 1 + ((2 - 3*r^2)/24)*v^2*t
	return(A*a^3 + B*a^2 + C*a - vol*fwd^(1 - b))
}