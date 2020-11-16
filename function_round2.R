#### ---- Rounding function in R ---- ####

# Function
round2 = function(x, n=0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Examples

round(c(4.5,5.5,6.5)) # Pre-defined R Function

round2(c(4.5,5.5,6.5)) # User-defined R Function
