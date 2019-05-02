mad <- function(x) {
  (-1)^x/(2*x + 1)
}

pi/4
sum(mad(0:9))
sum(mad(0:99))
sum(mad(0:999))
sum(mad(0:9999))
sum(mad(0:99999))
sum(mad(0:999999))
mad(0:9)
