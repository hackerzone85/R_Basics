interval <- 0.05
eps <- seq(0 + interval, 1, interval)
gam <- seq(0 + interval, 0.5, interval)

plot_grid <- expand.grid(eps = eps, gam = gam)

n1 <- function(err, edge) {
  (err * edge^2)
}

n2 <- function(err, edge) {
  (err^2 * edge^4)
}

n3 <- function(err, edge) {
(1/edge^2) * log(1/err)
}

z1 <- n1(plot_grid$eps, plot_grid$gam)
z2 <- n2(plot_grid$eps, plot_grid$gam)
z3 <- n3(plot_grid$eps, plot_grid$gam)
lattice::wireframe(z3~plot_grid$eps*plot_grid$gam
                   , xlab = "eps", ylab = "gam", zlab = "big O"
                   , drape = TRUE
                   , colorkey = TRUE
                   # , screen = list(
                   #   z = -60
                   #   , x = -0
                   #   , y = -0)
                   )
#lattice::levelplot(z2~plot_grid$eps*plot_grid$gam)
#lattice::levelplot(z3~plot_grid$eps*plot_grid$gam)

# T
num_examples <- 100
# S
excess_loss <- 10
# Sum Sigma errors
num_errors <- 40

data.frame(errors = num_errors
           , ineq = (0.5 - gam) * num_examples + excess_loss
           , benchmark = excess_loss / gam
           , gam = gam)


