# Brent's method root-finding (translated from C)
R_zeroin2 <- function(ax, bx, fa, fb, f, Tol, Maxit) {
    EPSILON <- .Machine$double.eps
    a <- ax
    b <- bx
    c <- a
    fc <- fa
    maxit <- Maxit + 1
    tol <- Tol

    if (fa == 0.0) {
        Tol <- 0.0
        Maxit <- 0
        return(a)
    }
    if (fb == 0.0) {
        Tol <- 0.0
        Maxit <- 0
        return(b)
    }

    while (maxit > 0) {
        maxit <- maxit - 1
        prev_step <- b - a
        tol_act <- 2 * EPSILON * abs(b) + tol / 2
        new_step <- (c - b) / 2

        if (abs(new_step) <= tol_act || fb == 0.0) {
            Maxit <- Maxit - maxit
            Tol <- abs(c - b)
            return(b)
        }

        if (abs(prev_step) >= tol_act && abs(fa) > abs(fb)) {
            cb <- c - b
            if (a == c) {
                # Linear interpolation
                t1 <- fb / fa
                p <- cb * t1
                q <- 1.0 - t1
            } else {
                # Quadratic inverse interpolation
                qv <- fa / fc
                t1 <- fb / fc
                t2 <- fb / fa
                p <- t2 * (cb * qv * (qv - t1) - (b - a) * (t1 - 1.0))
                q <- (qv - 1.0) * (t1 - 1.0) * (t2 - 1.0)
            }
            if (exists("q")) {
                if (p > 0) {
                    q <- -q
                } else {
                    p <- -p
                }
                if (p < (0.75 * cb * q - abs(tol_act * q) / 2) &&
                    p < abs(prev_step * q / 2)) {
                    new_step <- p / q
                }
            }
        }

        if (abs(new_step) < tol_act) {
            if (new_step > 0) {
                new_step <- tol_act
            } else {
                new_step <- -tol_act
            }
        }

        a <- b
        fa <- fb
        b <- b + new_step
        fb <- f(b)
        if ((fb > 0 && fc > 0) || (fb < 0 && fc < 0)) {
            c <- a
            fc <- fa
        }
    }

    Tol <- abs(c - b)
    Maxit <- -1
    return(b)
}



# benchmark uniroot / C_zeroin2 : 6 times faster (!)
f = function(x) {Sys.sleep(0.01); x^2-1.2}
f_lower = f(0)
f_upper = f(2)
tol= 1e-5
rbenchmark::benchmark(
  brentDekker = pracma::brentDekker(f, a=0, b=2,
                        # f.lower = f_lower, f.upper = f_upper,
                        tol = tol, maxiter=1000),
  brent = pracma::brent(f, a=0, b=2,
                        # f.lower = f_lower, f.upper = f_upper,
                        tol = tol, maxiter=1000),
  fzero = pracma::fzero(f, x=1,
                        # lower = 0, upper = 2,
           # f.lower = f_lower, f.upper = f_upper,
           tol = tol, maxiter=1000),
  uniroot = uniroot(f = f, lower = 0, upper = 2,
                    f.lower = f_lower, f.upper = f_upper,
                    tol = tol, maxiter=1000,extendInt="no"),
  C_zeroin2 = .External2(stats:::C_zeroin2, f,
                          0, 2, f.lower = f_lower, f.upper = f_upper, tol, 1000)[1],
  R_zeroin2 = R_zeroin2(  0, 2,  f_lower, f_upper, f, tol, 1000)[1],
  replications = 100,
  columns = c("test", "replications", "elapsed", "relative"),
  order = "relative"
)



