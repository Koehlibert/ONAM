lotf <- list(function(x) cos(2*x),
             function(x) tanh(0.5*x),
             function(x) dnorm(x - 1.5) + dnorm(x + 1.5),
             function(x) cos(x*3-2)*(-x*3),
             function(x) ifelse(x > 0, pweibull(x, shape = 3),
                                pweibull(-x, shape = 0.5)),
             function(x) x^2,
             function(x) cos(2 * x),
             function(x) tanh(x),
             function(x) - x^3,
             function(x) -x * tanh(3*x) * sin(4*x))
interf <- list(function(x1, x2) sin(1.5 * (x1^2 - x2^2)) + dnorm(0.5*x1 * x2),
               function(x1, x2) cos(x1 + x2) + sin(x1 * x2),
               function(x1, x2) 0.5 * sin((x1 - 1)^2+(x2 + 1)^2),
               function(x1, x2) sin(0.75 * x2*x1) + 0.25 *sqrt(x1^2 + x2^2) +
                 0.25 * cos((x1 - pi) * (x2 + pi)),
               function(x1, x2) sin(x1^2+x2^2),
               function(x1, x2) sin(x1 + x2),
               function(x1, x2) -cos(1.5 * x1 - 0.75 * x2),
               function(x1, x2) cos((x1 - pi) * (x2 + pi)),
               function(x1, x2) ((dnorm(x1) / dnorm(0) - 0.5) * 2) *
                 ((pnorm(x2) - 0.5) * 2) +
                 sin(x1 - x2)
)
globalTerm <- function(x)
{
  # return(sqrt(sum(x^2)))
  return((prod(abs(x))^0.125 / (3^10)^0.125 - 0.5) * 2)
}
