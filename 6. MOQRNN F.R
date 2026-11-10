# ── 1. ACTIVATION FUNCTIONS ─────────────────────────────────
sigmo            <- function(x) {1 / (1 + exp(-x))}
sigmo.prime      <- function(x) {Th(x) * (1 - Th(x)) }
linear           <- function(x) {x}
linear.prime     <- function(x) {1}
Th            <- function(x) {(exp(x) - exp(-x)) / (exp(x) + exp(-x))}
Th.prime      <- function(x) {1 - Th(x)^2}

huber <- function (x, eps) {
  h <- ifelse(abs(x) > eps, abs(x) - eps/2, (x^2)/(2 * eps))
  h[is.nan(h)] <- 0
  h
}
huber.prime <- function (x, eps) {
  dh <- x/eps
  dh[x > eps] <- 1
  dh[x < -eps] <- -1
  dh[is.nan(dh)] <- 0
  dh
}

hramp <- function (x, lower, eps) {
  if (length(lower) > 1) {
    mapply(hramp, x, lower, eps)
  }
  else {
    if (lower == -Inf) {
      return(x)
    }
    else {
      return(ifelse(x > lower, huber(x - lower, eps), 0) + 
               lower)
    }
  }
}

hramp.prime <- function (x, lower, eps) {
  if (length(lower) > 1) {
    mapply(hramp.prime, x, lower, eps)
  }
  else {
    if (lower == -Inf) {
      return(1)
    }
    else {
      dhr <- (x - lower)/eps
      dhr[x > (lower + eps)] <- 1
      dhr[x < lower] <- 0
      return(dhr)
    }
  }
}

tilted.approx <- function (x, tau, eps) { ifelse(x > 0, tau * huber(x, eps), (1 - tau) * huber(x, eps))}
tilted.approx.prime <- function (x, tau, eps) {ifelse(x > 0, tau * huber.prime(x, eps), (1 - tau) * huber.prime(x, eps))}

# ── 2. SUPPORTER FUNCTIONS ─────────────────────────────────
qrnn.initialize <-function (x, y, n.hidden, init.range = c(-0.05, 0.05, -0.05, 0.05)) {
  if (!is.list(init.range)) {
    if (length(init.range) == 4) {
      r11 <- init.range[1]
      r12 <- init.range[2]
      r21 <- init.range[3]
      r22 <- init.range[4]
    }
    else {
      r11 <- r21 <- init.range[1]
      r12 <- r22 <- init.range[2]
    }
    W1 <- matrix(runif((ncol(x) + 1) * n.hidden, r11, r12), 
                 ncol(x) + 1, n.hidden)
    W2 <- matrix(runif((n.hidden + 1) * ncol(y), r21, r22), 
                 n.hidden + 1, ncol(y))
    weights <- c(W1, W2)
  }
  else {
    weights <- unlist(init.range)
  }
  weights
}

qrnn.reshape <- function (x, y, weights, n.hidden) {
  N11 <- ncol(x) + 1
  N12 <- n.hidden
  N1 <- N11 * N12
  W1 <- weights[1:N1]
  W1 <- matrix(W1, N11, N12)
  N21 <- n.hidden + 1
  N22 <- ncol(y)
  N2 <- N1 + N21 * N22
  W2 <- weights[(N1 + 1):N2]
  W2 <- matrix(W2, N21, N22)
  list(W1 = W1, W2 = W2)
}

gam.mask <- function (x, n.hidden) {
  mask <- matrix(1, ncol(x) + 1, n.hidden)
  nh <- ncol(mask)/ncol(x)
  ij <- matrix(seq(nh * ncol(x)), nrow = nh)
  for (i in seq(ncol(x))) {
    mask[i, c(ij[, -i])] <- 0
  }
  mask
}

# ── 3. MAIN FUNCTIONS ─────────────────────────────────
qrnn.cost <- function (weights, x, y, n.hidden, w, tau, lower, monotone, additive, 
                       eps, Th, Th.prime, penalty, unpenalized) {
  penalty2 <- ifelse(identical(Th, linear), penalty, 0)
  w1w2 <- qrnn.reshape(x, y, weights, n.hidden)
  W1 <- w1w2$W1
  rW1 <- nrow(W1)
  cW1 <- ncol(W1)
  W2 <- w1w2$W2
  rW2 <- nrow(W2)
  cW2 <- ncol(W2)
  if (!is.null(monotone)) {
    W1[monotone, ] <- exp(W1[monotone, ])
    W2[1:(rW2 - 1), ] <- exp(W2[1:(rW2 - 1), ])
  }
  if (!is.logical(additive)) {
    W1 <- W1 * additive
  }
  x <- as.matrix(cbind(x, 1))
  h1 <- x %*% W1
  y1 <- Th(h1)
  aug.y1 <- cbind(y1, 1)
  h2 <- aug.y1 %*% W2
  y2 <- hramp(h2, lower, eps)
  E <- y - y2
  delta2 <- hramp.prime(h2, lower, eps) * tilted.approx.prime(E, tau, eps)
  gradient.W2 <- -(t(aug.y1) %*% sweep(delta2, 1, w[2], "*"))
  if (!is.null(monotone)) {
    gradient.W2[1:(rW2 - 1), ] <- gradient.W2[1:(rW2 - 1),] * W2[1:(rW2 - 1), ]
  }
  gradient.W2.penalty <- 2 * penalty2 * rbind(W2[1:(rW2 - 1), , drop = FALSE], 0)/(length(W2) - cW2)
  E1 <- delta2 %*% t(W2[1:(rW2 - 1), , drop = FALSE])
  delta1 <- Th.prime(h1) * E1
  gradient.W1 = -(t(x) %*% sweep(delta1, 1, w[1], "*"))
  if (!is.null(monotone)) {
    gradient.W1[monotone, ] <- gradient.W1[monotone, ] * 
      W1[monotone, ]
  }
  W1p <- W1
  W1p[c(unpenalized, rW1), ] <- 0
  gradient.W1.penalty <- 2 * penalty * W1p/sum(W1p != 0)
  cost <- sum(w * tilted.approx(E, tau, eps)) + penalty * sum(W1p^2)/sum(W1p != 0) + 
    penalty2 * sum(W2[1:(rW2 - 1), , drop = FALSE]^2)/(length(W2) - cW2)
  gradient <- c(gradient.W1 + gradient.W1.penalty, gradient.W2 +gradient.W2.penalty)
  attr(cost, "gradient") <- gradient
  cost
}

qrnn.nlm <- function (x, y, n.hidden, w, tau, iter.max, n.trials, bag, lower, 
                      init.range, monotone, additive, eps.seq, Th, Th.prime, penalty, 
                      unpenalized, trace, ...) 
{
  cases <- seq(nrow(x))
  if (bag) 
    cases <- sample(nrow(x), replace = TRUE)
  x <- x[cases, , drop = FALSE]
  y <- y[cases, , drop = FALSE]
  w <- w[cases]
  if (length(tau) > 1) 
    tau <- tau[cases]
  if (length(lower) > 1) 
    lower <- lower[cases]
  eps.seq <- sort(eps.seq, decreasing = TRUE)
  cost.best <- Inf #lihat ini infinit positif
  for (i in seq(n.trials)) {
    weights <- qrnn.initialize(x, y, n.hidden, init.range)
    if (any(lower != -Inf)) {
      for (eps in eps.seq) {
        fit <- suppressWarnings(nlm(qrnn.cost, weights, 
                                    iterlim = iter.max, x = x, y = y, n.hidden = n.hidden, 
                                    w = w, tau = tau, lower = -Inf, monotone = monotone, 
                                    additive = additive, eps = eps, Th , Th.prime , 
                                    penalty = penalty, unpenalized = unpenalized, 
                                    check.analyticals = FALSE, ...))
        weights <- fit$estimate
      }
    }
    for (eps in eps.seq) {
      fit <- suppressWarnings(nlm(qrnn.cost, weights, iterlim = iter.max, 
                                  x = x, y = y, n.hidden = n.hidden, w = w, tau = tau, 
                                  lower = lower, monotone = monotone, additive = additive, 
                                  eps = eps, Th , Th.prime , penalty = penalty, 
                                  unpenalized = unpenalized, check.analyticals = FALSE, 
                                  ...))
      weights <- fit$estimate
    }
    cost <- fit$minimum
    if (trace) 
      cat(i, cost, "\n")
    if (cost < cost.best) {
      cost.best <- cost
      weights.best <- fit$estimate
    }
  }
  if (trace) 
    cat("*", cost.best, "\n")
  weights.best <- qrnn.reshape(x, y, weights.best, n.hidden)
  if (!is.logical(additive)) {
    weights.best$W1 <- weights.best$W1 * additive
  }
  weights.best
}

qrnn.fit <- function (x, y, n.hidden, w = NULL, tau = 0.5, n.ensemble = 1, 
                      iter.max = 5000, n.trials = 5, bag = FALSE, lower = -Inf, 
                      init.range = c(-0.5, 0.5, -0.5, 0.5), monotone = NULL, additive = FALSE, 
                      eps.seq = 2^seq(-8, -32, by = -4), Th =tangh, Th.prime=tangh.prime, 
                      penalty = 0, unpenalized = NULL, n.errors.max = 10, trace = TRUE, 
                      scale.y = TRUE, ...) 
{
  if (!is.matrix(x)) 
    stop("\"x\" must be a matrix")
  if (!is.matrix(y)) 
    stop("\"y\" must be a matrix")
  if (any(is.na(c(x, y)))) 
    stop("missing values in \"x\" or \"y\"")
  if (any(apply(x, 2, sd) < .Machine$double.eps^0.5)) 
    stop("zero variance column(s) in \"x\"")
  if (any((tau > 1) | (tau < 0))) 
    stop("invalid \"tau\"")
  if (!identical(Th, linear) && missing(n.hidden)) 
    stop("must specify \"n.hidden\"")
  if (identical(Th, linear)) 
    n.hidden <- 1
  is.whole <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
  if (additive && !is.whole(n.hidden/ncol(x))) 
    stop("\"n.hidden\" must be an integer multiple of \"ncol(x)\" when \"additive=TRUE\"")
  if (is.null(w)) {
    if (ncol(y) == 1) {
      w <- rep(1/nrow(y), nrow(y))
    } else {
      w <- matrix(rep(1/nrow(y)),nrow(y),ncol(y))
    }}
  if (any(w < 0)) 
    stop("invalid \"w\"")
  x <- scale(x)
  x.center <- attr(x, "scaled:center")
  x.scale <- attr(x, "scaled:scale")
  if (scale.y) {
    y <- scale(y)
  } else {
    attr(y, "scaled:center") <- rep(0, ncol(y))
    attr(y, "scaled:scale") <- rep(1, ncol(y))
  }
  y.center <- attr(y, "scaled:center")
  y.scale <- attr(y, "scaled:scale")
  lower.scaled <- (lower - y.center)/y.scale
  if (additive) 
    additive <- gam.mask(x, n.hidden)
  weights <- vector("list", n.ensemble)
  if (trace) {
    if (length(unique(tau)) <= 100) {
      cat("tau =", unique(tau), "\n", sep = " ")
    }
    else {
      cat("tau = [", range(tau), "]; n.tau =", length(tau), 
          "\n", sep = " ")
    }
  }
  for (i in seq(n.ensemble)) {
    if (trace) 
      cat(i, "/", n.ensemble, "\n", sep = "")
    w.tmp <- NA
    class(w.tmp) <- "try-error"
    n.errors <- 0
    while (inherits(w.tmp, "try-error")) {
      w.tmp <- try(qrnn.nlm(x, y, n.hidden, w, tau, iter.max, 
                            n.trials, bag, lower.scaled, init.range, monotone, 
                            additive, eps.seq, Th, Th.prime, penalty, unpenalized, 
                            trace, ...), silent = TRUE)
      n.errors <- n.errors + 1
      if (n.errors > n.errors.max) 
        stop("nlm optimization failed")
    }
    weights[[i]] <- w.tmp
  }
  if (trace) 
    cat("\n")
  parms <- list(weights = weights, lower = lower, eps.seq = eps.seq, 
                tau = tau, Th = Th, x.center = x.center, x.scale = x.scale, 
                y.center = y.center, y.scale = y.scale, monotone = monotone, 
                additive = additive)
  parms
}

qrnn.predict <- function (x, parms) {
  if (!is.matrix(x)) 
    stop("\"x\" must be a matrix")
  weights <- parms$weights
  lower <- parms$lower
  monotone <- parms$monotone
  additive <- parms$additive
  eps <- min(parms$eps.seq)
  Th <- parms$Th
  x.center <- parms$x.center
  x.scale <- parms$x.scale
  y.center <- parms$y.center
  y.scale <- parms$y.scale
  lower <- (lower - y.center)/y.scale
  x <- sweep(x, 2, x.center, "-")
  x <- sweep(x, 2, x.scale, "/")
  y.bag <- matrix(0, ncol = length(weights), nrow = nrow(x))
  for (i in seq_along(weights)) {
    y.bag[, i] <- qrnn.eval(x, weights[[i]]$W1, weights[[i]]$W2, 
                            lower, monotone, additive, eps, Th)
    y.bag[, i] <- y.bag[, i] * y.scale[i] + y.center[i]
  }
  y.bag
}

qrnn.eval <- function (x, W1, W2, lower, monotone, additive, eps, Th) {
  if (!is.null(monotone)) {
    W1[monotone, ] <- exp(W1[monotone, ])
    W2[1:(nrow(W2) - 1), ] <- exp(W2[1:(nrow(W2) - 1), ])
  }
  if (!is.logical(additive)) {
    W1 <- W1 * additive
  }
  x <- cbind(x, 1)
  h1 <- x %*% W1
  y1 <- Th(h1)
  aug.y1 <- cbind(y1, 1)
  y2 <- aug.y1 %*% W2
  y2 <- hramp(y2, lower, eps)
  y2
}

# tuning ketika tau=0.5
tune_qrnn0.5 <- function(x, y,
                         hidden.range = 7:8,
                         penalty.range = c(0, 0.5, 0.1),
                         tau = 0.5,
                         eps.seq = 10^seq(-2, -4, length.out = 5),
                         n.trials = 10, 
                         init.range = c(-0.1,0.5,-0.1,0.5),
                         scale.y=FALSE) {
  
  results <- data.frame()
  best.model <- list()
  best.smape <- Inf
  best.c <- Inf
  
  if (scale.y) {
    y <- scale(y)
  } else {
    attr(y, "scaled:center") <- rep(0, ncol(y))
    attr(y, "scaled:scale") <- rep(1, ncol(y))
  }
  y.center <- attr(y, "scaled:center")
  y.scale <- attr(y, "scaled:scale")
  
  w <- matrix(1/nrow(y), nrow(y), ncol(y))
  
  for (h in hidden.range) {
    
    for (pen in penalty.range) {
      cat("Hidden:", h,
          " Penalty:", pen, "\n")
      cost.best <- Inf
      weights.best <- NULL
      
      # multiple random initializations
      for (i in 1:n.trials) {
        wg <- qrnn.initialize(x = x, y = y, n.hidden = h,init.range = init.range)
        fit <- NULL
        
        # smoothing optimization
        for (eps in eps.seq) {
          fit <- suppressWarnings(
            nlm(qrnn.cost,wg, x = x,y = y,n.hidden = h, w = w,tau = tau,
                lower = -Inf,eps = eps, Th = Th,Th.prime = Th.prime,
                penalty = pen,unpenalized = NULL, iterlim = 5000,gradtol = 1e-4,
                check.analyticals = FALSE, monotone = NULL,additive = FALSE
            )
          )
          wg <- fit$estimate
        }
        if (fit$minimum < cost.best) {
          cost.best <- fit$minimum
          weights.best <- fit$estimate
        }
      }
      
      # prediction in-sample
      weights <- qrnn.reshape(x=x, y=y, weights=weights.best, n.hidden=h)
      
      pred.in <- matrix(0, ncol = length(weights), nrow = nrow(x))
      pred.in<- qrnn.eval(x, weights$W1, weights$W2, lower=-Inf, monotone=NULL, 
                          additive=FALSE, eps=3.906250e-03, Th=Th)
      for (j in 1:ncol(pred.in)) {
        pred.in[, j] <- pred.in[, j] * y.scale[j] + y.center[j]
      }
      
      # calculate quantile loss
      val.loss <- quantile_loss(y, pred.in, tau)
      E.cost <- tilted.approx(x=y-pred.in, tau, min(eps.seq)) 
      
      # calculate smape in-sample
      if (tau == 0.5) {
        smapey <- matrix(0, ncol = length(pred.in), nrow = 1)
        for (j in 1:ncol(pred.in)) {
          smapey[,j] <- smape.100(y[,j], pred.in[, j])
        }
        sm.mean <- mean(c(smapey[,1],smapey[,2]))
      }
      
      # calculate smape out-sample
      
      
      results <- rbind(
        results,
        data.frame(
          hidden = h,
          penalty = pen,
          train.cost = cost.best,
          validation.loss = val.loss,
          y1.smape = smapey[,1],
          y2.smape = smapey[,2],
          mean.smape = sm.mean
        )
      )
      
      if (sm.mean < best.smape) {
        best.smape <- sm.mean
        best.model <- list(
          hidden = h,
          penalty = pen,
          weights = weights,
          prediksi = pred.in,
          train.cost = cost.best,
          validation.loss = val.loss,
          mean.smape = sm.mean
        )
      }
    }
  }
  return(
    list(
      results = results,
      best.model = best.model
    )
  )
}

recursive_moqrnn <- function(W, df, te.new, p,lower=-Inf, monotone=NULL,  
                             additive=FALSE, eps=3.906250e-03, Th=Th){
  W1 <- W[[1]]
  W2 <- W[[2]]
  h <- nrow(te.new)
  forecast <- matrix(0, nrow = h, ncol = 2)
  y_history <- tail(df[,c(2:3)], p)
  for(t in 1:h){
    y_lag <- as.vector(t(y_history))
    x_current <- te.new[t, ]
    Z <- t(as.numeric(c(y_lag, x_current)))
    y_new <- qrnn.eval(Z, W1, W2, lower, monotone, additive, eps, Th)
    forecast[t, ] <- y_new
    if(p > 1){
      y_history <- rbind(as.numeric(y_history[-1, ]),y_new)
    } else {
      y_history[1, ] <- y_new
    }
  }
  return(forecast)
}

qvss <- function(y , ypred_ref, ypred_for, tau, eps, W1, W2, unpenalized =NULL,
                 penalty, penalty2 =0){
  pinball_exact <- function(u, tau) {u * (tau - (u < 0))}
  E_ref <- y-ypred_ref
  E.qvarx <- sum(w * pinball_exact(E_ref, tau))
  
  E_for <- as.matrix(y-ypred_for)
  W1p <- W1
  W1p[c(unpenalized, nrow(W1)), ] <- 0
  w <- matrix(1/nrow(y), nrow(y), ncol(y))
  E.moqrnn <-  sum(w * tilted.approx(E_for, tau, eps)) +
    penalty  * sum(W1p^2) / sum(W1p != 0) +
    penalty2 * sum(W2[1:(nrow(W2) - 1), , drop = FALSE]^2) / (length(W2) - ncol(W2))
  
  r <- 1-(E.moqrnn/E.qvarx)
  return(list(E.moqrnn = E.moqrnn, E.qvarx = E.qvarx, QVSS = r))
}

