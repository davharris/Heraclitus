#' Online sample mean and sample variance computation
#'
#' \emph{The river where you set your foot just now is gone – those waters
#' giving way to this, now this.} ---Heraclitus
#'
#' A \code{moment_stream} object is an \code{\link[R6]{R6}} object with
#' reference semantics. Rather than calculating the mean and variance on a
#' complete data set, which is difficult when the data does not fit in memory
#' or when only parts of it are available, \code{moment_stream}s use Welford's
#' online algorithm to calculate these values during a single pass through the
#' data (which can then be discarded if no other summary statistics are needed).
#'
#' A stream can be monitored by creating an object with
#' \code{stream_name = moment_stream$new()}, and the mean and variance can be
#' calculated as new data comes in using \code{stream_name$update(x)}. The
#' sample mean is stored in \code{stream_name$m} and the sample variance is
#' stored in \code{stream_name$v_hat}.
#'
#' @examples
#' set.seed(1)
#' x = rnorm(1E4)
#' moments = moment_stream$new(x)
#' moments$m     #sample mean of x
#' moments$v_hat #sample variance of x
#'
#' x2 = rnorm(1E3)
#' moments$update(x2)
#' all.equal(moments$m, mean(c(x, x2)))
#' all.equal(moments$v_hat, mean(c(x, x2)))
#'
#' @exportClass moment_stream
moment_stream = R6::R6Class(classname = "moment_stream",
                        public = list(
                          m = NULL,
                          v_hat = NULL,
                          initialize = function(x){
                            private$n = 0
                            if (!missing(x)) {
                              self$update(x)
                            }
                          },
                          update = function(x){
                            for (i in seq_along(x)) {
                              self$update1(x[[i]])
                            }
                          },
                          update1 = function(x){
                            # Stream in one new element, using code that has
                            # been translated from StreamStats.jl/src/var.jl;
                            # see below for license information.
                            private$n = private$n + 1
                            if (private$n == 1) {
                              self$m = x
                              private$sum_sqs = 0.0
                              self$v_hat = NaN
                            } else {
                              m_new = self$m + (x - self$m) / private$n
                              private$sum_sqs = private$sum_sqs +
                                (x - self$m) * (x - m_new)
                              self$m = m_new
                              self$v_hat = private$sum_sqs / (private$n - 1)
                            }
                            c(mean = self$m, var = self$v_hat)
                          }
                        ),
                        private = list(
                          n = NULL,
                          sum_sqs = NULL))


# Copyright (c) 2015: John Myles White and other contributors.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.