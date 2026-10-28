#include <Rcpp.h>
#include <algorithm>
#include <array>
#include <cmath>
using namespace Rcpp;

// The learning recursions and likelihood of Sections 6 and 7 (Supplement S2),
// used through R/inference.R. tests/verify_inference.R checks the recursions
// against inference_reference(), which uses the simulators in R/models.R, and
// the likelihood against a direct calculation.
//
// par holds, in the order of inference_names: mu, spread (tau), cue (d),
// intercept (b), log_slope (log gamma), g_value (g_V), eta, g_alpha and
// history_intercept (b_B). A participant has the slower (z = -1) or the
// faster (z = +1) learning-rate type, and cue i has the starting learning rate
// r_i = .5 logistic(mu + u + x_i d) of Equation S2, with u = tau z,
// x_B = allocation, x_D = -allocation and x = 0 for the tones A and C and the
// buzzer X. Rescorla-Wagner uses the fixed learning rates r_i; the adaptive
// model uses the saliences S_i = 2 r_i and starting associability .5. Cues 0-4
// are A, B, C, D and X; v holds their values and a their associabilities.

inline double logistic(double x) {
  return x >= 0 ? 1 / (1 + std::exp(-x)) : std::exp(x) / (1 + std::exp(x));
}

// One trial with cue i alone (j < 0) or with cue j, followed by the outcome
// (lambda = 1). Rescorla-Wagner updates the cues present with their shared
// error (Equation 1). The adaptive model updates each value with the cue's own
// error (Equation 2) and associability with Equation 3, both from the values
// at the start of the trial: associability changes by eta times the other
// cue's error minus its own and stays within [.01, 1], as in R/models.R. A cue
// presented alone is compared with predicting zero, whose error is 1.
inline void update(std::array<double, 5>& v, std::array<double, 5>& a,
                   const std::array<double, 5>& r, int i, int j,
                   bool adaptive, double eta) {
  const double vi = v[i], vj = j < 0 ? 0 : v[j];
  if (adaptive) {
    v[i] += 2 * r[i] * a[i] * (1 - vi);
    if (j >= 0) v[j] += 2 * r[j] * a[j] * (1 - vj);
    const double ei = std::abs(1 - vi), ej = std::abs(1 - vj);
    a[i] = std::max(.01, std::min(1., a[i] + eta * (ej - ei)));
    if (j >= 0) a[j] = std::max(.01, std::min(1., a[j] + eta * (ei - ej)));
  } else {
    const double error = 1 - vi - vj;
    v[i] += r[i] * error;
    if (j >= 0) v[j] += r[j] * error;
  }
}

// Response probabilities of Equation S3, with b_B added for B, for the four
// groups of participants. Rows are counterbalancing group -1 (slower, then
// faster type) and group +1 (slower, faster); the first n_pear columns are B's
// pear trials and the next n_pear are D's, the layout that R/inference.R
// expects. The probability on pear trial q uses the value after q - 1
// completed pear trials, before that trial's outcome. The pear trials run the
// B block first, or the D block first if d_first is true; the columns keep the
// same layout.
// [[Rcpp::export]]
NumericMatrix inference_prob_cpp(NumericVector par, bool adaptive,
                                 int n_train = 10, int n_pear = 10,
                                 bool d_first = false) {
  if (par.size() != 9 || n_train < 1 || n_pear < 1)
    stop("Invalid parameter vector or design.");
  const double mu = par[0], spread = par[1], cue_contrast = par[2];
  const double intercept = par[3], log_slope = par[4], g_value = par[5];
  const double eta = par[6], g_alpha = par[7], history_intercept = par[8];
  NumericMatrix p(4, 2 * n_pear);
  for (int group = 0; group < 2; ++group) {
    const int allocation = group == 0 ? -1 : 1;
    for (int type = 0; type < 2; ++type) {
      const double u = (type == 0 ? -1 : 1) * spread;
      std::array<double, 5> r, v = {}, a = {{.5, .5, .5, .5, .5}};
      for (int i = 0; i < 5; ++i) {
        const int x = i == 1 ? allocation : (i == 3 ? -allocation : 0);
        r[i] = .5 * logistic(mu + u + x * cue_contrast);
      }
      // A+ and X+ trials, then AB+ trials and CD+ trials, in which light D is
      // presented with tone C. The two histories share no cues, so
      // interleaving their trials gives the same states as running them one
      // after the other, as inference_reference() does.
      for (int t = 0; t < n_train; ++t) {
        update(v, a, r, 0, -1, adaptive, eta);
        update(v, a, r, 4, -1, adaptive, eta);
      }
      for (int t = 0; t < n_train; ++t) {
        update(v, a, r, 0, 1, adaptive, eta);
        update(v, a, r, 3, 2, adaptive, eta);
      }
      // Carryover to the pear trials: values g_V V (Equation 5) and
      // associabilities .5 + g_alpha (alpha - .5) (Equation 4).
      for (int i = 0; i < 5; ++i) {
        v[i] *= g_value;
        a[i] = .5 + g_alpha * (a[i] - .5);
      }
      // The two blocks of pear trials, one light's after the other's. A trial
      // with one light changes only that light's state, so the order of the
      // blocks does not change the probabilities.
      for (int block = 0; block < 2; ++block) {
        const int cue = d_first ? 1 - block : block;
        const int i = cue == 0 ? 1 : 3;
        for (int k = 0; k < n_pear; ++k) {
          p(2 * group + type, cue * n_pear + k) = logistic(
            intercept + std::exp(log_slope) * v[i] + (cue == 0 ? history_intercept : 0));
          update(v, a, r, i, -1, adaptive, eta);
        }
      }
    }
  }
  return p;
}

// Log likelihood log L_s of each participant (Equation S4): the probability
// of the participant's complete set of responses, averaged over the two
// learning-rate types and computed with log-sum-exp. NA responses are not
// analyzed; their pear trials still update the learned state. In the
// candidates, as in the generating models, a pear trial with one light leaves
// the other unchanged, so the probabilities are computed with the B block
// first whatever the participant's block order.
// [[Rcpp::export]]
NumericVector inference_loglik_cpp(NumericVector par, bool adaptive,
                                   NumericMatrix y, IntegerVector allocation,
                                   int n_train = 10) {
  const int n = y.nrow(), n_responses = y.ncol();
  if (n_responses % 2 || allocation.size() != n) stop("Invalid response matrix.");
  NumericMatrix p = inference_prob_cpp(par, adaptive, n_train, n_responses / 2);
  // Probabilities are kept 1e-12 from 0 and 1 so that every logarithm is finite.
  NumericMatrix log_yes(4, n_responses), log_no(4, n_responses);
  for (int row = 0; row < 4; ++row) {
    for (int col = 0; col < n_responses; ++col) {
      double prob = std::max(1e-12, std::min(1 - 1e-12, p(row, col)));
      log_yes(row, col) = std::log(prob);
      log_no(row, col) = std::log1p(-prob);
    }
  }
  NumericVector loglik(n);
  for (int s = 0; s < n; ++s) {
    if (allocation[s] != -1 && allocation[s] != 1) stop("Invalid allocation.");
    // The participant's rows of p: the slower rate type, then the faster.
    const int row = allocation[s] == -1 ? 0 : 2;
    double ll_slower = 0, ll_faster = 0;
    for (int col = 0; col < n_responses; ++col) {
      double response = y(s, col);
      if (NumericVector::is_na(response)) continue;
      if (response != 0 && response != 1) stop("Responses must be binary or NA.");
      ll_slower += response == 1 ? log_yes(row, col) : log_no(row, col);
      ll_faster += response == 1 ? log_yes(row + 1, col) : log_no(row + 1, col);
    }
    const double ll_max = std::max(ll_slower, ll_faster);
    loglik[s] = ll_max +
                std::log(std::exp(ll_slower - ll_max) + std::exp(ll_faster - ll_max)) -
                std::log(2.);
  }
  return loglik;
}
