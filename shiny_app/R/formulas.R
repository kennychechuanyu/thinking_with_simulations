# The model equations, with their symbols, numbered as in the paper. They are
# written in MathML, which browsers display without an external typesetting
# service.
model_equation <- function(id, expression, number = NULL) div(class = "model-equation", id = id,
  HTML(paste0('<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">',
              expression, '</math>')),
  if (!is.null(number)) span(class = "equation-number", paste0("(", number, ")")))

# V_{i,t}, and the like, in MathML.
mml_sub <- function(base, sub) paste0("<msub><mi>", base, "</mi><mrow>", sub, "</mrow></msub>")
mml_it <- "<mi>i</mi><mo>,</mo><mi>t</mi>"
mml_next <- "<mi>i</mi><mo>,</mo><mi>t</mi><mo>+</mo><mn>1</mn>"

# Step 1: Equation 1
rw_formula_ui <- function() tags$details(class = "formula-details",
  tags$summary("The rule as an equation"),
  P("<i>V</i><sub>i,t</sub> is the learned value of cue <i>i</i>, A or B, after <i>t</i>
     completed trials, and both cues start at zero. On trial <i>t</i> + 1, each cue present
     gets a new value:"),
  model_equation("formula-rw", paste0('
    <mrow>', mml_sub("V", mml_next), '<mo>=</mo>', mml_sub("V", mml_it), '<mo>+</mo>',
    mml_sub("β", "<mi>i</mi>"), '<mo>(</mo>', mml_sub("λ", "<mi>t</mi><mo>+</mo><mn>1</mn>"),
    '<mo>−</mo><munder><mo>∑</mo><mrow><mi>j</mi><mo>∈</mo>',
    mml_sub("C", "<mi>t</mi><mo>+</mo><mn>1</mn>"), '</mrow></munder>',
    mml_sub("V", "<mi>j</mi><mo>,</mo><mi>t</mi>"), '<mo>)</mo></mrow>'), number = 1),
  P("<i>C</i><sub>t+1</sub> is the set of cues present on the trial, and the sum is their
     combined prediction. The outcome &lambda;<sub>t+1</sub> is 1 when the apple appears.
     The quantity in parentheses is the shared error. &beta;<sub>i</sub> is cue <i>i</i>'s
     fixed learning rate, here ", axis_num(default_settings$beta), " for both cues. The
     shared error is calculated before any cue changes value, and an absent cue keeps its
     value."),
  aside("Section 2.2, Equation 1."))

# Step 2: Equations 2 and 3
adaptive_formula_ui <- function() tags$details(class = "formula-details",
  tags$summary("The full rule as equations"),
  P("As in Step 1, <i>t</i> counts completed trials. For a cue <i>i</i> present on trial
     <i>t</i> + 1, the learned value changes by the cue's learning rate times its own
     error:"),
  model_equation("formula-adaptive", paste0('
    <mrow>', mml_sub("V", mml_next), '<mo>=</mo>', mml_sub("V", mml_it), '<mo>+</mo>',
    mml_sub("β", mml_it), '<mo>(</mo>', mml_sub("λ", "<mi>t</mi><mo>+</mo><mn>1</mn>"),
    '<mo>−</mo>', mml_sub("V", mml_it), '<mo>)</mo><mo>,</mo><mspace width="1em"/>',
    mml_sub("β", mml_it), '<mo>=</mo>', mml_sub("S", "<mi>i</mi>"), mml_sub("α", mml_it),
    '</mrow>'), number = 2),
  P("<i>S</i><sub>i</sub> is the cue's salience, ", axis_num(default_settings$S), " for every
     cue, and &alpha;<sub>i,t</sub> its associability, which starts at ",
     axis_num(default_settings$alpha_init), ". Let <i>e</i><sub>i,t+1</sub> =
     |&lambda;<sub>t+1</sub> &minus; <i>V</i><sub>i,t</sub>| be the size of the cue's own
     error, and <i>e</i><sub>&minus;i,t+1</sub> the size of the error made by the summed
     prediction of the other cues present. With one companion, this is the companion's
     error. For a cue presented alone, it is the error of predicting zero,
     |&lambda;<sub>t+1</sub>|. Then"),
  model_equation("formula-associability", paste0('
    <mrow>', mml_sub("α", mml_next), '<mo>=</mo>', mml_sub("α", mml_it), '<mo>+</mo><mi>η</mi>
    <mo>(</mo>', mml_sub("e", "<mo>−</mo><mi>i</mi><mo>,</mo><mi>t</mi><mo>+</mo><mn>1</mn>"),
    '<mo>−</mo>', mml_sub("e", mml_next), '<mo>)</mo></mrow>'), number = 3),
  P("with &eta; = ", axis_num(default_settings$eta), ". Associability is kept between .01 and
     1: a value that would fall outside this range is set to the nearer bound. A cue that makes
     the larger
     error loses associability, and one that makes the smaller error gains it, by an amount
     proportional to the difference between the errors. Equal errors leave associability
     unchanged. Both updates use the values from before the trial, so a change in
     associability first affects learning on the next trial."),
  p("In R, the loop of Step 1 becomes:"),
  tags$pre(class = "code", "for (t in seq_along(design)) {
  cues    <- design[[t]]$cues
  lambda  <- design[[t]]$lambda
  e_own   <- abs(lambda - V[cues])
  e_other <- abs(lambda - (sum(V[cues]) - V[cues]))
  V[cues] <- V[cues] + S[cues] * alpha[cues] * (lambda - V[cues])
  alpha[cues] <- alpha[cues] + eta * (e_other - e_own)
  alpha[cues] <- pmin(1, pmax(.01, alpha[cues]))
}"),
  P("<code>S[cues] * alpha[cues]</code> is the learning rate. The value update uses each
     cue's own error, where the ", RW, " loop used the shared error. The order of the
     updates matters here: B must gain ", num(at(compound$B$ad_gain, 0)), " on its first
     compound trial, not the ", num(default_settings$S * at(compound$B$ad_alpha_after, 0)),
     " that its new associability of ", num(at(compound$B$ad_alpha_after, 0)), " would give."),
  aside("Section 3.3, Equations 2 and 3, and Table 2."))

# Step 3: Equations 4 and 5
carryover_formulas_ui <- function() tags$details(class = "formula-details",
  tags$summary("Both carryover fractions as equations"),
  model_equation("formula-associability-carryover", '
    <mrow><msub><mi>α</mi><mi>i</mi></msub><mrow><mo>(</mo><mn>0</mn><mo>)</mo></mrow>
    <mo>=</mo><msub><mi>α</mi><mtext>init</mtext></msub><mo>+</mo><msub><mi>g</mi><mi>α</mi></msub>
    <mrow><mo>(</mo><msubsup><mi>α</mi><mi>i</mi><mtext>end</mtext></msubsup>
    <mo>−</mo><msub><mi>α</mi><mtext>init</mtext></msub><mo>)</mo></mrow></mrow>', number = 4),
  model_equation("formula-value-carryover", '
    <mrow><msub><mi>V</mi><mi>i</mi></msub><mrow><mo>(</mo><mn>0</mn><mo>)</mo></mrow>
    <mo>=</mo><msub><mi>g</mi><mi>V</mi></msub>
    <msubsup><mi>V</mi><mi>i</mi><mtext>apple</mtext></msubsup></mrow>', number = 5),
  P("&alpha;<sub>i</sub><sup>end</sup> is light <i>i</i>'s associability at the end of the
     compound trials, &alpha;<sub>init</sub> = ", axis_num(default_settings$alpha_init), " the
     associability with which every cue starts, and <i>V</i><sub>i</sub><sup>apple</sup> its
     learned value for the apple at the end of the compound trials. &alpha;<sub>i</sub>(0) is
     the light's associability, and <i>V</i><sub>i</sub>(0) its learned value for the pear,
     before its first pear trial. Both fractions lie between 0 and 1."),
  aside("Section 4.2, Equations 4 and 5."))

# Step 4: Equations 6 and 7, and the threshold rule
response_formulas_ui <- function() tags$details(class = "formula-details",
  tags$summary("The response functions as equations"),
  P("<i>V</i><sub>i</sub>(<i>k</i>) is light <i>i</i>'s learned value for the pear after
     <i>k</i> completed pear trials. The probability of a yes on the next pear trial is"),
  model_equation("formula-observation", '
    <mrow><msub><mi>p</mi><mi>i</mi></msub><mrow><mo>(</mo><mi>k</mi><mo>)</mo></mrow><mo>=</mo>
    <mi>f</mi><mrow><mo>(</mo><msub><mi>V</mi><mi>i</mi></msub><mrow><mo>(</mo><mi>k</mi><mo>)</mo></mrow><mo>)</mo></mrow></mrow>', number = 6),
  P("where <i>f</i> is the response function. The sigmoid is"),
  model_equation("formula-sigmoid", '
    <mrow><mi>f</mi><mrow><mo>(</mo><mi>V</mi><mo>)</mo></mrow><mo>=</mo>
    <mfrac><mn>1</mn><mrow><mn>1</mn><mo>+</mo><mi mathvariant="normal">exp</mi><mrow><mo>[</mo>
      <mrow><mo>−</mo><mi>γ</mi><mrow><mo>(</mo><mi>V</mi><mo>−</mo><mi>θ</mi><mo>)</mo></mrow></mrow><mo>]</mo></mrow></mrow></mfrac></mrow>', number = 7),
  P("&theta; is the learned value at which the probability of a yes is .5, and &gamma; sets
     how steeply the probability rises around that value. Here &gamma; = 8 and &theta; = .5.
     The threshold rule gives <i>f</i>(<i>V</i>) = 1 if <i>V</i> &gt; &theta; and 0
     otherwise, with &theta; = .5. Both functions are applied to B and D alike."),
  aside("Section 5, Equations 6 and 7."))

# Step 5: the response model of the simulated experiments
experiment_formula_ui <- function() tags$details(class = "formula-details",
  tags$summary("How the simulated responses were generated"),
  P("The sigmoid of Step 4 is written with an intercept, <i>b</i> = &minus;&gamma;&theta;,
     and a slope, &gamma;. The probability of a yes at learned value <i>V</i> is then"),
  model_equation("formula-experiment-probability", '
    <mrow><mfrac><mn>1</mn><mrow><mn>1</mn><mo>+</mo><mi mathvariant="normal">exp</mi><mrow><mo>[</mo><mrow><mo>−</mo><mrow><mo>(</mo>
      <mi>b</mi><mo>+</mo><mi>γ</mi><mi>V</mi>
      <mo>)</mo></mrow></mrow><mo>]</mo></mrow></mrow></mfrac></mrow>'),
  P("Each response is drawn as yes with this probability, using the light's learned value
     after the participant's completed pear trials with it. The saved cases use the base
     parameter set of the tutorial: <i>b</i> = &minus;4 and &gamma; = 8, so the midpoint is
     .5, as in Step 4. Each participant is one of two learning-rate types, and a light
     starts with a learning rate of about .12 for the slower type and about .19 for the
     faster type, apart from the salience difference between the physical lights. When a
     candidate is
     fitted, <i>b</i> and &gamma; are estimated from the responses."),
  aside("Section 6.1 and Supplement S2."))

# Step 7: the drift diffusion model
ddm_formulas_ui <- function() tags$details(class = "formula-details",
  tags$summary("How the drift diffusion model gives these predictions"),
  model_equation("formula-ddm-process", '
    <mrow><mi>d</mi><msub><mi>X</mi><mi>t</mi></msub><mo>=</mo><mi>v</mi><mi>d</mi><mi>t</mi>
    <mo>+</mo><mi>d</mi><msub><mi>W</mi><mi>t</mi></msub></mrow>'),
  P("<i>X</i><sub>t</sub> is the accumulated evidence and <i>v</i> the drift rate.
     <i>W</i><sub>t</sub> is random noise whose change over one unit of time has a standard
     deviation of 1. Evidence starts halfway between the boundaries, which are a distance
     <i>a</i> apart. The boundary reached gives the response, and one of the two is the
     correct response. There is no deadline."),
  model_equation("formula-ddm-accuracy", '
    <mrow><mi>P</mi><mrow><mo>(</mo><mtext>correct</mtext><mo>)</mo></mrow><mo>=</mo>
    <mfrac><mn>1</mn><mrow><mn>1</mn><mo>+</mo><mi mathvariant="normal">exp</mi><mrow><mo>(</mo><mrow><mo>−</mo><mi>v</mi><mi>a</mi></mrow><mo>)</mo></mrow></mrow></mfrac></mrow>', number = 9),
  model_equation("formula-ddm-time", '
    <mrow><mi mathvariant="normal">E</mi><mrow><mo>[</mo><msub><mi>T</mi><mtext>decision</mtext></msub><mo>]</mo></mrow><mo>=</mo>
    <mfrac><mi>a</mi><mrow><mn>2</mn><mi>v</mi></mrow></mfrac>
    <mi mathvariant="normal">tanh</mi><mrow><mo>(</mo><mfrac><mrow><mi>v</mi><mi>a</mi></mrow><mn>2</mn></mfrac><mo>)</mo></mrow></mrow>'),
  P("The second expression gives the probability of a correct response, and the third the
     mean decision time, correct and incorrect responses together. Decision time leaves out
     the time needed to perceive the stimulus and to execute the response."),
  aside("Section 8, Equation 9 and the footnote in Section 8.2; Supplement S1."))
