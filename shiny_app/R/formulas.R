# The model equations of Steps 1 and 3 to 6, with their symbols. They are
# written in MathML, which browsers display without an external typesetting
# service.
model_equation <- function(id, expression) div(class = "model-equation", id = id,
  HTML(paste0('<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">',
              expression, '</math>')))

learning_formulas_ui <- function() tagList(
  P("In the equations, <i>V</i><sub>i,t</sub> is cue <i>i</i>'s value before
     trial <i>t</i>, and &Delta;<i>V</i><sub>i,t</sub> is the amount it gains
     on that trial. The outcome,
     &lambda;<sub>t</sub>, is 1 on every training trial here."),
  div(class = "model-rules",
    div(class = "model-rule",
      h3(class = "sub", HTML(paste0(RW, ": one shared error"))),
      model_equation("formula-rw", '
        <mrow><mi>Δ</mi><msub><mi>V</mi><mrow><mi>i</mi><mo>,</mo><mi>t</mi></mrow></msub>
        <mo>=</mo><msub><mi>α</mi><mi>i</mi></msub><mi>β</mi>
        <mo>(</mo><msub><mi>λ</mi><mi>t</mi></msub><mo>−</mo>
        <munder><mo>∑</mo><mrow><mi>j</mi><mo>∈</mo><msub><mi>C</mi><mi>t</mi></msub></mrow></munder>
        <msub><mi>V</mi><mrow><mi>j</mi><mo>,</mo><mi>t</mi></mrow></msub><mo>)</mo></mrow>'),
      P("<i>C</i><sub>t</sub> contains the cues present on the trial. Their values are
         summed before the error is calculated. Here &alpha;<sub>i</sub> = ",
         axis_num(default_settings$rw_alpha), " is a fixed cue salience and &beta; = ",
         axis_num(default_settings$rw_beta), " is a fixed learning rate. Their product, ",
         axis_num(default_settings$rw_alpha * default_settings$rw_beta), ", is the update multiplier.")),
    div(class = "model-rule",
      h3(class = "sub", "Adaptive model: each cue's own error"),
      model_equation("formula-adaptive", '
        <mrow><mi>Δ</mi><msub><mi>V</mi><mrow><mi>i</mi><mo>,</mo><mi>t</mi></mrow></msub>
        <mo>=</mo><msub><mi>S</mi><mi>i</mi></msub>
        <msub><mi>α</mi><mrow><mi>i</mi><mo>,</mo><mi>t</mi></mrow></msub>
        <mo>(</mo><msub><mi>λ</mi><mi>t</mi></msub><mo>−</mo>
        <msub><mi>V</mi><mrow><mi>i</mi><mo>,</mo><mi>t</mi></mrow></msub><mo>)</mo></mrow>'),
      P("Here <i>S</i><sub>i</sub> = ", axis_num(default_settings$S), " is fixed salience.
         &alpha;<sub>i,t</sub> is associability: it starts at ",
         axis_num(default_settings$alpha_init), " and can change after each trial."))),
  P("Salience is a cue's fixed weight in learning: a larger weight gives a larger
     value update for the same error and associability. The adaptive model uses
     &alpha; for changing associability; Rescorla&ndash;Wagner uses it for fixed salience.
     Add &Delta;<i>V</i> to the current value to get the next value.
     A cue that is absent keeps its value."),
  tags$details(id = "associability-equations", class = "formula-details",
    tags$summary("How associability changes"),
    P("Compare absolute prediction errors at the start of the trial:
       <i>e</i><sub>i,t</sub> = |&lambda;<sub>t</sub> &minus; <i>V</i><sub>i,t</sub>|.
       In a two-cue compound, <i>e</i><sub>&minus;i,t</sub> is the other cue's error."),
    model_equation("formula-associability", '
      <mrow><msub><mi>α</mi><mrow><mi>i</mi><mo>,</mo><mi>t</mi><mo>+</mo><mn>1</mn></mrow></msub>
      <mo>=</mo><msub><mi>α</mi><mrow><mi>i</mi><mo>,</mo><mi>t</mi></mrow></msub>
      <mo>+</mo><mi>η</mi><mo>(</mo><msub><mi>e</mi><mrow><mo>−</mo><mi>i</mi><mo>,</mo><mi>t</mi></mrow></msub>
      <mo>−</mo><msub><mi>e</mi><mrow><mi>i</mi><mo>,</mo><mi>t</mi></mrow></msub><mo>)</mo></mrow>'),
    P("&eta; = ", axis_num(default_settings$eta), " controls how strongly associability
       responds to the difference in errors. A cue that makes the smaller error gains
       associability; one that makes the larger error loses it, and the change grows
       with the difference. Equal errors leave associability unchanged.
       When a cue appears alone, the reference error is |&lambda;<sub>t</sub>|,
       the error of predicting zero. The value update uses the current associability;
       the changed associability takes effect on the next trial."),
    p(class = "muted", "Associability is kept between .01 and 1: a value that would fall
       outside this range is set to the nearer bound."))
)

carryover_formulas_ui <- function() tags$details(class = "formula-details",
  tags$summary("Starting values and associability for O2"),
  model_equation("formula-value-carryover", '
    <mrow><msub><mi>V</mi><mrow><mi>i</mi><mo>,</mo><mi>O2</mi></mrow></msub><mrow><mo>(</mo><mn>0</mn><mo>)</mo></mrow>
    <mo>=</mo><msub><mi>g</mi><mi>V</mi></msub>
    <msubsup><mi>V</mi><mrow><mi>i</mi><mo>,</mo><mi>O1</mi></mrow><mtext>end</mtext></msubsup></mrow>'),
  model_equation("formula-associability-carryover", '
    <mrow><msub><mi>α</mi><mi>i</mi></msub><mrow><mo>(</mo><mn>0</mn><mo>)</mo></mrow>
    <mo>=</mo><msub><mi>α</mi><mtext>init</mtext></msub><mo>+</mo><msub><mi>g</mi><mi>α</mi></msub>
    <mrow><mo>(</mo><msubsup><mi>α</mi><mi>i</mi><mtext>end</mtext></msubsup>
    <mo>−</mo><msub><mi>α</mi><mtext>init</mtext></msub><mo>)</mo></mrow></mrow>'),
  P("Use the first equation to set each cue's O2 value in either model. Use the
     second to set associability in the adaptive model, starting from baseline
     &alpha;<sub>init</sub> = ", axis_num(default_settings$alpha_init), ". Setting ", gV,
     " to 0 makes both O2 values zero. Setting ", gA, " to 0 resets associability
     to baseline; setting it to 1 keeps the final O1 associability. Values between
     0 and 1 keep part of the change from baseline."))

response_formulas_ui <- function() tags$details(class = "formula-details",
  tags$summary("Response equations"),
  P("<i>Y</i><sub>q</sub> is 1 for yes and 0 for no. <i>V</i>(<i>q</i> &minus; 1) is the
     cue's value before feedback on presentation <i>q</i>, after <i>q</i> &minus; 1 updates."),
  h3(class = "sub", "Sigmoid"),
  model_equation("formula-sigmoid", '
    <mrow><mi>P</mi><mrow><mo>(</mo><msub><mi>Y</mi><mi>q</mi></msub><mo>=</mo><mn>1</mn><mo>)</mo></mrow><mo>=</mo>
    <mfrac><mn>1</mn><mrow><mn>1</mn><mo>+</mo><mi mathvariant="normal">exp</mi><mrow><mo>[</mo>
      <mrow><mo>−</mo><mi>γ</mi><mrow><mo>(</mo><mi>V</mi><mrow><mo>(</mo><mi>q</mi><mo>−</mo><mn>1</mn><mo>)</mo></mrow><mo>−</mo><mi>θ</mi><mo>)</mo></mrow></mrow><mo>]</mo></mrow></mrow></mfrac></mrow>'),
  P("&gamma; = 8 is the slope parameter and &theta; = .5 is the midpoint:
     a value of .5 gives a yes probability of .5."),
  h3(class = "sub", "Threshold"),
  model_equation("formula-threshold", '
    <mrow><mi>P</mi><mrow><mo>(</mo><msub><mi>Y</mi><mi>q</mi></msub><mo>=</mo><mn>1</mn><mo>)</mo></mrow><mo>=</mo>
    <mrow><mo stretchy="true" fence="true" form="prefix">{</mo><mtable columnalign="left left" rowspacing="0.4em">
      <mtr><mtd><mn>1</mn></mtd><mtd><mtext>if</mtext><mspace width="0.3em"/><mi>V</mi><mrow><mo>(</mo><mi>q</mi><mo>−</mo><mn>1</mn><mo>)</mo></mrow><mo>&gt;</mo><mi>θ</mi></mtd></mtr>
      <mtr><mtd><mn>0</mn></mtd><mtd><mtext>otherwise.</mtext></mtd></mtr>
    </mtable></mrow></mrow>'),
  P("Here &theta; = .5. A value exactly at the threshold gives a no response.
     The same rule and parameters are applied to B and D."))

experiment_formula_ui <- function() tags$details(class = "formula-details",
  tags$summary("The response model used for these records"),
  model_equation("formula-experiment-response", '
    <mrow><msub><mi>Y</mi><mrow><mi>s</mi><mo>,</mo><mi>i</mi><mo>,</mo><mi>q</mi></mrow></msub>
    <mo>∼</mo><mi mathvariant="normal">Bernoulli</mi><mrow><mo>(</mo>
    <msub><mi>p</mi><mrow><mi>s</mi><mo>,</mo><mi>i</mi><mo>,</mo><mi>q</mi></mrow></msub><mo>)</mo></mrow></mrow>'),
  model_equation("formula-experiment-probability", '
    <mrow><msub><mi>p</mi><mrow><mi>s</mi><mo>,</mo><mi>i</mi><mo>,</mo><mi>q</mi></mrow></msub><mo>=</mo>
    <mfrac><mn>1</mn><mrow><mn>1</mn><mo>+</mo><mi mathvariant="normal">exp</mi><mrow><mo>[</mo><mrow><mo>−</mo><mrow><mo>(</mo>
      <mi>b</mi><mo>+</mo><mi>γ</mi><msub><mi>V</mi><mrow><mi>s</mi><mo>,</mo><mi>i</mi></mrow></msub><mrow><mo>(</mo><mi>q</mi><mo>−</mo><mn>1</mn><mo>)</mo></mrow>
      <mo>)</mo></mrow></mrow><mo>]</mo></mrow></mrow></mfrac></mrow>'),
  P("Here <i>s</i> identifies the participant, <i>i</i> the cue and <i>q</i> the
     presentation. <i>V</i><sub>s,i</sub>(<i>q</i> &minus; 1) is that participant's value
     for the cue before feedback on presentation <i>q</i>. The model turns it into a yes
     probability, <i>p</i>, then draws a yes-or-no response. Learning rates vary across
     participants. When a candidate is fitted, the intercept <i>b</i> and the positive
     slope &gamma;, shared by B and D, are estimated from the data rather than fixed at
     the values used in Step 4 (&gamma; = 8 and &theta; = .5, so <i>b</i> = &minus;4).
     The midpoint is &minus;<i>b</i>/&gamma;."))

ddm_formulas_ui <- function() tags$details(class = "formula-details",
  tags$summary("How the diffusion model gives these predictions"),
  model_equation("formula-ddm-process", '
    <mrow><mi>d</mi><msub><mi>X</mi><mi>t</mi></msub><mo>=</mo><mi>v</mi><mi>d</mi><mi>t</mi>
    <mo>+</mo><mi>d</mi><msub><mi>W</mi><mi>t</mi></msub></mrow>'),
  P("<i>X</i><sub>t</sub> is accumulated evidence; <i>v</i> is the drift rate.
     <i>W</i><sub>t</sub> is standard Brownian noise, with noise scale fixed at 1.
     Evidence starts at <i>a</i>/2 and stops at 0 or <i>a</i>, where <i>a</i> is boundary
     separation. The boundary at <i>a</i> gives the correct answer. There is no deadline."),
  model_equation("formula-ddm-accuracy", '
    <mrow><mi>P</mi><mrow><mo>(</mo><mtext>correct</mtext><mo>)</mo></mrow><mo>=</mo>
    <mfrac><mn>1</mn><mrow><mn>1</mn><mo>+</mo><mi mathvariant="normal">exp</mi><mrow><mo>(</mo><mrow><mo>−</mo><mi>v</mi><mi>a</mi></mrow><mo>)</mo></mrow></mrow></mfrac></mrow>'),
  model_equation("formula-ddm-time", '
    <mrow><mi mathvariant="normal">E</mi><mrow><mo>[</mo><msub><mi>T</mi><mtext>decision</mtext></msub><mo>]</mo></mrow><mo>=</mo>
    <mfrac><mi>a</mi><mrow><mn>2</mn><mi>v</mi></mrow></mfrac>
    <mi mathvariant="normal">tanh</mi><mrow><mo>(</mo><mfrac><mrow><mi>v</mi><mi>a</mi></mrow><mn>2</mn></mfrac><mo>)</mo></mrow></mrow>'),
  P("The second expression gives accuracy and the third the mean decision time,
     pooling correct and incorrect answers. Decision time excludes perception and motor execution."))
