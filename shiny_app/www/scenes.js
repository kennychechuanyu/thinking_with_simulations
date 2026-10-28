// Moving between the steps of the inquiry, sections that start closed on
// narrow screens, and saving the notes.
document.addEventListener('DOMContentLoaded', function () {
  // The flags that complete Steps 1 to 7, which app.R sends as scene_done.
  var milestones = ['done_q1c', 'done_q2d', 'done_q3e', 'done_q4c', 'done_e4', 'done_f5', 'done_x3'];
  var completed = {};
  var currentStep = 1;
  var stepButtons = document.querySelectorAll('[data-step-target]');
  // A step's button is enabled once every earlier step is complete.
  function updateProgress() {
    var available = 1;
    while (available <= milestones.length && completed[milestones[available - 1]]) available++;
    currentStep = Math.min(currentStep, available);
    stepButtons.forEach(function (button) {
      var step = Number(button.dataset.stepTarget);
      button.disabled = step > available;
      button.classList.toggle('is-complete', step < available);
      if (step === currentStep) button.setAttribute('aria-current', 'step');
      else button.removeAttribute('aria-current');
      button.setAttribute('aria-label', 'Step ' + step + ': ' + button.lastElementChild.textContent +
        (step < available ? ', completed' : step > available ? ', available after the previous step' : ''));
    });
  }
  function goToStep(next, attempts) {
    if (!next) return;
    // A step takes the focus at its summary. Safari leaves everything inside a
    // details element out of the Tab order once the element has a tabindex.
    var target = next.tagName === 'DETAILS' ? next.querySelector('summary') : next.querySelector('h2') || next;
    // The completion message can arrive just before Shiny reveals the next panel.
    if (!target.getClientRects().length) {
      if ((attempts || 0) < 20) setTimeout(function () { goToStep(next, (attempts || 0) + 1); }, 50);
      return;
    }
    document.querySelectorAll('.inquiry-scene[data-complete="true"]').forEach(function (scene) {
      if (scene !== next) scene.open = false;
    });
    if (next.tagName === 'DETAILS') next.open = true;
    currentStep = Number(next.id.replace('scene-', ''));
    updateProgress();
    if (target.tagName !== 'SUMMARY') target.setAttribute('tabindex', '-1');
    target.focus({ preventScroll: true });
    target.scrollIntoView({ block: 'start' });
    window.dispatchEvent(new Event('resize'));
  }
  stepButtons.forEach(function (button) {
    button.addEventListener('click', function () {
      goToStep(document.getElementById('scene-' + button.dataset.stepTarget));
    });
  });
  updateProgress();
  Shiny.addCustomMessageHandler('scene_done', function (message) {
    completed[message.name] = message.done;
    updateProgress();
    var scene = document.querySelector('[data-done="' + message.name + '"]');
    if (!scene) return;
    scene.dataset.complete = String(message.done);
    if (!message.done) scene.open = true;
  });
  document.querySelectorAll('.inquiry-scene').forEach(function (scene) {
    scene.querySelector('summary').addEventListener('click', function () {
      // Native details toggle after the click; follow a reader who reopens a step.
      setTimeout(function () {
        if (scene.open) {
          currentStep = Number(scene.id.replace('scene-', ''));
          updateProgress();
        }
      }, 0);
    });
    scene.addEventListener('toggle', function () {
      scene.querySelector('.scene-review').textContent = scene.open ? 'Hide details' : 'Review details';
      // Plots need their visible width after a reader reopens an earlier scene.
      if (scene.open) window.dispatchEvent(new Event('resize'));
    });
  });
  document.querySelectorAll('.scene-continue').forEach(function (button) {
    button.addEventListener('click', function () {
      goToStep(document.getElementById(button.dataset.next));
    });
  });
  var notes = document.getElementById('notes_details');
  if (window.matchMedia('(max-width: 960px)').matches) notes.open = false;
  document.querySelector('.notes-jump').addEventListener('click', function () { notes.open = true; });
  if (window.matchMedia('(max-width: 720px)').matches) {
    document.querySelectorAll('[data-mobile-collapse]').forEach(function (group) { group.open = false; });
  }
  // The notes are fetched and then saved from the page. In the version of the
  // app that runs in the browser (made with shinylive), a download request from
  // the link itself does not reach the app, but a fetch does.
  document.getElementById('notes_download').addEventListener('click', function (event) {
    var link = this;
    if (!link.getAttribute('href')) return;
    event.preventDefault();
    fetch(link.href).then(function (response) {
      var name = /filename="([^"]+)"/.exec(response.headers.get('Content-Disposition') || '');
      return response.blob().then(function (blob) {
        var save = document.createElement('a');
        save.href = URL.createObjectURL(blob);
        save.download = name ? name[1] : 'notes.txt';
        document.body.appendChild(save);
        save.click();
        save.remove();
        setTimeout(function () { URL.revokeObjectURL(save.href); }, 1000);
      });
    });
  });
});
