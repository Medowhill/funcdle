const size = 10;
const dx = 0.25;
const mdx = 0.01;
const delta = 0.5;
const near = '#202020';
const far = '#e0e0e0';

var board, answer, points = [];

function validate(f) {
  const xs = (f.toString().match(/x/g) || []).length;
  if (xs === 0 || xs >= 3)
    return false;

  const ys = [];
  for (let x = -size; x <= size; x += dx)
    ys.push(evaluate(f, x));

  if (ys.filter(y => -size <= y && y <= size).length < (size / dx))
    return false;

  if (Math.max(...ys) - Math.min(...ys) <= 0.5)
    return false;

  return true;
}

function validRand() {
  let f = rand();
  while (!validate(f)) { f = rand(); }
  return f;
}

function clear() {
  board.removeObject(points);
  points = [];
}

function draw(x, y, color) {
  points.push(board.create('point', [x, y], {
    name: '',
    fixed: true,
    size: 1,
    strokeColor: color,
    fillColor: color
  }));
}

function guessGraph(f, guess) {
  let x = -size;
  while (x <= size) {
    const _y = evaluate(f, x);
    const y = evaluate(guess, x);

    const d = _y - y;
    const color = (-delta <= d && d <= delta) ? near : far;
    draw(x, y, color);

    if (isNaN(y)) {
      x += mdx;
    } else {
      const gpx = evaluate(derivative(guess), x);
      const inc = dx / Math.sqrt(gpx * gpx + 1);
      if (isNaN(inc) || inc < mdx)
        x += mdx;
      else
        x += inc;
    }
  }
}

function guessColors(word, guess) { 
  let colors = new Array(guess.length);

  let unmatched = {};
  for (let i = 0; i < word.length; i++) {
      let letter = word[i];
      if (letter === guess[i]) {
          colors[i] = 'green';
      } else {
          unmatched[letter] = (unmatched[letter] || 0) + 1;
      }
  }

  for (let i = 0; i < guess.length; i++) {
      let letter = guess[i];
      if (letter !== word[i]) {
          if (unmatched[letter]) {
              colors[i] = 'yellow';
              unmatched[letter]--;
          } else {
              colors[i] = 'gray';
          }
      }
  }

  return colors;
}

function run() {
  const s = $('#input').val();
  if (s.length !== 5) {
    alert('length != 5');
    return;
  }

  const f = parse(s);
  if (f === null) {
    alert('cannot parse');
    return;
  }

  guessGraph(answer, f);

  const g = guessColors(answer.toString(), s);
  const tds = _.zip(s.split(''), g).map(x => `<td class="${x[1]}">${x[0]}</td>`).join('');
  const pf = `<td class="pretty">${pretty(f)}</td>`
  $('#div-trial').append(`<table><tr>${tds}${pf}</tr></table>`);

  $('#input').val('');
}


$(() => {
  board = JXG.JSXGraph.initBoard('jxgbox', {
    boundingbox: [-size, size, size, -size],
    showCopyright: false,
    showNavigation: false,
    axis: true,
    grid: true
  });

  answer = validRand();
});
