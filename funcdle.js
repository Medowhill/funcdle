const size = 10;
const dx = 0.25;
const mdx = 0.01;
const delta = 0.5;
const near = '#202020';
const far = '#e0e0e0';
const nearHighlight = '#03193f';
const farHighlight = '#c3d9ff';

const correctWord = validRand().toString();
var board, points = [];
let isGameOver = false;
let guessNum = 0;
let guessWord = '';

const processKey = key => {
	if (guessWord.length < 5 && key.match(/^[x0-9\+\-\*\/\^√㏑]$/)) {
		guessWord += key;
	} else if (guessWord.length > 0 && ['Backspace', 'Delete'].indexOf(key) !== -1) {
		guessWord = guessWord.slice(0, -1);
	} else if (guessWord.length == 5 && key == 'Enter') {
		const f = parse(guessWord);
		if (f !== null) {
			guessGraph(parse(correctWord), f, guessNum);

			const guessStatuses = Array(guessWord.length).fill('wrong');
			const almostTestLetters = correctWord.split('');

			guessWord.split('').forEach((letter, index) => {
				if (letter == correctWord[index]) {
					guessStatuses[index] = 'correct';
					almostTestLetters.splice(almostTestLetters.indexOf(letter), 1);
				}
			});

			guessWord.split('').forEach((letter, index) => {
				if (guessStatuses[index] == 'wrong' && almostTestLetters.indexOf(letter) !== -1) {
					guessStatuses[index] = 'almost';
					almostTestLetters.splice(almostTestLetters.indexOf(letter), 1);
				}
			});

			guessStatuses.forEach((status, index) => {
				const gbTileNode = document.querySelectorAll('.gb-row')[guessNum].childNodes[index];
				gbTileNode.style.animationDelay = (100 * index) + 'ms';
				gbTileNode.classList.add(status);
				Array.from(document.querySelectorAll('.kb-row button')).find(n => n.textContent == guessWord[index]).classList.add(status);
			});

			if (guessStatuses.every(s => s == 'correct')) {
				alert('You won in ' + (guessNum + 1) + ' guesses! Reload for a new word.');
				isGameOver = true;
			} else if (guessNum == 5) {
				alert('You lost, the word was "' + correctWord + '". Reload for a new word.');
				isGameOver = true;
			} else {
				guessNum++;
				guessWord = '';
			}
		} else {
			document.querySelectorAll('.gb-row')[guessNum].classList.add('invalid');
			alert('Not a valid formula.');
			document.querySelectorAll('.gb-row')[guessNum].classList.remove('invalid');
		}
	}

	document.querySelectorAll('.gb-row')[guessNum].childNodes.forEach((node, index) => {
		if (guessWord[index]) {
			node.textContent = guessWord[index];
			node.classList.add('filled');
		} else {
			node.textContent = '';
			node.classList.remove('filled');
		}
	});
};

board = JXG.JSXGraph.initBoard('jxgbox', {
  boundingbox: [-size, size, size, -size],
  showCopyright: false,
  showNavigation: false,
  zoom: { enabled: false },
  pan: { enabled: false },
  drag: { enabled: false },
  axis: true,
  grid: true
});

document.querySelectorAll('.gb-row').forEach((n, idx) => { n.addEventListener('mouseenter', () => board.select({graphId: idx}).highlight()); n.addEventListener('mouseleave', () => board.select({graphId: idx}).noHighlight()); });
document.querySelectorAll('.kb-row button').forEach(n => n.addEventListener('click', e => !isGameOver && processKey(e.target.textContent)));
document.addEventListener('keydown', e => { if (e.keyCode === 191) e.preventDefault(); });
document.addEventListener('keyup', e => !isGameOver && processKey(e.key));

function validate(f) {
  const xs = (f.toString().match(/x/g) || []).length;
  if (xs === 0 || xs >= 3)
    return false;

  const ys = [];
  for (let x = -size; x <= size; x += dx) {
    const y = evaluate(f, x);
    if (!isNaN(y))
      ys.push(y);
  }

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

function draw(x, y, color, highlightColor, gid) {
  points.push(board.create('point', [x, y], {
    name: '',
    fixed: true,
    size: 1,
    strokeColor: color,
    fillColor: color,
    highlightStrokeColor: highlightColor,
    highlightFillColor: highlightColor,
    graphId: gid
  }));
}

function guessGraph(f, guess, gid) {
  let x = -size + 0.01;
  const dguess = simplify(derivative(simplify(guess)));
  while (x <= size) {
    const _y = evaluate(f, x);
    const y = evaluate(guess, x);

    if (isNaN(y) || !isFinite(y)) {
      x += dx;
    } else {
      const d = _y - y;
      const isNear = (-delta <= d && d <= delta);
      const color = isNear ? near : far;
      const highlightColor = isNear ? nearHighlight : farHighlight;
      draw(x, y, color, highlightColor, gid);

      const gpx = evaluate(dguess, x);
      const inc = dx / Math.sqrt(gpx * gpx + 1);
      if (isNaN(inc) || inc < mdx)
        x += mdx;
      else
        x += inc;
    }
  }
}
