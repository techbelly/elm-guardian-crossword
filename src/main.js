import { Elm } from "./Main.elm";
import "./style.css";

const HISTORY_KEY = "crosswords.index";

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: {
    history: readHistory(),
    now: Date.now(),
  },
});

// Fetching a puzzle needs DOMParser, so it stays here: Elm asks for a path and
// gets back the Guardian's own puzzle JSON plus whatever grid we saved for it.
app.ports.loadPuzzle.subscribe(async function (path) {
  try {
    const puzzle = await fetchCrossword(path);
    const savedGridRaw = localStorage.getItem("crosswords." + puzzle.id);
    app.ports.puzzleLoaded.send({
      puzzle: puzzle,
      savedGrid: savedGridRaw ? JSON.parse(savedGridRaw) : null,
    });
  } catch (err) {
    app.ports.puzzleLoadFailed.send(err.message || String(err));
  }
});

async function fetchCrossword(path) {
  const res = await fetch(`/guardian/crosswords/${path}`);
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  const html = await res.text();

  const parser = new DOMParser();
  const doc = parser.parseFromString(html, "text/html");
  const island = doc.querySelector('gu-island[name="CrosswordComponent"]');
  if (!island) throw new Error("CrosswordComponent not found on page");

  const props = JSON.parse(island.getAttribute("props"));
  return props.data;
}

app.ports.saveGrid.subscribe(function (gridData) {
  const puzzleId = gridData.puzzleId;
  localStorage.setItem("crosswords." + puzzleId, JSON.stringify(gridData.cells));
});

// One record per puzzle, keyed by path, newest write wins.
app.ports.saveHistoryEntry.subscribe(function (entry) {
  const history = readHistory().filter((e) => e && e.path !== entry.path);
  history.push(entry);
  try {
    localStorage.setItem(HISTORY_KEY, JSON.stringify(history));
  } catch (err) {
    // A full quota shouldn't cost you the puzzle you're solving.
    console.warn("Could not save crossword history:", err);
  }
});

function readHistory() {
  try {
    const raw = localStorage.getItem(HISTORY_KEY);
    const parsed = raw ? JSON.parse(raw) : [];
    return Array.isArray(parsed) ? parsed : [];
  } catch (err) {
    return [];
  }
}

// Bring the selected clue into view by scrolling its own list only. The native
// scrollIntoView also scrolls every ancestor, which on a phone drags the grid
// off the screen every time the selection moves.
app.ports.scrollIntoView.subscribe(function (id) {
  requestAnimationFrame(function () {
    const el = document.getElementById(id);
    if (!el) return;
    const panel = scrollingParent(el);
    if (!panel) return;

    const item = el.getBoundingClientRect();
    const box = panel.getBoundingClientRect();
    const header = panel.querySelector(".crossword__clues-header");
    const headroom = header ? header.offsetHeight : 0;

    if (item.top < box.top + headroom) {
      panel.scrollTop += item.top - box.top - headroom;
    } else if (item.bottom > box.bottom) {
      panel.scrollTop += item.bottom - box.bottom;
    }
  });
});

function scrollingParent(el) {
  let node = el.parentElement;
  while (node && node !== document.body) {
    const overflow = getComputedStyle(node).overflowY;
    if ((overflow === "auto" || overflow === "scroll") && node.scrollHeight > node.clientHeight) {
      return node;
    }
    node = node.parentElement;
  }
  return null;
}

// Typing goes through an offscreen input, and a phone only raises its keyboard
// for a focus() made inside the gesture itself — which Elm's Browser.Dom.focus,
// deferred to an animation frame, is not. So tapping a square or a clue focuses
// it here, straight from the event.
document.addEventListener("click", function (event) {
  const target = event.target;
  if (!target || !target.closest) return;
  if (!target.closest("#crossword")) return;
  if (target.closest(".anagram-modal")) return;
  if (!target.closest("svg") && !target.closest(".crossword__clue")) return;

  const letterInput = document.getElementById("crossword-letter-input");
  if (letterInput) letterInput.focus({ preventScroll: true });
});

// Elm has read the characters by the time this bubbles up; emptying the input
// again keeps the next keystroke a single letter.
document.addEventListener("input", function (event) {
  if (event.target && event.target.id === "crossword-letter-input") {
    event.target.value = "";
  }
});

let dictPromise = null;
app.ports.loadDictionary.subscribe(function () {
  if (!dictPromise) {
    // The dictionary ships as a bare word list; Elm derives the anagram keys.
    dictPromise = fetch("/dict.txt").then(function (res) {
      if (!res.ok) throw new Error("HTTP " + res.status);
      return res.text();
    });
  }
  dictPromise
    .then(function (data) {
      app.ports.dictionaryLoaded.send(data);
    })
    .catch(function (err) {
      dictPromise = null;
      app.ports.dictionaryLoadFailed.send(err.message || String(err));
    });
});

// Track text selection within clue elements and push to Elm. The selection
// is reported as empty when nothing is selected, or when the selection moves
// outside a clue.
document.addEventListener("selectionchange", function () {
  const sel = window.getSelection();
  if (!sel || sel.isCollapsed || sel.rangeCount === 0) {
    app.ports.clueSelectionChanged.send("");
    return;
  }
  const anchor = sel.anchorNode;
  const focus = sel.focusNode;
  if (!anchor || !focus) {
    app.ports.clueSelectionChanged.send("");
    return;
  }
  const anchorClue = nearestClue(anchor);
  const focusClue = nearestClue(focus);
  if (anchorClue && anchorClue === focusClue) {
    app.ports.clueSelectionChanged.send(sel.toString());
  } else {
    app.ports.clueSelectionChanged.send("");
  }
});

function nearestClue(node) {
  let el = node.nodeType === 1 ? node : node.parentElement;
  while (el) {
    if (el.classList && el.classList.contains("crossword__clue")) return el;
    el = el.parentElement;
  }
  return null;
}
