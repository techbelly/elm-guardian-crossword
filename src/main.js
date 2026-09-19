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

app.ports.scrollIntoView.subscribe(function (id) {
  requestAnimationFrame(function () {
    var el = document.getElementById(id);
    if (el) el.scrollIntoView({ block: "nearest", behavior: "smooth" });
  });
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
