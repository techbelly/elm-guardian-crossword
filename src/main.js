import { Elm } from "./Main.elm";
import "./style.css";

const form = document.getElementById("crossword-form");
const input = document.getElementById("crossword-path");
const loader = document.getElementById("loader");
const appEl = document.getElementById("app");

form.addEventListener("submit", async (e) => {
  e.preventDefault();
  const path = input.value.trim();
  if (!path) return;

  try {
    const puzzleJson = await fetchCrossword(path);
    loader.style.display = "none";
    initElm(puzzleJson);
  } catch (err) {
    alert("Failed to load crossword: " + err.message);
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

function initElm(puzzleJson) {
  const puzzleId = puzzleJson.id;
  const savedGridRaw = localStorage.getItem("crosswords." + puzzleId);
  const savedGrid = savedGridRaw ? JSON.parse(savedGridRaw) : null;

  const app = Elm.Main.init({
    node: appEl,
    flags: {
      puzzle: puzzleJson,
      savedGrid: savedGrid,
    },
  });

  app.ports.saveGrid.subscribe(function (gridData) {
    localStorage.setItem("crosswords." + puzzleId, JSON.stringify(gridData));
  });

  app.ports.scrollIntoView.subscribe(function (id) {
    requestAnimationFrame(function () {
      var el = document.getElementById(id);
      if (el) el.scrollIntoView({ block: "nearest", behavior: "smooth" });
    });
  });

  let dictPromise = null;
  app.ports.loadDictionary.subscribe(function () {
    if (!dictPromise) {
      dictPromise = fetch("/dict.json")
        .then(function (res) {
          if (!res.ok) throw new Error("HTTP " + res.status);
          return res.json();
        });
    }
    dictPromise
      .then(function (data) { app.ports.dictionaryLoaded.send(data); })
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
}

function nearestClue(node) {
  let el = node.nodeType === 1 ? node : node.parentElement;
  while (el) {
    if (el.classList && el.classList.contains("crossword__clue")) return el;
    el = el.parentElement;
  }
  return null;
}
