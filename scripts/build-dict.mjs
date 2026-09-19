#!/usr/bin/env node
//
// Preprocesses a word list into the anagram-finder dictionary at
// `public/dict.txt`.
//
// Default behaviour: download UKACD from UKACD_URL, cache to scripts/ukacd.txt,
// preprocess into public/dict.txt.
//
// You can pass an explicit local path as the first arg to skip the download
// (handy for testing with a smaller wordlist):
//     node scripts/build-dict.mjs scripts/wordlist-mini.txt
//
// Output is one word per line, sorted case-insensitively. Nothing else: every
// anagram key is a sorted permutation of the word it indexes, so shipping keys
// as well would ship each word twice. The client derives them on load. Sorting
// alphabetically rather than by key also gives gzip far more to work with —
// neighbouring words share prefixes — which is most of why the file is a third
// of the size of the equivalent JSON.
//
// One entry per line in the source. Comments (#) and blank lines are skipped.
// Phrases with spaces keep the spaces in the displayed value; the index key
// strips them.
// Entries < 3 letters are dropped (they bloat multi-word search), as are
// entries > 15 letters: the modal caps input at 15, so a longer key can never
// be a subset of any target. UKACD holds whole quotations at the top end.
//
// UKACD license: 3-clause BSD-style (Beresford, 2009). See LICENSE-UKACD.
// The source file begins with the license header, followed by a `---` divider,
// then one word per line; we skip lines until we pass the divider.

import { readFile, writeFile, mkdir } from "node:fs/promises";
import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(here, "..");

const UKACD_URL =
  "https://github.com/rdeits/cryptics/raw/refs/heads/master/raw_data/UKACD.txt";
const defaultCachePath = resolve(repoRoot, "scripts/ukacd.txt");
const outputPath = resolve(repoRoot, "public/dict.txt");

const MIN_LETTERS = 3;
const MAX_LETTERS = 15;

let raw;
const explicitPath = process.argv[2];
if (explicitPath) {
  const p = resolve(repoRoot, explicitPath);
  raw = await readFile(p, "utf8");
  console.log(`Read source: ${p}`);
} else if (existsSync(defaultCachePath)) {
  raw = await readFile(defaultCachePath, "utf8");
  console.log(`Read cached source: ${defaultCachePath}`);
} else {
  console.log(`Downloading UKACD from ${UKACD_URL} …`);
  const res = await fetch(UKACD_URL);
  if (!res.ok) {
    console.error(`Download failed: HTTP ${res.status}`);
    process.exit(1);
  }
  raw = await res.text();
  await mkdir(dirname(defaultCachePath), { recursive: true });
  await writeFile(defaultCachePath, raw);
  console.log(`Cached source to ${defaultCachePath}`);
}

const index = new Map();

const lettersOf = (text) => text.toLowerCase().replace(/[^a-z]/g, "");
const keyOf = (text) => lettersOf(text).split("").sort().join("");

let entriesIn = 0;
let entriesIndexed = 0;

let inHeader = true;
for (const rawLine of raw.split(/\r?\n/)) {
  const line = rawLine.trim();

  if (inHeader) {
    // The divider is a long run of dashes (>= 10) on its own line.
    if (/^-{10,}$/.test(line)) inHeader = false;
    continue;
  }

  if (!line || line.startsWith("#")) continue;
  entriesIn += 1;

  // Skip entries containing the Unicode replacement character — UKACD's source
  // has ~1300 mojibaked accented borrowings (abbé, à bientôt, etc.) that would
  // render as "abb�" in the UI.
  if (line.includes("�")) continue;

  const display = line;
  const letters = lettersOf(line);
  if (letters.length < MIN_LETTERS || letters.length > MAX_LETTERS) continue;

  const key = keyOf(letters);
  const bucket = index.get(key);
  if (bucket) {
    if (!bucket.includes(display)) bucket.push(display);
  } else {
    index.set(key, [display]);
  }
  entriesIndexed += 1;
}

// Drop phrases the search can already build from their parts. "close company"
// is redundant — "close" and "company" are both indexed, so a two-word search
// finds it anyway — while "vitamin C" is not, because "C" is too short to be
// indexed at all. Redundant phrases are worse than useless: under an
// enumeration they masquerade as single words, so "(8,7)" offers "all the go"
// as its eight-letter word.
let phrasesDropped = 0;
for (const [key, entries] of index) {
  const kept = entries.filter((entry) => !isReconstructible(entry));
  phrasesDropped += entries.length - kept.length;
  if (kept.length === 0) index.delete(key);
  else if (kept.length !== entries.length) index.set(key, kept);
}

function isReconstructible(entry) {
  if (!entry.includes(" ")) return false;
  return entry
    .split(/\s+/)
    .filter((part) => /[a-z]/i.test(part))
    .every((part) => index.has(keyOf(part)));
}

// Stable output: one word per line, sorted case-insensitively so the file diffs
// cleanly and gzip sees neighbouring words sharing prefixes.
const collator = new Intl.Collator("en", { sensitivity: "base" });
const out = [...index.values()]
  .flat()
  .sort((a, b) => collator.compare(a, b) || (a < b ? -1 : a > b ? 1 : 0));

await writeFile(outputPath, out.join("\n"));

console.log(
  `Read ${entriesIn} entries, indexed ${out.length} words under ${index.size} keys.`,
);
console.log(`Dropped ${phrasesDropped} phrases the search can build from their parts.`);
console.log(`Wrote ${outputPath}`);
