#!/usr/bin/env node
//
// Preprocesses a word list into the anagram-finder dictionary at
// `public/dict.json`.
//
// Default behaviour: download UKACD from UKACD_URL, cache to scripts/ukacd.txt,
// preprocess into public/dict.json.
//
// You can pass an explicit local path as the first arg to skip the download
// (handy for testing with a smaller wordlist):
//     node scripts/build-dict.mjs scripts/wordlist-mini.txt
//
// One entry per line. Comments (#) and blank lines are skipped. Phrases with
// spaces keep the spaces in the displayed value; the index key strips them.
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
const outputPath = resolve(repoRoot, "public/dict.json");

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
  const letters = line.toLowerCase().replace(/[^a-z]/g, "");
  if (letters.length < MIN_LETTERS || letters.length > MAX_LETTERS) continue;

  const key = letters.split("").sort().join("");
  const bucket = index.get(key);
  if (bucket) {
    if (!bucket.includes(display)) bucket.push(display);
  } else {
    index.set(key, [display]);
  }
  entriesIndexed += 1;
}

// Stable output: sort keys and each value alphabetically so the file diffs cleanly.
const sortedKeys = [...index.keys()].sort();
const out = {};
for (const k of sortedKeys) {
  out[k] = index.get(k).sort();
}

await writeFile(outputPath, JSON.stringify(out));

console.log(
  `Read ${entriesIn} entries, indexed ${entriesIndexed} into ${sortedKeys.length} buckets.`,
);
console.log(`Wrote ${outputPath}`);
