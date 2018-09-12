if (process.argv.length !== 3) {
  console.log("USAGE: node index.js Module.hs");
  process.exit();
}

const passes = [
  hs => hs.replace(/^module (\S+) where/m, (match, name) => `module ${name} exposing (..) where`),
  hs => hs.replace(/^module (\S+)/m, (match, name) => `module ${name} exposing`),
  hs => hs.replace(/where/, ""),

  hs => hs.replace(/^import qualified/gm, "import"),
  hs => hs.replace(/^import (\S+)\s+\(/gm, (match, name) => `import ${name} exposing (`),

  hs => hs.replace(/::/g, "TMP_HAS_TYPE"),
  hs => hs.replace(/:/g, "TMP_CONS"),
  hs => hs.replace(/TMP_HAS_TYPE/g, ":"),
  hs => hs.replace(/TMP_CONS/g, "::"),

  hs => hs.replace(/^type/gm, "type alias"),
  hs => hs.replace(/^data/gm, "type"),
  hs => hs.replace(/^newtype/gm, "type"),

  hs => hs.replace(/ (:|->) \[([^\]]+)\]/g, (match, hint, list) => ` ${hint} (List (${list}))`),
];

const fs = require("fs");
const file = process.argv[2];
fs.readFile(file, 'utf8', (error, input) => {
  const output = passes.reduce((accum, next) => next(accum), input);
  fs.writeFile(file.replace(/\.hs$/, ".elm"), output, () => {});
});
