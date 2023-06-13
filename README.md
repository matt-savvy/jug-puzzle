# Jug Puzzle

You have two jugs, a three gallon jug and a five gallon jug. Measure out exactly four gallons.

1. Install dependencies
```
npm ci
```

2. Compile
```
elm make src/JugPuzzle.elm --optimize --output=dist/app.js
npm run uglify
sed 's/app.js/app.min.js/' index.html > dist/index.html
cp jugs.css dist
```

3. View index.html
```
open dist/index.html
```

Good luck.
