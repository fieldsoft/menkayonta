@echo off
setlocal
  set "NODE_ENV=production" && npx elm-watch make --optimize && npx elm make --optimize src-elm/Converter.elm --output=src/conv.js && npx @electron/packager .
endlocal
