@echo off
setlocal
  set "NODE_ENV=development" && npx run-pty run-pty-dev.json
endlocal
