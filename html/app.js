const { app, BrowserWindow } = require('electron')
const path = require('path')

let isDev = process.env.APP_DEV ? process.env.APP_DEV.trim() == 'true' : false

app.on('window-all-closed', () => app.quit())

if (isDev) require("electron-reload")(__dirname, {
  electron: path.join(process.cwd(), 'node_modules', '.bin', 'electron')
})

app.whenReady().then(() => {
  let mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    frame: true,
    resizable: true,
  })
  mainWindow.loadURL(`file://${__dirname}/index.html`);
  mainWindow.on('closed', () => {
    mainWindow = null;
  })
})
