const { app, BrowserWindow } = require('electron')
const path = require('path')

let isDev = process.env.APP_DEV ? process.env.APP_DEV.trim() == 'true' : false

if (isDev) require("electron-reload")(__dirname, {
  electron: path.join(process.cwd(), 'node_modules', '.bin', 'electron')
})

const createWindow = () => {
  const mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    frame: true,
    resizable: true,
  })

  mainWindow.loadURL(`file://${__dirname}/index.html`)
}

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit()
})

app.whenReady().then(() => {
  createWindow()

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})
