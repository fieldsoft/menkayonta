import { app, BrowserWindow, ipcMain } from 'electron';
import path from 'node:path';
import started from 'electron-squirrel-startup';

const PouchDB = require('pouchdb-core')
const HttpPouch = require('pouchdb-adapter-http')
const mapreduce = require('pouchdb-mapreduce')
const replication = require('pouchdb-replication')

PouchDB.plugin(HttpPouch).plugin(mapreduce).plugin(replication)

const sqliteAdapter = require('pouchdb-adapter-node-websql')

// Register the adapter
PouchDB.plugin(sqliteAdapter)

// Create a database using the SQLite3 adapter
const db = new PouchDB('mydb', { adapter: 'websql' })


const doc = {
  "_id": "mittens",
  "name": "Mittens",
  "occupation": "kitten",
  "age": 3,
  "hobbies": [
    "playing with balls of yarn",
    "chasing laser pointers",
    "lookin' hella cute"
  ]
}

db.put(doc)

// Handle creating/removing shortcuts on Windows when installing/uninstalling.
if (started) {
  app.quit();
}

let isDev = process.env.APP_DEV ? process.env.APP_DEV.trim() == 'true' : false

// if (isDev) require('electron-reload')(__dirname, {
//   electron: path.join(process.cwd(), 'node_modules', '.bin', 'electron')
// })

// Set the window title according to renderer events
const handleSetTitle = (event, title) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  win.setTitle(title)
}

const createWindow = () => {
  // Create the browser window.
  const mainWindow = new BrowserWindow({
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  // and load the index.html of the app.
  if (MAIN_WINDOW_VITE_DEV_SERVER_URL) {
    mainWindow.loadURL(MAIN_WINDOW_VITE_DEV_SERVER_URL)
  } else {
    mainWindow.loadFile(
      path.join(__dirname, `../renderer/${MAIN_WINDOW_VITE_NAME}/index.html`))
  }
}

app.whenReady().then(() => {
  ipcMain.on('set-title', handleSetTitle)
  createWindow()

  // Open a window on MacOS when none are otherwise open.
  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      createWindow()
    }
  })
})

// On MacOS, don't close the application when all windows are closed
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit()
  }
})
