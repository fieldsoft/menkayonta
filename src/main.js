import { app, BrowserWindow, ipcMain } from 'electron';
import path from 'node:path';
import fs from 'node:fs/promises';
import os from 'node:os';
import started from 'electron-squirrel-startup';

// Load the pouchdb packages. The pouchdb packages need commonjs
// imports.
const PouchDB = require('pouchdb-core')
const HttpPouch = require('pouchdb-adapter-http')
const mapreduce = require('pouchdb-mapreduce')
const replication = require('pouchdb-replication')
const sqliteAdapter = require('pouchdb-adapter-node-websql')

// Set the pouchdb plugins.
PouchDB.plugin(HttpPouch).plugin(mapreduce).plugin(replication).plugin(sqliteAdapter)


// Create a database using the SQLite3 adapter
//const db = new PouchDB('mydb', { adapter: 'websql' })


// const doc = {
//   "_id": "mittens",
//   "name": "Mittens",
//   "occupation": "kitten",
//   "age": 3,
//   "hobbies": [
//     "playing with balls of yarn",
//     "chasing laser pointers",
//     "lookin' hella cute"
//   ]
// }

// db.put(doc)

// These are values relavent to the global configuration of the
// application.
const projectsPath = path.join(os.homedir(), 'Menkayonta')
const globalConfPath = path.join(projectsPath, 'config')
const initialConf = { projects: [] }


// Handle creating/removing shortcuts on Windows when installing/uninstalling.
if (started) {
  app.quit();
}

let isDev = process.env.APP_DEV ? process.env.APP_DEV.trim() == 'true' : false

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

// The openConf() function will create the projects directory and
// global config file in the user's home directory if they do not
// exist.
const openGlobalConf = async () => {
  let fh

  try {
    await fs.mkdir(projectsPath, { recursive: true })
    const confstats = await fs.state(globalConfPath)

    fh = await fs.open(globalConfPath, 'r')
    const configData = await fh.readFile({ encoding: 'utf8' })
        
    return JSON.stringify(JSON.parse(configData))
  } catch (err) {
    fh = await fs.open(globalConfPath, 'w+')
    await fh.writeFile(JSON.stringify(initialConf, null, 4))

    return JSON.stringify(initialConf)
  } finally {
    if (fh) {
      await fh.close()
    }
  }
}

const init = async () => {
  try {
    await app.whenReady()

    ipcMain.handle('request-gconfig', openGlobalConf)
    ipcMain.on('set-title', handleSetTitle)
    createWindow()

    // Open a window on MacOS when none are otherwise open.
    app.on('activate', () => {
      if (BrowserWindow.getAllWindows().length === 0) {
        createWindow()
      }
    })
  } catch (err) {
    console.log(err)
  }
}

// On MacOS, don't close the application when all windows are closed
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

init()
