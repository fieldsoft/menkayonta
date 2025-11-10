import { app, BrowserWindow, ipcMain, Menu } from 'electron';
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

// Test if the platform is macos.
const isMac = process.platform === 'darwin'

// Handle creating/removing shortcuts on Windows when installing/uninstalling.
if (started) {
  app.quit();
}

// Determine whether the app is running in a development environment.
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

  // The application menu template
  const template = [
    // { role: 'appMenu' }
    ...(isMac
        ? [{
          label: app.name,
          submenu: [
            { role: 'about' },
            { type: 'separator' },
            { role: 'services' },
            { type: 'separator' },
            { role: 'hide' },
            { role: 'hideOthers' },
            { role: 'unhide' },
            { type: 'separator' },
            { role: 'quit' }
          ]
        }]
        : []),
    // { role: 'fileMenu' }
    {
      label: 'File',
      submenu: [
        {
          click: () => mainWindow.webContents.send('new-project'),
          label: 'New Project',
        },
        ...(isMac ? [{ role: 'close' }] : [{ role: 'quit' }])
      ]
    },
    // { role: 'editMenu' }
    {
      label: 'Edit',
      submenu: [
        { role: 'undo' },
        { role: 'redo' },
        { type: 'separator' },
        { role: 'cut' },
        { role: 'copy' },
        { role: 'paste' },
        ...(isMac
            ? [
              { role: 'pasteAndMatchStyle' },
              { role: 'delete' },
              { role: 'selectAll' },
              { type: 'separator' },
              {
                label: 'Speech',
                submenu: [
                  { role: 'startSpeaking' },
                  { role: 'stopSpeaking' }
                ]
              }
            ]
            : [
              { role: 'delete' },
              { type: 'separator' },
              { role: 'selectAll' }
            ])
      ]
    },
    // { role: 'viewMenu' }
    {
      label: 'View',
      submenu: [
        { role: 'reload' },
        { role: 'forceReload' },
        { role: 'toggleDevTools' },
        { type: 'separator' },
        { role: 'resetZoom' },
        { role: 'zoomIn' },
        { role: 'zoomOut' },
        { type: 'separator' },
        { role: 'togglefullscreen' }
      ]
    },
    // { role: 'windowMenu' }
    {
      label: 'Window',
      submenu: [
        { role: 'minimize' },
        { role: 'zoom' },
        ...(isMac
            ? [
              { type: 'separator' },
              { role: 'front' },
              { type: 'separator' },
              { role: 'window' }
            ]
            : [
              { role: 'close' }
            ])
      ]
    },
  ]

  const menu = Menu.buildFromTemplate(template)
  Menu.setApplicationMenu(menu)
  mainWindow.setMenu(menu)
  
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
// exist. It returns a JSON object of the apps global configuration,
// such as the projects that exist in the projects directory.
const openGlobalConf = async () => {
  let fh
  const initialConf = { projects: [] }

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
  if (!isMac) {
    app.quit()
  }
})

init()
