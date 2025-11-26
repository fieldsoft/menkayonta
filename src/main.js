import { app,
         BrowserWindow,
         ipcMain,
         Menu,
         utilityProcess,
         MessageChannelMain
       } from 'electron'
import path from 'node:path'
import fs from 'node:fs/promises'
import os from 'node:os'
import started from 'electron-squirrel-startup'
import { v4 } from 'uuid'

// Load the pouchdb packages. The pouchdb packages need commonjs
// imports.
const PouchDB = require('pouchdb-core')
const HttpPouch = require('pouchdb-adapter-http')
const mapreduce = require('pouchdb-mapreduce')
const replication = require('pouchdb-replication')
const sqliteAdapter = require('pouchdb-adapter-node-websql')

// Set the pouchdb plugins.
PouchDB
  .plugin(HttpPouch)
  .plugin(mapreduce)
  .plugin(replication)
  .plugin(sqliteAdapter)

// Global variables.
let gvs =
    { projectsPath: path.join(os.homedir(), 'Menkayonta'),
      globalConfPath: path.join(os.homedir(), 'Menkayonta', 'config.json'),
      globalConf: null,
      active: [],
    }

// Test if the platform is macos.
const isMac = process.platform === 'darwin'

// Handle creating/removing shortcuts on Windows when
// installing/uninstalling.
if (started) {
  app.quit();
}

// Determine whether the app is running in a development environment.
const isDev =
    process.env.APP_DEV ? process.env.APP_DEV.trim() == 'true' : false

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
          click: () => mainWindow.webContents.send('new-project', v4()),
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
      path.join(__dirname,
                `../renderer/${MAIN_WINDOW_VITE_NAME}/index.html`))
  }
}

const readGlobalConf = async () => {
  let fh

  try {
    fh = await fs.open(gvs.globalConfPath, 'r')
    const confData = await fh.readFile({ encoding: 'utf8' })

    gvs.globalConf = JSON.parse(confData)

    return gvs.globalConf
  } finally {
    if (fh) {
      await fh.close()
    }
  }
}

const writeGlobalConf = async (configData) => {
  let fh

  try {
    fh = await fs.open(gvs.globalConfPath, 'w+')
    await fh.writeFile(JSON.stringify(configData, null, 4))

    gvs.globalConf = configData

    return gvs.globalConf
  } finally {
    if (fh) {
      await fh.close()
    }
  }
}

const writeProjConf = async (identifier, configData) => {
  const projPath = path.join(gvs.projectsPath, identifier)
  const projConfPath = path.join(projPath, 'config.json')
  let fh

  try {
    fh = await fs.open(projConfPath, 'w+')
    await fh.writeFile(JSON.stringify(configData, null, 4))

    return configData
  } finally {
    if (fh) {
      await fh.close()
    }
  }
}

// The openConf() function will create the projects directory and
// global config file in the user's home directory if they do not
// exist. It returns a JSON object of the apps global configuration,
// such as the projects that exist in the projects directory.
const openGlobalConf = async () => {
  const initialConf = { projects: [] }

  try {
    await fs.mkdir(gvs.projectsPath, { recursive: true })

    await readGlobalConf()
        
    return gvs.globalConf
  } catch (err) {
    if (err.code == 'ENOENT') {
      console.log('Initializing global configuration')
      await writeGlobalConf(initialConf)

      return gvs.globalConf
    } else {
      throw err
    }
  }
}

// Create a new project and return updated configuration.
// This requires the following:
// 1. Retrieve the current global config
// 2. Check that the new project id is not already present
// 3. Create the new project directory structure
// 4. Create the new project config file
// 5. Initialize a database
// 6. Send the globabl config to the renderer
const createProject = async (_event, projectInfo) => {
  const projPath = path.join(gvs.projectsPath, projectInfo.identifier)
  const projSharePath = path.join(projPath, 'share')
  const projConfPath = path.join(projPath, 'config.json')
  const projDBPath = path.join(projPath, `${projectInfo.identifier}.sql`)
  const equalsCurrent = (p) => p.identifier === projectInfo.identifier
  const initialConf = {}
  
  await readGlobalConf()

  if (gvs.globalConf.projects.some(equalsCurrent)) {
    throw new Error('Project ID exists')
  }
  
  await fs.mkdir(projSharePath, { recursive: true })
  await writeProjConf(projectInfo.identifier, initialConf)

  // Simply create the database at this point.
  // The user will need to load it to begin.
  const db = new PouchDB(projDBPath, { adapter: 'websql' })
  await db.close()

  gvs.globalConf.projects.push(projectInfo)
  await writeGlobalConf(gvs.globalConf)
    
  // Testing utility processes
  const { port1, port2 } = new MessageChannelMain()
  gvs.active[projectInfo.identifier] = utilityProcess.fork(path.join(__dirname, './forker.js'))
    
  gvs.active[projectInfo.identifier].postMessage({ message: 'hello' }, [port1])
    
  port2.on('message', (e) => {
    console.log(`Message from ${projectInfo.identifier}: ${e.data}`)
  })
  port2.start()
  port2.postMessage(projectInfo.identifier)

  return gvs.globalConf
}

const init = async () => {
  try {
    await app.whenReady()

    ipcMain.handle('request-gconfig', openGlobalConf)
    ipcMain.handle('create-project', createProject)
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
