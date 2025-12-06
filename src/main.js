/* global MAIN_WINDOW_VITE_DEV_SERVER_URL, MAIN_WINDOW_VITE_NAME */
import {
  app,
  BrowserWindow,
  ipcMain,
  Menu,
  utilityProcess,
  dialog,
} from 'electron'
import path from 'node:path'
import fs from 'node:fs/promises'
import os from 'node:os'
import started from 'electron-squirrel-startup'
import { v4 } from 'uuid'

// Global variables.
let gvs = {
  projectsPath: path.join(os.homedir(), 'Menkayonta'),
  globalConfPath: path.join(os.homedir(), 'Menkayonta', 'config.json'),
  globalConf: null,
  active: [],
}

// Test if the platform is macos.
const isMac = process.platform === 'darwin'

// Handle creating/removing shortcuts on Windows when
// installing/uninstalling.
if (started) {
  app.quit()
}

// Set the window title according to renderer events
const handleSetTitle = (event, title) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  win.setTitle(title)
}

// Handle messages from the project processes.
const handleProjectMessage = (m) => {
  switch (m.command) {
    case 'info':
      console.log(`${m.identifier}: ${m.message}`)
      break
    case 'error':
      m.error.message = `Child ${m.identifier}: ${m.error.message}`
      throw m.error
    case 'received-trans-index':
      gvs.webContents.send(m.command, m)
      break
    case 'received-interlinear-index':
      gvs.webContents.send(m.command, m)
      break
    default:
      console.log(`${m.identifier} command: ${m.command}`)
  }
}

// Ensure that enabled projects are running and disabled processes are
// stopped.
const manageProjectProcesses = () => {
  gvs.globalConf.projects.forEach((p) => {
    if (!p.enabled && p.identifier in gvs.active) {
      if (gvs.active[p.identifier].kill()) {
        delete gvs.active[p.identifier]
      }
    } else if (p.enabled && !(p.identifier in gvs.active)) {
      const initMessage = {
        command: 'init',
        identifier: p.identifier,
        projectsPath: gvs.projectsPath,
        url: p.url,
      }

      gvs.active[p.identifier] = utilityProcess.fork(
        path.join(__dirname, './project.js'),
      )
      gvs.active[p.identifier].postMessage(initMessage)
      gvs.active[p.identifier].on('message', handleProjectMessage)
    }
  })
}

// Open a file dialog and return an array of file paths.
const fileDialogOpen = async () => {
  const { canceled, filePaths } = await dialog.showOpenDialog()
  if (!canceled) {
    return filePaths
  } else {
    return []
  }
}

const createWindow = () => {
  // Create the browser window.
  const mainWindow = new BrowserWindow({
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
    },
  })

  // The application menu template
  const mainMenu = [
    // { role: 'appMenu' }
    ...(isMac
      ? [
          {
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
              { role: 'quit' },
            ],
          },
        ]
      : []),
    // { role: 'fileMenu' }
    {
      label: 'File',
      submenu: [
        {
          click: () => mainWindow.webContents.send('new-project', v4()),
          label: 'New Project',
        },
        {
          click: async () => {
            const filepaths = await fileDialogOpen()

            if (filepaths.length > 0) {
              // Handled by onImportOptions in renderer.js and the
              // importOptions port in Main.elm. Elm will use the
              // importFile with an importOptions object after the
              // user has filled out a form with information about the
              // import. This calls readImportFile and the import-file
              // event, which is handled by importFile, below.
              mainWindow.webContents.send('import-options', filepaths[0])
            }
          },
          label: 'Import File',
        },
        {
          click: () =>
            mainWindow.webContents.send('global-settings', gvs.globalConf),
          label: 'Settings',
        },
        ...(isMac ? [{ role: 'close' }] : [{ role: 'quit' }]),
      ],
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
                submenu: [{ role: 'startSpeaking' }, { role: 'stopSpeaking' }],
              },
            ]
          : [{ role: 'delete' }, { type: 'separator' }, { role: 'selectAll' }]),
      ],
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
        { role: 'togglefullscreen' },
      ],
    },
    {
      label: 'Tab',
      submenu: [
        {
          click: () => mainWindow.webContents.send('move-left'),
          label: 'Move Left',
        },
        {
          click: () => mainWindow.webContents.send('move-right'),
          label: 'Move Right',
        },
        {
          click: () => mainWindow.webContents.send('move-up'),
          label: 'Move Up',
        },
        {
          click: () => mainWindow.webContents.send('move-down'),
          label: 'Move Down',
        },
        {
          click: () => mainWindow.webContents.send('clone-tab'),
          label: 'Clone Tab',
        },
        {
          click: () => mainWindow.webContents.send('close-tab'),
          label: 'Close Tab',
        },
      ],
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
              { role: 'window' },
            ]
          : [{ role: 'close' }]),
      ],
    },
  ]

  const contextMenu = Menu.buildFromTemplate([
    { role: 'copy' },
    { role: 'cut' },
    { role: 'paste' },
    { role: 'selectall' },
  ])

  const menu = Menu.buildFromTemplate(mainMenu)
  Menu.setApplicationMenu(menu)
  mainWindow.setMenu(menu)

  mainWindow.webContents.on('context-menu', (_event, params) => {
    if (params.formControlType !== 'submit-button') {
      contextMenu.popup()
    }
  })

  gvs.webContents = mainWindow.webContents

  // and load the index.html of the app.
  if (MAIN_WINDOW_VITE_DEV_SERVER_URL) {
    mainWindow.loadURL(MAIN_WINDOW_VITE_DEV_SERVER_URL)
  } else {
    mainWindow.loadFile(
      path.join(__dirname, `../renderer/${MAIN_WINDOW_VITE_NAME}/index.html`),
    )
  }
}

const readJsonFile = async (filepath) => {
  const fh = await fs.open(filepath, 'r')
  const json = await fh.readFile({ encoding: 'utf8' })

  fh.close()

  return JSON.parse(json)
}

const readGlobalConf = async () => {
  gvs.globalConf = await readJsonFile(gvs.globalConfPath)

  return gvs.globalConf
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
// exist. It returns a JSON object of the app's global configuration,
// such as the projects that exist in the projects directory.
const openGlobalConf = async () => {
  const initialConf = { projects: [], name: null, email: null }

  try {
    await fs.mkdir(gvs.projectsPath, { recursive: true })

    await readGlobalConf()

    return gvs.globalConf
  } catch (err) {
    if (err.code == 'ENOENT') {
      await writeGlobalConf(initialConf)

      return gvs.globalConf
    } else {
      throw err
    }
  } finally {
    manageProjectProcesses()
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
  const equalsCurrent = (p) => p.identifier === projectInfo.identifier
  const initialConf = {}

  await readGlobalConf()

  if (gvs.globalConf.projects.some(equalsCurrent)) {
    throw new Error('Project ID exists')
  }

  await fs.mkdir(projSharePath, { recursive: true })
  await writeProjConf(projectInfo.identifier, initialConf)

  gvs.globalConf.projects.push(projectInfo)
  await writeGlobalConf(gvs.globalConf)

  // Start up the project if it was enabled.
  manageProjectProcesses()

  return gvs.globalConf
}

const updateProject = async (_event, projectInfo) => {
  const equalsCurrent = (p) => p.identifier === projectInfo.identifier

  await readGlobalConf()

  const curr = gvs.globalConf.projects.findIndex(equalsCurrent)

  console.log(curr)

  if (!isNaN(curr)) {
    gvs.globalConf.projects[curr] = projectInfo
  }

  await writeGlobalConf(gvs.globalConf)

  if (gvs.active[projectInfo.identifier]) {
    try {
      gvs.active[projectInfo.identifier].kill()
    } catch (e) {
      console.log(e.message)
    }

    delete gvs.active[projectInfo.identifier]
  }

  // Start up the project if it was enabled.
  manageProjectProcesses()

  return gvs.globalConf
}

// This updates the non-projects related portion of global
// configuration.
const updateGlobalSettings = async (_event, globalSettings) => {
  gvs.globalConf.name = globalSettings.name
  gvs.globalConf.email = globalSettings.email

  await writeGlobalConf(gvs.globalConf)

  return gvs.globalConf
}

// This handles messages from a converter process, which has
// transformed some import data to a format suitable for a project
// database. The data is sent to the project's process so that it can
// be written to the database.
const handleImportWrite = (data) => {
  if (data.command === 'bulk-write') {
    const payload = {
      command: 'bulk-write',
      identifier: data.identifier,
      bulkDocs: data.bulkDocs,
    }

    gvs.active[data.identifier].postMessage(payload)
  } else {
    handleProjectMessage(data)
  }
}

// This event is triggered when the user has submitted a form
// containing information about a file import. In most cases, this
// event will be handled by spawning a converter process, which will
// read the file in and attempt to convert it to a database native
// format. The converter process sends a message on completion, which
// handled by handleDBWrite.
const importFile = async (_event, importOptions) => {
  switch (importOptions.kind) {
    case 'Dative Form Json': {
      const converter = utilityProcess.fork(
        path.join(__dirname, './converter.js'),
      )

      importOptions.command = 'convert-to-batch'

      converter.postMessage(importOptions)
      converter.on('message', handleImportWrite)

      return importOptions
    }
    default: {
      return importOptions
    }
  }
}

const requestTransIndex = (_event, identifier) => {
  gvs.active[identifier].postMessage({
    command: 'request-trans-index',
  })
}

const requestInterlinearIndex = (_event, identifier) => {
  gvs.active[identifier].postMessage({
    command: 'request-interlinear-index',
  })
}

const init = async () => {
  try {
    await app.whenReady()

    ipcMain.handle('request-gconfig', openGlobalConf)
    ipcMain.handle('request-trans-index', requestTransIndex)
    ipcMain.handle('request-interlinear-index', requestInterlinearIndex)
    ipcMain.handle('create-project', createProject)
    ipcMain.handle('update-project', updateProject)
    ipcMain.handle('import-file', importFile)
    ipcMain.handle('update-global-settings', updateGlobalSettings)
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
