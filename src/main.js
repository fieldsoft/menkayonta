const {
  app,
  BrowserWindow,
  ipcMain,
  Menu,
  utilityProcess,
  dialog,
} = require('electron')
const path = require('node:path')
const fs = require('node:fs/promises')
const os = require('node:os')
const { cwd } = require('node:process')
const { v4 } = require('uuid')

const production = process.env.NODE_ENV === 'production' || app.isPackaged

const environment = () => {
  if (production) {
    return 'production'
  } else if (process.env.NODE_ENV === 'development') {
    return 'development'
  } else if (process.env.NODE_ENV === 'testing') {
    return 'testing'
  } else {
    throw Error('Undefined runtime environment. Try setting NODE_ENV.')
  }
}

const home = () => {
  switch (environment()) {
    case 'production': {
      return os.homedir()
    }

    case 'development': {
      return path.join(cwd(), '.devel')
    }

    case 'testing': {
      return path.join(cwd(), '.testing')
    }

    default: {
      throw Error('Undefined runtime environment. Try setting NODE_ENV.')
    }
  }
}

// Global variables.
let gvs = {
  projectsPath: path.join(home(), 'Menkayonta'),
  globalConfPath: path.join(home(), 'Menkayonta', 'config.json'),
  globalConf: null,
  active: [],
}

// Test if the platform is macos.
const isMac = process.platform === 'darwin'

// Handle creating/removing shortcuts on Windows when
// installing/uninstalling.
// if (started) {
//   app.quit()
// }

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
    case 'received-person-index':
      gvs.webContents.send(m.command, m)
      break
    case 'received-interlinear-index':
      gvs.webContents.send(m.command, m)
      break
    case 'received-all-doc':
      gvs.webContents.send(m.command, m)
      break
    case 'received-doc':
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
          click: () => mainWindow.webContents.send('new-project'),
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
  if (environment() === 'development') {
    mainWindow.loadURL('http://localhost:4403')
  } else {
    mainWindow.loadFile(path.join(__dirname, 'renderer', 'index.html'))
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
  const initialConf = {
    projects: [],
    name: null,
    email: null,
  }

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

// Create or update a project and return updated configuration.
// This requires the following:
// 1. Retrieve the current global config
// 2. Check whether the project id is present
// 3. If so, branch to a helper function, if not, continue.
// 4. Create the new project directory structure
// 5. Create the new project config file
// 6. Initialize a database
// 7. Send the globabl config to the renderer
const updateProject = async (_event, projectInfo) => {
  const projPath = path.join(gvs.projectsPath, projectInfo.identifier)
  const projSharePath = path.join(projPath, 'share')
  const equalsCurrent = (p) => p.identifier === projectInfo.identifier
  const initialConf = {}

  await readGlobalConf()

  if (gvs.globalConf.projects.some(equalsCurrent)) {
    await updateProjectHelper(projectInfo)
  } else {
    await fs.mkdir(projSharePath, { recursive: true })
    await writeProjConf(projectInfo.identifier, initialConf)

    gvs.globalConf.projects.push(projectInfo)
    await writeGlobalConf(gvs.globalConf)

    // Start up the project if it was enabled.
    manageProjectProcesses()

    // const payload = {
    //   command: 'bulk-write',
    //   identifier: projectInfo.identifier,
    //   bulkDocs: seed,
    // }

    // gvs.active[projectInfo.identifier].postMessage(payload)
  }

  return gvs.globalConf
}

const updateProjectHelper = async (projectInfo) => {
  const equalsCurrent = (p) => p.identifier === projectInfo.identifier

  await readGlobalConf()

  const curr = gvs.globalConf.projects.findIndex(equalsCurrent)

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
      content: data.content,
    }

    gvs.active[data.identifier].postMessage(payload)
  } else {
    handleProjectMessage(data)
  }
}

const handleDativeJson = (importOptions) => {
  const args = JSON.stringify({
    project: importOptions.project,
    me: gvs.globalConf.email,
    time: Date.now(),
  })
  const converter = utilityProcess.fork(
    path.join(__dirname, './converter.js'),
    [args],
  )

  console.log(converter.pid)

  importOptions.command = 'convert-to-batch'

  converter.postMessage(importOptions)
  converter.on('message', handleImportWrite)

  return importOptions
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
      handleDativeJson(importOptions)

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

const requestPersonIndex = (_event, identifier) => {
  gvs.active[identifier].postMessage({
    command: 'request-person-index',
  })
}

const command = (_event, envelope) => {
  switch (envelope.command) {
    case 'bulk-write':
    case 'request-docid': {
      gvs.active[envelope.project].postMessage(envelope)
      break
    }
    default: {
      gvs.active[envelope.identifier].postMessage(envelope)
    }
  }
}

const init = async () => {
  try {
    await app.whenReady()

    ipcMain.handle('request-gconfig', openGlobalConf)
    ipcMain.handle('request-trans-index', requestTransIndex)
    ipcMain.handle('request-interlinear-index', requestInterlinearIndex)
    ipcMain.handle('request-person-index', requestPersonIndex)
    ipcMain.handle('request-all-docid', requestDocId)
    ipcMain.handle('request-docid', requestDocId)
    ipcMain.handle('update-project', updateProject)
    ipcMain.handle('import-file', importFile)
    ipcMain.handle('update-global-settings', updateGlobalSettings)
    ipcMain.handle('command', command)
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
