const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  setTitle: (title) => ipcRenderer.send('set-title', title),
  onMoveLeft: (callback) => ipcRenderer.on('move-left', (_event) => callback()),
  onMoveRight: (callback) =>
    ipcRenderer.on('move-right', (_event) => callback()),
  onMoveUp: (callback) => ipcRenderer.on('move-up', (_event) => callback()),
  onMoveDown: (callback) => ipcRenderer.on('move-down', (_event) => callback()),
  onCloseTab: (callback) => ipcRenderer.on('close-tab', (_event) => callback()),
  onNewProject: (callback) =>
    ipcRenderer.on('new-project', (_event, ident) => callback(ident)),
  onImportOptions: (callback) =>
    ipcRenderer.on('import-options', (_event, filepath) => callback(filepath)),
  onGlobalSettings: (callback) =>
    ipcRenderer.on('global-settings', (_event, globalConf) =>
      callback(globalConf),
    ),
  onReceivedTransIndex: (callback) =>
    ipcRenderer.on('received-trans-index', (_event, data) => callback(data)),
  requestGlobalConfig: () => ipcRenderer.invoke('request-gconfig'),
  requestProjectIndex: (identifier) =>
    ipcRenderer.invoke('request-trans-index', identifier),
  createProject: (projectInfo) =>
    ipcRenderer.invoke('create-project', projectInfo),
  updateProject: (projectInfo) =>
    ipcRenderer.invoke('update-project', projectInfo),
  importFile: (importOptions) =>
    ipcRenderer.invoke('import-file', importOptions),
  updateGlobalSettings: (globalSettings) =>
    ipcRenderer.invoke('update-global-settings', globalSettings),
})
