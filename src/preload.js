const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  setTitle: (title) => ipcRenderer.send('set-title', title),
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
  importFile: (importOptions) =>
    ipcRenderer.invoke('import-file', importOptions),
  updateGlobalSettings: (globalSettings) =>
    ipcRenderer.invoke('update-global-settings', globalSettings),
})
