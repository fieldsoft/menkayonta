const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  setTitle: (title) => ipcRenderer.send('set-title', title),
  onNewProject: (callback) =>
    ipcRenderer.on('new-project', (_event, ident) => callback(ident)),
  onImportOptions: (callback) =>
    ipcRenderer.on('import-options', (_event, filepath) => callback(filepath)),
  requestGlobalConfig: () => ipcRenderer.invoke('request-gconfig'),
  createProject: (projectInfo) =>
    ipcRenderer.invoke('create-project', projectInfo),
  importFile: (importOptions) =>
    ipcRenderer.invoke('import-file', importOptions),
})
