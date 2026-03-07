const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  setTitle: (title) => ipcRenderer.send('set-title', title),
  onMoveLeft: (callback) =>
    ipcRenderer.on('move-left', (_event, data) => callback(data)),
  onMoveRight: (callback, data) =>
    ipcRenderer.on('move-right', (_event, data) => callback(data)),
  onMoveUp: (callback) =>
    ipcRenderer.on('move-up', (_event, data) => callback(data)),
  onMoveDown: (callback) =>
    ipcRenderer.on('move-down', (_event, data) => callback(data)),
  onCloseTab: (callback) =>
    ipcRenderer.on('close-tab', (_event, data) => callback(data)),
  onCloneTab: (callback) =>
    ipcRenderer.on('clone-tab', (_event, data) => callback(data)),
  onNewProject: (callback) =>
    ipcRenderer.on('new-project', (_event) => callback()),
  onImportOptions: (callback) =>
    ipcRenderer.on('import-options', (_event, filepath) => callback(filepath)),
  onGlobalSettings: (callback) =>
    ipcRenderer.on('global-settings', (_event, globalConf) =>
      callback(globalConf),
    ),
  onToggleSidebar: (callback) =>
    ipcRenderer.on('toggle-sidebar', (_event) => callback()),
  onReceivedInterlinearListing: (callback) =>
    ipcRenderer.on('received-interlinear-listing', (_event, data) =>
      callback(data),
    ),
  onReceivedInterlinearReversals: (callback) =>
    ipcRenderer.on('received-interlinear-reversals', (_event, data) =>
      callback(data),
    ),
  onReceivedPersonListing: (callback) =>
    ipcRenderer.on('received-person-listing', (_event, data) => callback(data)),
  onReceivedComposite: (callback) =>
    ipcRenderer.on('received-composite', (_event, data) => callback(data)),
  onReceivedNote: (callback) =>
    ipcRenderer.on('received-note', (_event, data) => callback(data)),
  onReceivedNoteFor: (callback) =>
    ipcRenderer.on('received-note-for', (_event, data) => callback(data)),
  onReceivedDoc: (callback) =>
    ipcRenderer.on('received-doc', (_event, data) => callback(data)),
  onReceivedReloadRequest: (callback) =>
    ipcRenderer.on('received-reload-request', (_event, data) => callback(data)),
  requestGlobalConfig: () => ipcRenderer.invoke('request-gconfig'),
  updateProject: (projectInfo) =>
    ipcRenderer.invoke('update-project', projectInfo),
  importFile: (importOptions) =>
    ipcRenderer.invoke('import-file', importOptions),
  updateGlobalSettings: (globalSettings) =>
    ipcRenderer.invoke('update-global-settings', globalSettings),
  command: (envelope) => ipcRenderer.invoke('command', envelope),
})
