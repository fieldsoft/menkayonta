const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  setTitle: (title) => ipcRenderer.send('set-title', title),
  onNewProject: (callback) => ipcRenderer.on('new-project',
                                             (_event, _value) => callback()),
  requestGlobalConfig : () => ipcRenderer.invoke('request-gconfig'),
})
