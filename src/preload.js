const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  setTitle: (title) => ipcRenderer.send('set-title', title),
  onNewProject: (callback) => ipcRenderer.on('new-project',
                                             (_event, ident) => callback(ident)),
  requestGlobalConfig : () => ipcRenderer.invoke('request-gconfig'),
  createProject: (projectInfo) => ipcRenderer.invoke('create-project', projectInfo),
})
