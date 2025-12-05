const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  setTitle: (title) => ipcRenderer.send('set-title', title),
  onMoveLeft: (callback) =>
    ipcRenderer.on('move-left', (_event, tabpath) => callback(tabpath)),
  onMoveRight: (callback) =>
    ipcRenderer.on('move-right', (_event, tabpath) => callback(tabpath)),
  onMoveUp: (callback) =>
    ipcRenderer.on('move-up', (_event, tabpath) => callback(tabpath)),
  onMoveDown: (callback) =>
    ipcRenderer.on('move-down', (_event, tabpath) => callback(tabpath)),
  onCloseTab: (callback) =>
    ipcRenderer.on('close-tab', (_event, tabpath) => callback(tabpath)),
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

document.addEventListener('DOMContentLoaded', () => {
  const body = document.getElementById('main-body')
  const config = { childList: true, subtree: true }

  const observer = new MutationObserver((mutations) => {
    mutations.forEach((m) => {
      if (m.type === 'childList' && m.addedNodes.length > 0) {
        m.addedNodes.forEach((n) => {
          // The first tab in a column or row is added beneath a
          // structure. Following tabs are added individually.
          const firstTab = n.querySelector('.tabnav')
          const nextTab = n.classList.contains('tabnav')

          const addListener = (elem) => {
            elem.addEventListener('contextmenu', (event) => {
              event.preventDefault()
              ipcRenderer.send('tab-menu', elem.id)
            })
          }

          if (firstTab) {
            addListener(firstTab)
          } else if (nextTab) {
            addListener(n)
          }
        })
      }
    })
  })

  observer.observe(body, config)
})
