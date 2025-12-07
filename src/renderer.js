import { Elm } from '../src-elm/Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { windowHeight: window.innerHeight },
})

app.ports.setWindowTitle.subscribe((title) => {
  window.electronAPI.setTitle(title)
})

app.ports.requestGlobalConfig.subscribe(async () => {
  const gconfig = await window.electronAPI.requestGlobalConfig()
  app.ports.receivedGlobalConfig.send(gconfig)
})

app.ports.requestProjectIndex.subscribe((identifier) => {
  window.electronAPI.requestProjectIndex(identifier)
})

app.ports.requestInterlinearIndex.subscribe((identifier) => {
  window.electronAPI.requestInterlinearIndex(identifier)
})

app.ports.requestDocId.subscribe((message) => {
  window.electronAPI.requestDocId(message)
})

window.electronAPI.onReceivedTransIndex((data) => {
  const vista = {
    project: data.identifier,
    kind: 'all-translations',
    identifier: `all-translations::${data.identifier}`,
    content: data.payload.rows,
  }

  app.ports.receivedProjectIndex.send(vista)
})

window.electronAPI.onReceivedInterlinearIndex((data) => {
  const vista = {
    project: data.identifier,
    kind: 'all-interlinears',
    identifier: `all-interlinears::${data.identifier}`,
    content: data.payload.rows.map((elem) => elem.doc),
  }

  app.ports.receivedInterlinearIndex.send(vista)
})

window.electronAPI.onReceivedDoc((data) => {
  app.ports.receivedDoc.send(data)
})

app.ports.createProject.subscribe(async (projectInfo) => {
  const gconfig = await window.electronAPI.createProject(projectInfo)
  app.ports.receivedGlobalConfig.send(gconfig)
})

app.ports.updateProject.subscribe(async (projectInfo) => {
  const gconfig = await window.electronAPI.updateProject(projectInfo)
  app.ports.receivedGlobalConfig.send(gconfig)
})

app.ports.importFile.subscribe((importOptions) => {
  window.electronAPI.importFile(importOptions)
})

app.ports.updateGlobalSettings.subscribe(async (globalSettings) => {
  await window.electronAPI.updateGlobalSettings(globalSettings)
  const gconfig = await window.electronAPI.requestGlobalConfig()
  app.ports.receivedGlobalConfig.send(gconfig)
})

// The following are all triggered by application menu selection.
window.electronAPI.onNewProject((ident) => {
  app.ports.newProject.send(ident)
})

window.electronAPI.onImportOptions((filepath) => {
  app.ports.importOptions.send(filepath)
})

window.electronAPI.onGlobalSettings((globalConf) => {
  app.ports.globalSettings.send(globalConf)
})

// The following are triggered by tab-only context menu selection.
window.electronAPI.onMoveLeft(() => {
  app.ports.moveLeft_.send(null)
})

window.electronAPI.onMoveRight(() => {
  app.ports.moveRight_.send(null)
})

window.electronAPI.onMoveUp(() => {
  app.ports.moveUp_.send(null)
})

window.electronAPI.onMoveDown(() => {
  app.ports.moveDown_.send(null)
})

window.electronAPI.onCloseTab(() => {
  app.ports.closeTab_.send(null)
})

window.electronAPI.onCloneTab(() => {
  app.ports.cloneTab_.send(null)
})
