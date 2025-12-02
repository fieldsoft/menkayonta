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

window.electronAPI.onNewProject((ident) => {
  app.ports.newProject.send(ident)
})

window.electronAPI.onImportOptions((filepath) => {
  app.ports.importOptions.send(filepath)
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

app.ports.createProject.subscribe(async (projectInfo) => {
  const gconfig = await window.electronAPI.createProject(projectInfo)
  app.ports.receivedGlobalConfig.send(gconfig)
})

app.ports.importFile.subscribe((importOptions) => {
  window.electronAPI.importFile(importOptions)
})
