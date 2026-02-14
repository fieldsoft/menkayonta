const typedArray = new Int32Array(4)
const randomSeeds = crypto.getRandomValues(typedArray)

const app = window.Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    seeds: {
      seed1: randomSeeds[0],
      seed2: randomSeeds[1],
      seed3: randomSeeds[2],
      seed4: randomSeeds[3],
    },
    dimensions: {
      length: document.documentElement.clientWidth,
      width: document.documentElement.clientHeight,
    },
  },
})

window.addEventListener('resize', (_event) => {
  const dimensions = {
    length: document.documentElement.clientWidth,
    width: document.documentElement.clientHeight,
  }

  app.ports.receivedViewArea.send(dimensions)
})

app.ports.send.subscribe((envelope) => {
  window.electronAPI.command(envelope)
})

// app.ports.setWindowTitle.subscribe((title) => {
//   window.electronAPI.setTitle(title)
// })

app.ports.requestGlobalConfig.subscribe(async () => {
  const gconfig = await window.electronAPI.requestGlobalConfig()
  app.ports.receivedGlobalConfig.send(gconfig)
})

app.ports.requestInterlinearIndex.subscribe((identifier) => {
  window.electronAPI.requestInterlinearIndex(identifier)
})

window.electronAPI.onReceivedPersonIndex((data) => {
  app.ports.receivedPersonIndex.send(data)
})

window.electronAPI.onReceivedInterlinearIndex((data) => {
  app.ports.receivedInterlinearIndex.send(data)
})

window.electronAPI.onReceivedDoc((data) => {
  app.ports.receivedDoc.send(data)
})

window.electronAPI.onReceivedAllDoc((data) => {
  app.ports.receivedAllDoc.send(data)
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
window.electronAPI.onNewProject(() => {
  app.ports.newProject.send(null)
})

window.electronAPI.onToggleSidebar(() => {
  app.ports.toggleSidebar.send(null)
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
