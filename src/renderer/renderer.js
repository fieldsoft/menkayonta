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

window.electronAPI.onStatus((data) => {
  app.ports.status.send(data)
})

window.electronAPI.onReceivedReloadRequest((data) => {
  app.ports.receivedReloadRequest.send(data)
})

window.electronAPI.onReceivedPersonListing((data) => {
  app.ports.receivedPersonListing.send(data)
})

window.electronAPI.onReceivedInterlinearListing((data) => {
  app.ports.receivedInterlinearListing.send(data)
})

window.electronAPI.onReceivedSequenceListing((data) => {
  app.ports.receivedSequenceListing.send(data)
})

window.electronAPI.onReceivedSequence((data) => {
  app.ports.receivedSequence.send(data)
})

window.electronAPI.onReceivedInterlinearReversals((data) => {
  app.ports.receivedInterlinearReversals.send(data)
})

window.electronAPI.onReceivedDoc((data) => {
  app.ports.receivedDoc.send(data)
})

window.electronAPI.onReceivedNote((data) => {
  app.ports.receivedNote.send(data)
})

window.electronAPI.onReceivedNoteFor((data) => {
  app.ports.receivedNoteFor.send(data)
})

window.electronAPI.onReceivedComposite((data) => {
  app.ports.receivedComposite.send(data)
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
window.electronAPI.onMoveLeft((data) => {
  app.ports.moveLeft.send(data)
})

window.electronAPI.onMoveRight((data) => {
  app.ports.moveRight.send(data)
})

window.electronAPI.onMoveUp((data) => {
  app.ports.moveUp.send(data)
})

window.electronAPI.onMoveDown((data) => {
  app.ports.moveDown.send(data)
})

window.electronAPI.onCloseTab((data) => {
  app.ports.closeTab.send(data)
})

window.electronAPI.onCloneTab((data) => {
  app.ports.cloneTab.send(data)
})
