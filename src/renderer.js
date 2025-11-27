import { Elm } from '../src-elm/Main.elm'

const app = Elm.Main.init({ node: document.getElementById("root"),
                            flags: { windowHeight: window.innerHeight }
                          })

app.ports.setWindowTitle.subscribe((title) => {
  window.electronAPI.setTitle(title)
})

app.ports.requestGlobalConfig.subscribe(async () => {
  const gconfig = await window.electronAPI.requestGlobalConfig()
  app.ports.receivedGlobalConfig.send(gconfig)
})

app.ports.requestProjectIndex.subscribe((identifier) => {
  const demo = { project : identifier,
                 kind : 'all-translations',
                 identifier : `all-translations::${identifier}`,
                 content : [ { source : 'Abadeka adoke epene oñompa.',
                               translation : 'There is only a duck in the river.',
                             },
                             {
                               source : 'Abopa adoke okiye.',
                               translation : 'I see one woman.',
                             },
                           ]
               }
  
  app.ports.receivedProjectIndex.send(demo)
})

window.electronAPI.onNewProject((ident) => {
  app.ports.newProject.send(ident)
})

window.electronAPI.onImportOptions((filepath) => {
  console.log(filepath)
  app.ports.importOptions.send(filepath)
})

app.ports.createProject.subscribe(async (projectInfo) => {
  const gconfig = await window.electronAPI.createProject(projectInfo)
  app.ports.receivedGlobalConfig.send(gconfig)
})

app.ports.readImportFile.subscribe(async (importOptions) => {
  const result = await window.electronAPI.readImportFile(importOptons)
  app.ports.receivedJsonFromFile.send(result)
})
