import './index.css'
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
  const demo = { identifier : 'xxxx',
                 content : [ { source : 'Abadeka adoke epene oñompa.',
                               translation : 'There is only a duck in the river.',
                               parse : 'abade-ka adoke epẽ-de õyõ-pa',
                               gloss : 'duck-LIM one water-LOC is.on-DECL'
                             }
                           ]
               }
  
  app.ports.receivedProjectIndex.send(JSON.stringify(demo))
})

window.electronAPI.onNewProject(() => {
  app.ports.newProject.send(null)
})
