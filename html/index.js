// import { Elm } from "./main.js";

const app = Elm.Main.init({ node: document.getElementById("root"),
                            flags: { windowHeight: window.innerHeight }
                          });

app.ports.setTitle.subscribe((title) => {
  window.electronAPI.setTitle(title)
})

const getFilePath = async () => {
  const filePath = await window.electronAPI.openFile()
  app.ports.receivePath.send(filePath)
}

app.ports.openFile.subscribe(() => {
  getFilePath()
})

window.electronAPI.onUpdateCounter((value) => {
  app.ports.receiveCount.send(value)
})
