import './index.css'
import { Elm } from '../src-elm/Main.elm'

const app = Elm.Main.init({ node: document.getElementById("root"),
                            flags: { windowHeight: window.innerHeight }
                          })
