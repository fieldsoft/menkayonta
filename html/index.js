import { Elm } from "../src/Main.elm";

Elm.Main.init({ node: document.getElementById("root"),
                flags: { windowHeight: window.innerHeight }
              });
