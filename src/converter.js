import { Elm } from '../src-elm/Converter.elm'
import { v4 } from 'uuid'
import fs from 'node:fs/promises'

// Get a high-quality random seed to make a client UUID. We use 4
// 32-bit integers
var typedArray = new Int32Array(4)
var randomSeeds = crypto.getRandomValues(typedArray)

const app = Elm.Converter.init({ flags: { seed1: randomSeeds[0],
                                          seed2: randomSeeds[1],
                                          seed3: randomSeeds[2],
                                          seed4: randomSeeds[3],
                                        }
                               })

const readJsonFile = async (filepath) => {
  const fh = await fs.open(filepath, 'r')
  const json = await fh.readFile({ encoding: 'utf8' })

  fh.close()
  
  return JSON.parse(json)
}

const handleMainMessage = async (m) => {
  switch (m.data.command) {
  case 'convert-to-batch':
    const parsedJson = await readJsonFile(m.data.filepath)

    process.parentPort.postMessage(
      { command: 'info',
        message: `The init was done`,
        identifier: 'converter',
      })

    if (m.data.kind === 'Dative Form Json') {
      app.ports.receivedDativeForms.send(parsedJson)
    }
    break
  case 'init':
    process.parentPort.postMessage(
      { command: 'info',
        message: `The init was done`,
        identifier: 'converter',
      })
    break
  default:
    process.parentPort.postMessage(
      { command: 'info',
        message: `Main command: ${m.data.command}`,
        identifier: 'converter',
      })
  }            
}

process.parentPort.on('message', handleMainMessage)

app.ports.subscribe.sendBulkDocs((bulkDocs) => {
})
                                 
// process.on('close', () => gvs.db.close())
