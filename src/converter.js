import { Elm } from '../src-elm/Converter.elm'
import fs from 'node:fs/promises'

// Get a high-quality random seed to make a client UUID. We use 4
// 32-bit integers
const typedArray = new Int32Array(4)
const randomSeeds = crypto.getRandomValues(typedArray)

const app = Elm.Converter.init({
  flags: {
    seed1: randomSeeds[0],
    seed2: randomSeeds[1],
    seed3: randomSeeds[2],
    seed4: randomSeeds[3],
  },
})

const readJsonFile = async (filepath) => {
  try {
    const fh = await fs.open(filepath, 'r')
    const json = await fh.readFile({ encoding: 'utf8' })

    fh.close()

    return JSON.parse(json)
  } catch (e) {
    process.parentPort.postMessage({
      command: 'error',
      error: e,
      identifier: 'converter',
    })

    throw e
  }
}

const handleMainMessage = async (m) => {
  switch (m.data.command) {
    case 'convert-to-batch': {
      const parsedJson = await readJsonFile(m.data.filepath)

      if (m.data.kind === 'Dative Form Json') {
        app.ports.receivedDativeForms.send({
          project: m.data.project,
          payload: parsedJson,
        })
      }
      break
    }
    case 'init': {
      process.parentPort.postMessage({
        command: 'info',
        message: `The init was done`,
        identifier: 'converter',
      })
      break
    }
    default: {
      process.parentPort.postMessage({
        command: 'info',
        message: `Main command: ${m.data.command}`,
        identifier: 'converter',
      })
    }
  }
}

process.parentPort.on('message', handleMainMessage)

app.ports.sendBulkDocs.subscribe((job) => {
  process.parentPort.postMessage({
    command: 'bulk-write',
    identifier: job.project,
    bulkDocs: job.payload,
  })
})
