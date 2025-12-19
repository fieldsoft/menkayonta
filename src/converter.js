import { Elm } from '../src-elm/Converter.elm'
import fs from 'node:fs/promises'

// Get a high-quality random seed to make a client UUID. We use 4
// 32-bit integers
const typedArray = new Int32Array(4)
const randomSeeds = crypto.getRandomValues(typedArray)
const args = JSON.parse(process.argv[2])
let app

const info = (msg) => {
  process.parentPort.postMessage({
    command: 'info',
    message: msg,
    identifier: 'converter',
  })
}

const error = (e) => {
  process.parentPort.postMessage({
    command: 'error',
    error: e,
    identifier: 'converter',
  })
}

try {
  app = Elm.Converter.init({
    flags: {
      seed1: randomSeeds[0],
      seed2: randomSeeds[1],
      seed3: randomSeeds[2],
      seed4: randomSeeds[3],
      me: args.me,
      project: args.project,
      time: args.time,
    },
  })
} catch (e) {
  error(e)
}

info(JSON.stringify(args))

const readJsonFile = async (filepath) => {
  try {
    const fh = await fs.open(filepath, 'r')
    const json = await fh.readFile({ encoding: 'utf8' })

    fh.close()

    return JSON.parse(json)
  } catch (e) {
    error(e)

    return null
  }
}

const handleMainMessage = async (m) => {
  switch (m.data.command) {
    case 'convert-to-batch': {
      const parsedJson = await readJsonFile(m.data.filepath)

      if (m.data.kind === 'Dative Form Json') {
        info('Got!!')

        app.ports.receivedDativeForms.send({
          project: m.data.project,
          payload: parsedJson,
        })
      }
      break
    }
    default: {
      info(`Main command: ${m.data.command}`)
    }
  }
}

process.parentPort.on('message', handleMainMessage)

app.ports.sendBulkDocs.subscribe((job) => {
  console.log('here too')
  if (Array.isArray(job.payload)) {
    process.parentPort.postMessage({
      command: 'bulk-write',
      identifier: job.project,
      bulkDocs: job.payload,
    })
  } else {
    error(Error(job.payload))
  }
})

app.ports.reportError.subscribe((error) => {
  error(Error(error))
})

app.ports.reportInfo.subscribe((msg) => {
  info(msg)
})
