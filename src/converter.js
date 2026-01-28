const fs = require('node:fs')
const path = require('node:path')
const vm = require('node:vm')

// Get a high-quality random seed to make a client UUID. We use 4
// 32-bit integers
const typedArray = new Int32Array(4)
const randomSeeds = crypto.getRandomValues(typedArray)
const args = JSON.parse(process.argv[2])

// Load the elm code into the global namespace
const elmPath = path.join(__dirname, 'conv.js')
const code = fs.readFileSync(elmPath, 'utf-8')
vm.runInThisContext(code, elmPath)

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
  app = global.Elm.Converter.init({
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

const readJsonFile = (filepath) => {
  try {
    const json = fs.readFileSync(filepath, 'utf-8')

    return JSON.parse(json)
  } catch (e) {
    error(e)

    return null
  }
}

const handleMainMessage = (m) => {
  switch (m.data.command) {
    case 'convert-to-batch': {
      const parsedJson = readJsonFile(m.data.filepath)

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
  if (Array.isArray(job.payload)) {
    process.parentPort.postMessage({
      command: 'bulk-write',
      identifier: job.project,
      address: '',
      content: job.payload,
    })
  } else {
    error(Error(job.payload))
  }
})

app.ports.reportError.subscribe((e) => {
  error(new Error(e))
})

app.ports.reportInfo.subscribe((_msg) => {
  // this should eventually be enabled by a debug setting.
  //info(msg)
})
