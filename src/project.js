const path = require('node:path')
const { menkayonta_dd } = require('./design_docs/menkayonta.js')

// Load the pouchdb packages. The pouchdb packages need commonjs
// imports.
const PouchDB = require('pouchdb-core')
const HttpPouch = require('pouchdb-adapter-http')
const mapreduce = require('pouchdb-mapreduce')
const replication = require('pouchdb-replication')
const SQLiteAdapterModule = require('pouchdb-adapter-node-sqlite')
const sqliteAdapter = SQLiteAdapterModule.default || SQLiteAdapterModule

// Set the pouchdb plugins.
PouchDB.plugin(HttpPouch)
  .plugin(mapreduce)
  .plugin(replication)
  .plugin(sqliteAdapter)

// Globals
let gvs = { design_docs: [menkayonta_dd] }

const info = (msg) => {
  process.parentPort.postMessage({
    command: 'info',
    message: msg,
    identifier: gvs.identifier,
  })
}

const error = (e) => {
  process.parentPort.postMessage({
    command: 'error',
    error: e,
    identifier: gvs.identifier,
  })
}

const status = (msg) => {
  process.parentPort.postMessage({
    command: 'status',
    content: msg,
    project: gvs.identifier,
    address: 'none',
  })
}

const startSync = async (url) => {
  gvs.remotedb = new PouchDB(url, { adapter: 'http' })

  try {
    const dbinfo = await gvs.remotedb.info()

    info(JSON.stringify(dbinfo))

    gvs.sync = gvs.db
      .sync(gvs.remotedb, {
        live: true,
        retry: true,
      })
      .on('change', (info) => {
        info('change')
        info(info)
      })
      .on('paused', (_err) => {
        // currently do nothing
      })
      .on('active', () => {
        // currently do nothing
      })
      .on('denied', (err) => {
        info(err)
      })
      .on('complete', (info) => {
        info('complete')
        info(info)
      })
      .on('error', (err) => {
        info('sync err')
        info(err)
      })

    info(JSON.stringify(gvs.sync))
  } catch (e) {
    info(e.message)
  }
}

const touchAllViews = () => {
  // This is to force an update of the index
  gvs.db.query('menkayonta/meta_reversals', {
    include_docs: false,
    startkey: '\ufff0',
  })
}

const startChangeWatcher = () => {
  gvs.changeCounter = 0

  gvs.changes = gvs.db
    .changes({ since: 'now', live: true })
    .on('change', (change) => {
      if (gvs.changeCounter < 100) {
        gvs.changeCounter += 1
      } else {
        gvs.changeCounter = 0
        touchAllViews()
      }
    })
    .on('complete', (information) => {
      info('change watcher stopped')
    })
    .on('error', (err) => {
      error(err)
    })
}

const startActiveTaskPolling = () => {
  setInterval(() => {
    const tasks = PouchDB.activeTasks.list()
    const names = tasks.map((t) => {
      return t.name
    })

    status(names)
  }, 2000)
}

const handleInit = ({ identifier: i, projectsPath: pp, url: url }) => {
  gvs.identifier = i
  gvs.path = path.join(pp, i)
  gvs.dbPath = path.join(gvs.path, `${i}.sql`)
  gvs.db = new PouchDB(gvs.dbPath, { adapter: 'sqlite3' })

  if (url) {
    startSync(url)
  }

  // Create or update the design documents
  gvs.design_docs.forEach(async (dd) => {
    try {
      const doc = await gvs.db.get(dd._id)

      if (doc.version !== dd.version) {
        dd._rev = doc._rev

        await gvs.db.put(dd)
      }
    } catch (err) {
      // 404 is given when a document is not found
      if (err.status === 404) {
        await gvs.db.put(dd)
      } else {
        error(err)
      }
    }

    startChangeWatcher()
    startActiveTaskPolling()
  })

  return gvs
}

const handleBulk = async (docs, address) => {
  try {
    await gvs.db.bulkDocs(docs)

    if (address) {
      process.parentPort.postMessage({
        command: 'received-reload-request',
        content: null,
        project: gvs.identifier,
        address: address,
      })
    }
  } catch (e) {
    error(e)
  }
}

const handleDelete = async (doc, address) => {
  try {
    await gvs.db.put(doc)

    process.parentPort.postMessage({
      command: 'received-reload-request',
      content: null,
      project: gvs.identifier,
      address: address,
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestReversal = async (queryString) => {
  try {
    const reversals = await gvs.db.query('menkayonta/meta_reversals', {
      include_docs: true,
      startkey: `${queryString}/interlinear/`,
      endkey: `${queryString}/interlinear/\ufff0`,
    })

    const onlyDocs = reversals.rows.reduce((acc, row) => {
      acc.push(row.doc)

      return acc
    }, [])

    process.parentPort.postMessage({
      command: 'received-interlinear-reversals',
      content: onlyDocs,
      project: gvs.identifier,
      address: queryString,
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestAttrReversal = async (queryString) => {
  try {
    const reversals = await gvs.db.query('menkayonta/meta_reversals', {
      include_docs: true,
      startkey: `${queryString}/\u0000/interlinear/`,
      endkey: `${queryString}/\ufff0/interlinear/\ufff0`,
    })

    const onlyDocs = reversals.rows.reduce((acc, row) => {
      acc.push(row.doc)

      return acc
    }, [])

    process.parentPort.postMessage({
      command: 'received-interlinear-reversals',
      content: onlyDocs,
      project: gvs.identifier,
      address: queryString,
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestSequence = async (docid) => {
  try {
    const sequence = await gvs.db.query('menkayonta/sequence', {
      include_docs: true,
      startkey: [docid],
      endkey: [docid, {}],
    })

    const keyed = sequence.rows.reduce((acc, row) => {
      acc.push({
        id: row.id,
        key: row.key[2],
        kind: row.key[1],
        title: row.key[3],
        description: row.key[4],
        doc: row.doc,
      })

      return acc
    }, [])

    process.parentPort.postMessage({
      command: 'received-sequence-items',
      content: keyed,
      project: gvs.identifier,
      address: docid,
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestInterlinears = async () => {
  try {
    const all = await gvs.db.allDocs({
      include_docs: true,
      startkey: 'interlinear/',
      endkey: 'interlinear/\ufff0',
    })

    const onlyDocs = all.rows.reduce((acc, row) => {
      if (row.key.length === 48) {
        acc.push(row.doc)
      }

      return acc
    }, [])

    process.parentPort.postMessage({
      command: 'received-interlinear-listing',
      content: onlyDocs,
      project: gvs.identifier,
      address: 'interlinear',
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestSequences = async () => {
  try {
    const all = await gvs.db.allDocs({
      include_docs: true,
      startkey: 'sequence/',
      endkey: 'sequence/\ufff0',
    })

    const onlyDocs = all.rows.reduce((acc, row) => {
      if (row.key.length === 45) {
        acc.push(row.doc)
      }

      return acc
    }, [])

    process.parentPort.postMessage({
      command: 'received-sequence-listing',
      content: onlyDocs,
      project: gvs.identifier,
      address: 'sequence',
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestPeople = async () => {
  try {
    const all = await gvs.db.allDocs({
      include_docs: true,
      startkey: 'person/',
      endkey: 'person/\ufff0',
    })

    const onlyDocs = all.rows.reduce((acc, row) => {
      if (row.key.split('/').length === 2) {
        acc.push(row.doc)
      }

      return acc
    }, [])

    process.parentPort.postMessage({
      command: 'received-person-listing',
      content: onlyDocs,
      project: gvs.identifier,
      address: 'person',
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestDocId = async (docid) => {
  try {
    const doc = await gvs.db.get(docid)

    process.parentPort.postMessage({
      command: 'received-doc',
      content: doc,
      project: gvs.identifier,
      address: 'doc',
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestNote = async (docid) => {
  try {
    const note = await gvs.db.get(docid)
    const reply = {
      command: 'received-note',
      content: note,
      project: gvs.identifier,
      address: docid,
    }

    process.parentPort.postMessage(reply)

    return reply
  } catch (e) {
    error(e)
  }
}

const handleRequestNoteFor = async (docid, context) => {
  try {
    const note = await gvs.db.get(`${docid}/note`)

    process.parentPort.postMessage({
      command: 'received-note-for',
      content: { note: note, desc: context },
      project: gvs.identifier,
      address: docid,
    })
  } catch (e) {
    if (e.status === 404) {
      // Succeed even if it doesn't exist, yet.
      // A new note will be created for the object.
      process.parentPort.postMessage({
        command: 'received-note-for',
        content: {
          note: { _id: `${docid}/note`, version: 1, note: 'Nothing yet.' },
          desc: context,
        },
        project: gvs.identifier,
        address: docid,
      })
    } else {
      error(e)
    }
  }
}

const handleRequestComposite = async (docid) => {
  try {
    if (typeof docid === 'string') {
      const all = await gvs.db.allDocs({
        include_docs: true,
        startkey: docid,
        endkey: `${docid}\ufff0`,
      })

      const onlyDocs = all.rows.reduce((acc, row) => {
        acc.push(row.doc)
        return acc
      }, [])

      process.parentPort.postMessage({
        command: 'received-composite',
        address: docid,
        content: onlyDocs,
        project: gvs.identifier,
      })
    } else {
      throw Error('Invalid document id in doc all requrest')
    }
  } catch (e) {
    error(e)
  }
}

const handleUpdateDoc = async (data) => {
  info(data)
  await handleBulk(data.content)
  handleRequestComposite(data.address)

  if (data.address.startsWith('interlinear')) {
    handleRequestInterlinears()
  } else if (data.address.content.startsWith('person')) {
    handleRequestPeople()
  }
}

const handleMainMessage = (m) => {
  // Ensure correct normalization for anything going into the DB.
  if (typeof m.data.content === 'string') {
    m.data.content.normalize('NFC')
  }

  switch (m.data.command) {
    case 'init': {
      handleInit(m.data)
      break
    }

    case 'update-doc': {
      handleUpdateDoc(m.data)
      break
    }

    case 'delete-doc': {
      handleDelete(m.data.content, m.data.address)
      break
    }

    case 'bulk-write': {
      handleBulk(m.data.content, m.data.address)
      break
    }

    case 'request-interlinear-listing': {
      handleRequestInterlinears()
      break
    }

    case 'request-sequence-listing': {
      handleRequestSequences()
      break
    }

    case 'request-person-listing': {
      handleRequestPeople()
      break
    }

    case 'request-person-index': {
      handleRequestPeople()
      break
    }

    case 'request-reversal': {
      handleRequestReversal(m.data.address)
      break
    }

    case 'request-attr-reversal': {
      handleRequestAttrReversal(m.data.address)
      break
    }

    case 'request-docid': {
      handleRequestDocId(m.data.docid)
      break
    }

    case 'request-note-for': {
      handleRequestNoteFor(m.data.address, m.data.content)
      break
    }

    case 'request-note': {
      handleRequestNote(m.data.address)
      break
    }

    case 'request-sequence': {
      handleRequestSequence(m.data.address)
      break
    }

    case 'request-composite': {
      handleRequestComposite(m.data.address)
      break
    }
    default: {
      info(`Main command: ${m.data.command}`)
    }
  }
}

process.parentPort.on('message', handleMainMessage)

process.on('close', () => gvs.db.close())
