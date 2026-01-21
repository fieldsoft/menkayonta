const path = require('node:path')
const { menkayonta_dd } = require('./design_docs/menkayonta.js')

// Load the pouchdb packages. The pouchdb packages need commonjs
// imports.
const PouchDB = require('pouchdb-core')
const HttpPouch = require('pouchdb-adapter-http')
const mapreduce = require('pouchdb-mapreduce')
const replication = require('pouchdb-replication')
const sqliteAdapter = require('pouchdb-adapter-node-websql')

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

const handleInit = ({ identifier: i, projectsPath: pp, url: url }) => {
  gvs.identifier = i
  gvs.path = path.join(pp, i)
  gvs.dbPath = path.join(gvs.path, `${i}.sql`)
  gvs.db = new PouchDB(gvs.dbPath, { adapter: 'websql' })

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
  })

  return gvs
}

const handleBulk = async (docs) => {
  try {
    await gvs.db.bulkDocs(docs)
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
      command: 'received-interlinear-index',
      payload: onlyDocs,
      identifier: gvs.identifier,
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
      if (row.key.length === 43) {
        acc.push(row.doc)
      }

      return acc
    }, [])

    process.parentPort.postMessage({
      command: 'received-person-index',
      content: onlyDocs,
      identifier: gvs.identifier,
      address: 'person',
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestTranslations = async () => {
  try {
    const transIndex = await gvs.db.query('trans/simple')

    process.parentPort.postMessage({
      command: 'received-trans-index',
      payload: transIndex,
      identifier: gvs.identifier,
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
      doc: doc,
      identifier: gvs.identifier,
    })
  } catch (e) {
    error(e)
  }
}

const handleRequestAllDocId = async (docid) => {
  try {
    if (typeof docid === 'string') {
      const all = await gvs.db.query('menkayonta/meta_reversals', {
        include_docs: true,
        startkey: docid,
        endkey: `${docid}\ufff0`,
      })

      const onlyDocs = all.rows.reduce((acc, row) => {
        acc.push(row.doc)
        return acc
      }, [])

      process.parentPort.postMessage({
        command: 'received-all-doc',
        address: docid,
        content: onlyDocs,
        identifier: gvs.identifier,
      })
    } else {
      throw Error('Invalid document id in doc all requrest')
    }
  } catch (e) {
    error(e)
  }
}

const handleUpdateDoc = async (data) => {
  await handleBulk(data.content)
  handleRequestAllDocId(data.address)

  if (data.address.startsWith('interlinear')) {
    handleRequestInterlinears()
  } else if (data.address.content.startsWith('person')) {
    handleRequestPeople()
  }
}

const handleMainMessage = (m) => {
  switch (m.data.command) {
    case 'init': {
      handleInit(m.data)
      break
    }

    case 'update-doc': {
      handleUpdateDoc(m.data)
      break
    }

    // TODO: There are two possible formats sent to bulk-write. This
    // needs to be fixed.
    case 'bulk-write': {
      if (m.data.content) {
        handleBulk(m.data.content)
      } else if (m.data.bulkDocs) {
        handleBulk(m.data.bulkDocs)
      } else if (m.bulkDocs) {
        handleBulk(m.bulkDocs)
      } else {
        error(Error(`Malformed bulk docs object: ${JSON.stringify(m)}`))
      }
      break
    }

    case 'request-trans-index': {
      handleRequestTranslations()
      break
    }

    case 'request-interlinear-index': {
      handleRequestInterlinears()
      break
    }

    case 'request-person-index': {
      handleRequestPeople()
      break
    }

    case 'request-docid': {
      handleRequestDocId(m.data.docid)
      break
    }

    case 'request-all-docid': {
      handleRequestAllDocId(m.data.address)
      break
    }
    default: {
      info(`Main command: ${m.data.command}`)
    }
  }
}

process.parentPort.on('message', handleMainMessage)

process.on('close', () => gvs.db.close())
