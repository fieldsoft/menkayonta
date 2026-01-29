/* global emit */
const design_version = 1
const design_id = '_design/menkayonta'

const menkayonta_dd = {
  _id: design_id,
  version: design_version,
  views: {
    flat_trans: {
      map: function (doc) {
        if (doc._id.startsWith('interlinear/') && doc._deleted !== true) {
          if (doc.text && doc.translations.length > 0) {
            doc.translations.forEach(function (trad) {
              emit(doc.text, trad.transcription)
            })
          } else {
            emit(doc.text, '')
          }
        }
      }.toString(),
    },
    // The below does not currently work with the schema. It is left
    // as a reference for a future implementation of a "reverse"
    // identifier view.
    meta_reversals: {
      map: function (doc) {
        if (doc._deleted !== true) {
          if (
            doc._id.startsWith('tag/') ||
            doc._id.startsWith('description/')
          ) {
            const [path, ...frag] = doc._id.split('#')
            const [t, tid, d, did] = path.split('/')
            let idstring = [d, did, t, tid].join('/')

            if (frag.length > 0) idstring = `${idstring}#${frag[0]}`

            emit(idstring)
          } else if (doc._id.startsWith('property')) {
            const [path, ...frag] = doc._id.split('#')
            const [t, tid1, tid2, d, did] = path.split('/')
            let idstring = [d, did, t, tid1, tid2].join('/')

            if (frag.length > 0) idstring = `${idstring}#${frag[0]}`

            emit(idstring)
          } else if (
            doc._id.startsWith('interlinear') ||
            doc._id.startsWith('person')
          ) {
            emit(doc._id)
          }
        }
      }.toString(),
    },
  },
}

module.exports = {
  menkayonta_dd,
}
