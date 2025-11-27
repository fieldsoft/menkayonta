const trans_version = 1
const trans_id = '_design/trans'

var trans_dd =  {
  _id: trans_id,
  version: trans_version,
  views: {
    simple: {
      map: function (doc) {
        if (doc._id.startsWith('interlinear::') && doc._deleted !== true) {
          if (doc.transcription && doc.translations.length > 0) {
            doc.translations.forEach(function (trad) {
              emit(doc.transcription, trad.transcription)
            });
          } else {
            emit(doc.transcription, "")
          }
        }
      }.toString()
    }
  }
}

export { trans_dd }
