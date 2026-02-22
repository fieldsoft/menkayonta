/* global emit */
const design_version = 1
const design_id = '_design/menkayonta'

const menkayonta_dd = {
  _id: design_id,
  version: design_version,
  views: {
    meta_reversals: {
      map: ((doc) => {
        if (doc._deleted !== true) {
          const [pathPart, ...frag] = doc._id.split('#')
          const path = pathPart.split('/')

          if (!(path.length > 3)) {
            return false
          } else {
            const docid = [path[0], path[1]].join('/')
            const value = { _id: docid }
            const keyCalc = (keypath) => {
              if (frag.length > 0) {
                return [keypath, frag[0]].join('#')
              } else {
                return keypath
              }
            }

            switch (path[2]) {
              case 'tag':
              case 'description': {
                const keypath = [path[2], path[3], docid].join('/')
                const key = keyCalc(keypath)

                emit(key, value)

                return { id: doc._id, key: key, value: value }
              }
              case 'property': {
                const keypath = [path[2], path[3], path[4], docid].join('/')
                const key = keyCalc(keypath)

                emit(key, value)

                return { id: doc._id, key: key, value: value }
              }
              default: {
                return false
              }
            }
          }
        } else {
          return false
        }
      }).toString(),
    },
  },
}

module.exports = {
  menkayonta_dd,
}
