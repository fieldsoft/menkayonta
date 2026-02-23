const { menkayonta_dd } = require('../src/design_docs/menkayonta.js')

describe('the menkayonta design document', () => {
  test('The design doc id is correct', () => {
    expect(menkayonta_dd._id).toBe('_design/menkayonta')
  })

  // patching emit
  const emit = (k, v) => {
    return null
  }

  describe('the link_from view', () => {
    test('returns false if doc is deleted', () => {
      const map = menkayonta_dd.views.link_from.map
      const fun = eval(map)
      const doc = { _deleted: true }

      expect(fun(doc)).toBe(false)
    })

    test('returns valid object', () => {
      const map = menkayonta_dd.views.link_from.map
      const fun = eval(map)
      const doc = { _id: 'link/2/3/4/5/6/7' }
      const emitted = {
        id: 'link/2/3/4/5/6/7',
        key: '4/5',
        value: { _id: '6/7' },
      }

      expect(fun(doc)).toStrictEqual(emitted)
    })
  })

  describe('the link_to view', () => {
    test('returns false if doc is deleted', () => {
      const map = menkayonta_dd.views.link_to.map
      const fun = eval(map)
      const doc = { _deleted: true }

      expect(fun(doc)).toBe(false)
    })

    test('returns valid object', () => {
      const map = menkayonta_dd.views.link_to.map
      const fun = eval(map)
      const doc = { _id: 'link/2/3/4/5/6/7' }
      const emitted = {
        id: 'link/2/3/4/5/6/7',
        key: '6/7',
        value: { _id: '4/5' },
      }

      expect(fun(doc)).toStrictEqual(emitted)
    })
  })

  describe('the meta_reversals view', () => {
    test('returns false if doc is deleted', () => {
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _deleted: true }

      expect(fun(doc)).toBe(false)
    })

    test('returns false if path is empty', () => {
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _id: '' }

      expect(fun(doc)).toBe(false)
    })

    test('returns false if path is one item', () => {
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _id: 'path' }

      expect(fun(doc)).toBe(false)
    })

    test('returns false if path is two items', () => {
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _id: 'path/path' }

      expect(fun(doc)).toBe(false)
    })

    test('returns false if path is three items', () => {
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _id: 'path/path/tag' }

      expect(fun(doc)).toStrictEqual(false)
    })

    test('returns valid object if path is long enough', () => {
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _id: 'path1/path2/tag/path3' }
      const emitted = {
        id: 'path1/path2/tag/path3',
        key: 'tag/path3/path1/path2',
        value: { _id: 'path1/path2' },
      }

      expect(fun(doc)).toStrictEqual(emitted)
    })

    test('returns false with invalid keyword', () => {
      // patching emit
      const emit = (k, v) => {
        return null
      }
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _id: 'path1/path2/tagg/path3' }

      expect(fun(doc)).toStrictEqual(false)
    })

    test('returns valid object for description', () => {
      // patching emit
      const emit = (k, v) => {
        return null
      }
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _id: 'path1/path2/description/path3' }
      const emitted = {
        id: 'path1/path2/description/path3',
        key: 'description/path3/path1/path2',
        value: { _id: 'path1/path2' },
      }

      expect(fun(doc)).toStrictEqual(emitted)
    })

    test('returns valid object for property', () => {
      // patching emit
      const emit = (k, v) => {
        return null
      }
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _id: 'path1/path2/property/path3/path4' }
      const emitted = {
        id: 'path1/path2/property/path3/path4',
        key: 'property/path3/path4/path1/path2',
        value: { _id: 'path1/path2' },
      }

      expect(fun(doc)).toStrictEqual(emitted)
    })

    test('returns valid object with fragment', () => {
      // patching emit
      const emit = (k, v) => {
        return null
      }
      const map = menkayonta_dd.views.meta_reversals.map
      const fun = eval(map)
      const doc = { _id: 'path1/path2/tag/path3#frag' }
      const emitted = {
        id: 'path1/path2/tag/path3#frag',
        key: 'tag/path3/path1/path2#frag',
        value: { _id: 'path1/path2' },
      }

      expect(fun(doc)).toStrictEqual(emitted)
    })
  })
})
