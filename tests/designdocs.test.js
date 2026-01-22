const { menkayonta_dd } = require('../src/design_docs/menkayonta.js')

test('The design doc id is correct', () => {
  expect(menkayonta_dd._id).toBe('_design/menkayonta')
})
