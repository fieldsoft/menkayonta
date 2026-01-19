import { test } from 'uvu'
import * as assert from 'uvu/assert'
import { menkayonta_dd } from '../src/design_docs/menkayonta.mjs'

test('The design doc id is correct', () => {
  assert.is(menkayonta_dd._id, '_design/menkayonta')
})

test.run()
