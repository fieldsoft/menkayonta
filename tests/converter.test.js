const fs = require('node:fs')
const { readJsonFile } = require('../src/converterHelpers.js')

//const path = require('node:path')

describe('behavior of file reading and normalization', () => {
  // The initial tests are to ensure that there is nothing strange
  // about the file that would cause errors with the test of the
  // target fucntion.
  test('the file is read to a string', () => {
    const json = fs.readFileSync(`${__dirname}/importfile.txt`, 'utf-8')

    expect(typeof json).toBe('string')
  })

  test('the file is parseable json', () => {
    const filedata = fs.readFileSync(`${__dirname}/importfile.txt`, 'utf-8')
    const json = JSON.parse(filedata)

    expect(json[0].speaker.last_name).toBe('Diewald')
  })

  test('the file is not normalized', () => {
    const filedata = fs.readFileSync(`${__dirname}/importfile.txt`, 'utf-8')
    const json = JSON.parse(filedata)

    expect(
      json[0].speaker.first_name.split('').map((c) => c.codePointAt(0)),
    ).toStrictEqual([78, 111, 97, 769, 97, 769, 104])
  })

  test('file normalization skips escaped unicode', () => {
    const filedata = fs.readFileSync(`${__dirname}/importfile.txt`, 'utf-8')
    const json = JSON.parse(filedata.normalize('NFC'))

    expect(
      json[0].speaker.first_name.split('').map((c) => c.codePointAt(0)),
    ).toStrictEqual([78, 111, 225, 97, 769, 104])
  })

  test('readJsonFile() normalizes strings', () => {
    const filejson = readJsonFile(`${__dirname}/importfile.txt`)

    expect(
      filejson[0].speaker.first_name.split('').map((c) => c.codePointAt(0)),
    ).toStrictEqual([78, 111, 225, 225, 104])
  })
})
