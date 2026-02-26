// The purpose of this is essentially to allow functions to be tested.
const fs = require('node:fs')

const readJsonFile = (filepath) => {
  try {
    // Parsing, stringifying, normalizing and then parsing again is
    // painful but the input may contain escaped unicode '\uXXXX', the
    // first parse and stringification unescapes the unicode
    // values. Afterward, unicode normalization can take
    // place. Finnally, the string may be parsed a second time and
    // returned. Using 'fromCharCode()' is an alternative option, but
    // the following works.
    const filedata = fs.readFileSync(filepath, 'utf-8')
    const json = JSON.parse(filedata)
    const jsonstring = JSON.stringify(json)
    const normed = jsonstring.normalize('NFC')

    return JSON.parse(normed)
  } catch (e) {
    error(e)

    return null
  }
}

module.exports = { readJsonFile }
