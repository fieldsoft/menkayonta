const packager = require('@electron/packager')
const secrets = require('./secrets.json')

packager.packager({
    dir: '.',
    osxSign: {},
    osxNotarize: {
	appleId: secrets.appleId,
	appleIdPassword: secrets.appleIdPassword,
	teamId: secrets.teamId
    }
})

