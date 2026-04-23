const packager = require('@electron/packager')
const secrets = require('./secrets.json')

packager.packager({
    dir: '.',
    osxUniversal: {},
    platform: ["darwin", "win32"],
    arch: ["arm64", "x64"],
    osxSign: {},
    osxNotarize: {
	appleId: secrets.appleId,
	appleIdPassword: secrets.appleIdPassword,
	teamId: secrets.teamId
    }
})

