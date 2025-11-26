const handleMainMessage = (m) => {
  switch (m.data.command) {
  case 'init':
    process.parentPort.postMessage({command: 'info', message: 'oko'})
    break
  default:
    process.parentPort.postMessage(
      { command: 'info',
        message: `Main command: ${m.data.command}`
      })
  }            
}

process.parentPort.on('message', handleMainMessage)
