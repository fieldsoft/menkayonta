process.parentPort.on('message', (e) => {
  const [port] = e.ports
  port.start()
  port.postMessage('hello')
  port.on('message', (e) => {
    console.log(`Message from parent: ${e.data}`)
    port.postMessage(e.data)
  })
})
