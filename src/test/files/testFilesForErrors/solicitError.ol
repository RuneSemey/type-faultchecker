type MessageType: void {
	sender: string
	message: string
	time: int
}

interface MessageInterface {
	RequestResponse:
		sendMessage( MessageType )( bool )
}

service MyService() {
	outputPort messageSender {
		Interfaces: MessageInterface
	}

	main {
		msgObj.sender = "John Doe"
		msgObj.message = "Hello World!"
		sendMessage@messageSender(msgObj)(success)

		msgObj.time = true
		sendMessage@messageSender(msgObj)(success)
	}
}