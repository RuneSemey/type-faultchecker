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
	inputPort messageIn {
		location: "local"
		protocol: sodep
		Interfaces: MessageInterface
	}

	main {
		[sendMessage(msg)(success){
			// do something with msg
			success = 10
		}]
	}
}