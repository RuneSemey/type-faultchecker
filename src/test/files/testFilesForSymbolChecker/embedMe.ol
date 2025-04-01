type inputType: string {
	x: int
	y: any
}

interface MyInterface {
	RequestResponse:
		helloReqRes( int )( string )
	OneWay:
		helloOneway( Y )
}

type Y: string {
	x: Y
}

service EmbedMe(paramEmbed: inputType){
	inputPort inputPort {
		Location: "socket://localhost:8080"
		Protocol: http { format = "json" }
		Interfaces: MyInterface 
	}

	inputPort inputPort2 {
		Location: "socket://localhost:8080"
		Protocol: http { format = "json" }
		Interfaces: MyInterface 
	}
}