from .types import penguin
from .embedMe import EmbedMe

interface MyInterface {
	RequestResponse:
		helloReqRes( int )( string )
	OneWay:
		helloOneway( Y )
}

type Y: string {
	x: Y
}

type X: int {
	x: X
}

type paramType: string {
	x: int
	y: bool
}

service MyService(param: paramType) {
	inputPort inputPort {
		Location: "socket://localhost:8080"
		Protocol: http { format = "json" }
		Interfaces: MyInterface 
	}
	
	outputPort outputPort {
		Location: "socket://localhost:8081"
		Protocol: http { format = "json" }
		Interfaces: MyInterface
	}

	embed EmbedMe(param) as embedMe

}