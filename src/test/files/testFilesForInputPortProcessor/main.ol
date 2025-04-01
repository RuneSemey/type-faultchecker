
type A: any {
	x: string
	y: A
}

type B: int | string

interface MyInterface1 {
	RequestResponse:
		MyReqRes(int)(string)
	OneWay:
		MyOneWay(bool)
}

interface MyInterface2 {
	RequestResponse:
		MyReqRes2(A)(B)
	OneWay:
		MyOneWay2(int)
}

service MyService {
	inputPort InputPort1 {
		Location: "socket://localhost:8080"
		Protocol: sodep
		Interfaces: MyInterface1
	}

	inputPort InputPort2 {
		Location: "socket://localhost:8082"
		Protocol: http { format = "json" }
		Interfaces: MyInterface2
	}
}