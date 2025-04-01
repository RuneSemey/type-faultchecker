type A: any {
	x: string
	y: A
}

type B: string | int

interface ImportedInterface1{
	RequestResponse:
	OneWay:
		MyOneWay(string)
}

interface ImportedInterface2 {
	RequestResponse:
		MyReqRes2(A)(B)
	OneWay:
		MyOneWay2(int)
}

service ImportService1(a: int){
	inputPort InputPort1 {
		Location: "local"
		Protocol: sodep
		Interfaces: ImportedInterface1
	}

	inputPort InputPort2 {
		Location: "local"
		Protocol: sodep
		Interfaces: ImportedInterface2
	}
}

service ImportService2(){
	inputPort InputPort2 {
		Location: "socket://localhost:8083"
		Protocol: http
		Interfaces: ImportedInterface2
	}
}