from .importServices import ImportService1, ImportService2

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
	outputPort OutputPort1 {
		Location: "socket://localhost:8080"
		Protocol: sodep
		Interfaces: MyInterface1
	}

	outputPort OutputPort2 {
		Location: "socket://localhost:8081"
		Protocol: http
		Interfaces: MyInterface2
	}

	embed ImportService1(10) as i3
	embed ImportService2 in OutputPort2
}