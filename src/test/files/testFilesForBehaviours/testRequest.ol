interface MyInterface{
	RequestResponse:
		reqResFunction(inputType)(outputType)
}

type inputType: int {
	x: string
	y: int
}

type outputType: string {
	x: string | int
}

service MyService(){
	inputPort in {
		Location: "socket://localhost:8081"
		Protocol: http { format = "json" }
		Interfaces: MyInterface
	}

	main{
		arg = 10
		arg.x = "hi"
		arg.y = 20

		reqResFunction(arg)(out){
			f = 10
			out = "hey"
			out.x = 20
		}
	}
}