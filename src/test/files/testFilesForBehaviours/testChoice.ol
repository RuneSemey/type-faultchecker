interface MyInterface{
	RequestResponse:
		reqResFunction(inputType)(outputType)
	OneWay:
		oneWayFunction(int),
		oneWayFunction2(string)
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
		[reqResFunction(arg)(out){
			out = "hey"
			out.x = "hi"
		}]{
			out.y = 10
		}
		[oneWayFunction(input)]{
			out = 10
		}
		[oneWayFunction2(input)]{
			out = true
			random = 20
		}
	}
}