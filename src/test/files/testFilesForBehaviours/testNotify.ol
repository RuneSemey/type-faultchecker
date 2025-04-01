interface MyInterface{
	OneWay:
		onewayFunction(inputType)
}

type inputType: int{
	x: string
	y: int
}

service MyService(){
	outputPort out {
		Location: "socket://localhost:8081"
		Protocol: http { format = "json" }
		Interfaces: MyInterface
	}
	
	main{
		inputType = 10
		inputType.x = "hi"
		inputType.y = 20

		onewayFunction@out(inputType)
	}
}