interface MyInterface{
	OneWay:
		onewayFunction(inputType)
}

type inputType: int{
	x: string
	y: int
}

service MyService(){
	inputPort in {
		location: "local"
		protocol: sodep
		interfaces: MyInterface
	}
	
	main{
		inputType = 10
		inputType.x = "hi"
		inputType.y = 20

		onewayFunction(p)
	}
}