service MyService(){	
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