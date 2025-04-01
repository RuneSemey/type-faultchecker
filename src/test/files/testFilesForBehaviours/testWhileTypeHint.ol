from .assertions import Assertions

type rec: int | recInt
type recInt: int {
	x: rec
}

service Main{
	embed Assertions as assertions

	main{
		a = 10
		while(true){
			a.x << a
			assert@assertions(a instanceof rec)
		}
	}
}
