from .assertions import Assertions, AssertionsInterface

type Weird: int {
	y: Weird | int
}

type A: int { x: string }
type B: int { x: B | int }
type C: bool | string

service MyService{
	outputPort lolleren{
		interfaces: AssertionsInterface
	}

	embed Assertions as assertions

	main {
		// x = 10

		// while(true){
		// 	x.y << x
		// 	assert@lolleren(x instanceof Weird)
		// }

		// case 1
		a = 10
		a.x = "hello"
		assert@assertions(a instanceof A)

		// case 2
		b = 10
		while(true){
			b.x << b
			assert@assertions(b instanceof B)
		}

		// case 3
		c = true
		assert@assertions(c instanceof C)                

		// case 4
		d = "hey"
		if(true){
			install(myfault=>d=10)
			throw(myfault)
		}
		assert@assertions(d instanceof int)
	}
}
