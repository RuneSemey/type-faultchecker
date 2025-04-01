type rec1: int {
	x: rec1
}

type rec2: int {
	y: rec2
	x: C
}

type C: D | E

type D: bool {
	r: C
}

type E: string {
	z: string
}

type input: void {
	r1: rec1
	r2: rec2
}

service Main(p: input){
	main{
		p.r1.x.x = "10"
		p.r2.x.z = 10
	}
}