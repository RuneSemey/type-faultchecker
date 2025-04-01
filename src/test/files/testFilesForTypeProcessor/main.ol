from .importedTypes import importedType1, importedType2, importedCircular

type A: void {
	x: int
	y: string {
		z: bool
	}
}

type B: any {
	?
	x: int
}

type C: string | int

type D: A | C

type E: int {
	x: A
}

type F: string {
	x: F
}

type G: string {
	x: H
}

type H: int {
	x: G
}

type I: bool {
	x: I | void
}

type J: int {
	x: string {
		y: J
	}
}

type K: int {
	x: importedCircular
}

type L: int {
	x: string {
		y: L
	}
}

type Weird: int {
	y: Weird | int
}