from .main import C, K

type importedType1: int {
	x: int
	y: int
}

type importedType2: string {
	x: C
}

type importedCircular: void {
	x: K
}