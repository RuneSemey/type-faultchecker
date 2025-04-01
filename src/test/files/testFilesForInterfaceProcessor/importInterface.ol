interface ImportedInterface{
	RequestResponse:
		importedReqRes( A )( B )
	OneWay:
		importedOneWay( int )
}

type A: string {
	x: int
}

type B: int {
	x: B
}