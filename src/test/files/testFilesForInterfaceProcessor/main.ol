from .importInterface import ImportedInterface

interface MyInterface {
	RequestResponse: 
		myReqRes( int )( reqResReturn )
	OneWay:
		myOneWay( string ),
		mySecondOneWay( A )
}

type reqResReturn: int {
	x: int
	y: string
}

type A: bool