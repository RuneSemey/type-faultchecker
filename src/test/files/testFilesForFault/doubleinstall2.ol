include "console.iol"
main {
		
        install(myfault=>
        println@Console("first install")())
        throw(myfault)
        install(myfault=>
        println@Console("second install")())
	}
