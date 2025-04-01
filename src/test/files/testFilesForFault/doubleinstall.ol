main {
        scope(a){
        throw(myfault)
        install(myfault=>
        b=10)
        install(myfault2=>
        c=10)
        }
	}
