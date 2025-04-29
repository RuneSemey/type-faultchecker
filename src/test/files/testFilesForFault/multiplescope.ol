main {
        scope(a){
            install(myfault=>d=10)
            scope(b){
                throw(myfault)
                scope(c){
                    throw(bob)
                }
            }
        }
	}