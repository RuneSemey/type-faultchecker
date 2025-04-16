main {
        scope(a){
            scope(b){
                throw(myfault)
                scope(c){
                    install(myfault=>d=10)
                }
            }
        }
	}