service Main {
	main {
		a = 10
		i = 0
		while(i < 3){
			a.b << a
			i++
		}
	}
}