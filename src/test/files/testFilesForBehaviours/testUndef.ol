service Main {
	main {
		a = 10
		b = "he"
		b.x = 10
		b.x.y = 10

		undef(a)
		undef(b.x)
	}
}