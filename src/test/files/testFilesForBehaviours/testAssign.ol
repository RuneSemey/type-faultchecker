service Main {
	main {
		a = 10
		a.x = "h"
		a.x.y = true
		
		b = "h"
		b.h = 20L

		a = b.h

		c = a.x.y
	}
}