interface AssertionsInterface{
	OneWay:
		assert( bool )
}

service Assertions{
	inputPort inport {
		Location: "local"
		Protocol: sodep
		Interfaces: AssertionsInterface
	}

	main{
		[assert(inBool)]{
			nullProcess	
		}
	}
}