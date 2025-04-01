interface assInterface{
	OneWay:
		assert(bool)
}

service Assertions{
	inputPort in{
		location: "local"
		protocol: sodep
		interfaces: assInterface
	}
}