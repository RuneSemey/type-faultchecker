service EmbedMe(param: int) {}

service Main {
	embed EmbedMe() as lol
	embed EmbedMe("hello") as lol2
}