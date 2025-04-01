# Static type checker tool for Jolie
This project contains a tool for checking type safety in Jolie modules statically. At this point it supports only the most critical features of Jolie, such as loops, operation invocations and embeddings. However, you can run the type checker on any Jolie module - the features not supported will simply be ignored.

</br>

---

## How to run

### Requirements
To run the type checker you must have Java, Apache Maven and Jolie installed.</br>
</br>
The type checker has been tested on **Ubuntu 20.04.5 LTS** with **Java 11.0.17**, **Apache Maven 3.6.3** and **Jolie 1.10.5**.

</br>

### Compile the project
If you do not want to compile the project yourself, the target folder already contains a compiled `.jar` file, which you can use (just go to section **Execute the project below**).

</br>

To compile the type checker yourself run the `compile.sh` script from the root folder, like so:

```
./compile.sh
```

It will now download all the necessary dependencies for the type checker, and create a folder called `target` containing all the compiled files of the type checker. In the folder there will also be a `.jar` file with the name `typechecker-main.jar` - this is the main file.

</br>

### Execute the project
You can move this `.jar` file anywhere you want and run it. To run it, you must specify the path to the Jolie module you want to type check as the first command line argument. For example, if you wanted to run it on a file called `myJolieFile.ol` in the same folder as the type checker, you would use the command:
```
java -jar typechecker-main.jar myJolieFile.ol
```
</br>

### Output
If the type checker finds any type errors, it will print them to stdout. otherwise, it will not print anything.

</br>

### Execute with custom typehint keyword
You can use a custom name for the typehint keyword by setting a Java Property with the key `typehint`. To do so, use Java's `-D` flag, like so:
```
java -jar -Dtypehint=myCustomTypehintKeyword myJolieFile.ol
```