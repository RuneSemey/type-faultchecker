package staticTypechecker.entities;
import jolie.lang.parse.ast.OLSyntaxNode;

public class Handler {
    OLSyntaxNode body;
    String id;
    String scope;
    public Handler(String id,OLSyntaxNode i, String scope){
        this.body=i;
        this.id=id;
        this.scope=scope;
    }
    public String id(){
        return this.id;
    }
    public OLSyntaxNode body(){
        return this.body;
    }
    public String Scope(){
        return this.scope;
    }
}