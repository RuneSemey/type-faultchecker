package staticTypechecker.entities;
import jolie.lang.parse.ast.OLSyntaxNode;
import jolie.lang.parse.context.ParsingContext;

public class Fault {
    String id;
    String scope;
    ParsingContext p;
    OLSyntaxNode body;
    public Fault(String id,String scope,ParsingContext p){
        this.id=id;
        this.scope=scope;
        this.p=p;
    }
    public Fault(String id,String scope,ParsingContext p,OLSyntaxNode b){
        this.id=id;
        this.scope=scope;
        this.p=p;
        this.body=b;
    }
    public String id(){
        return this.id;
    }
    public String scope(){
        return this.scope;
    }
    public ParsingContext parsingContext(){
        return this.p;
    }
    public OLSyntaxNode body(){
        return this.body;
    }
}
