package staticTypechecker.entities;
import  staticTypechecker.entities.Fault;
import java.util.ArrayList;
import java.util.List;
import java.util.IdentityHashMap;
import java.util.Map.Entry;

import jolie.lang.NativeType;
import jolie.lang.parse.ast.types.BasicTypeDefinition;
import jolie.lang.parse.context.ParsingContext;
import jolie.util.Range;


public class Tau{
    private Type T;
    private Boolean ishandeled;
    private Handler H;
    private int cutof;
    private ArrayList<Fault> handledfaults=new ArrayList<Fault>();
    private ArrayList<Fault> unhandledfaults=new ArrayList<Fault>();
    private ArrayList<Handler> handlers= new ArrayList<Handler>();
    public Tau(Type T){
        this.T=T;
        this.ishandeled=false;
        cutof=0;
    }
    public Tau(Type T,Fault F){
        this.T=T;
        this.ishandeled=false;
        unhandledfaults.add(F);
        cutof=0;

    }
    public Tau(Type T,Fault F,Handler H){
        this.T=T;
        this.ishandeled=false;
        this.unhandledfaults.add(F);
        this.handlers.add(H);
        cutof=0;
    }
    public Boolean ishandled(){
        return this.ishandeled;
    }
    public int cutof(){
        return this.cutof;
    }
    public void setcutof(int i){
        this.cutof=i;
    }
    public ArrayList<Handler> gethandlers(){
        return this.handlers;
    }
    public void reorder(int cutof){
        ArrayList<Handler> H=new ArrayList(this.handlers.subList(0, this.cutof));
        this.handlers=H;
        this.cutof=cutof;
        this.ishandeled=false;
        ArrayList<Fault> temp=new ArrayList<Fault>(this.unhandledfaults);
        this.unhandledfaults.clear();
        this.handledfaults.clear();
        System.out.println("size"+temp.size());
        addFaults(temp);
        System.out.println(unhandledfaults.size());
    }
    public void addFault(Fault F){
        if(this.unhandledfaults.contains(F)){

        }
        else{
            check(F);
        }
        
    }
    public void addFaults(ArrayList<Fault>fault){
        for (Fault fault2 : fault) {
            if(!this.unhandledfaults.contains(fault2) && !this.handledfaults.contains(fault2)){
                this.addFault(fault2);
            }
            
        }
    }
    public void check(Fault F){
        String id=F.id();
        boolean added=false;
        if(this.ishandeled==false ){
            System.out.println("test3");
        for (int i = this.handlers.size() - 1; i >= this.cutof; i--) {
            System.out.println("test4");
            if(this.ishandeled==false ){
                if(this.handlers.get(i).id()==id){
                    System.out.println("test5");
                    this.ishandeled=true;
                    this.handledfaults.add(F);
                    this.H=this.handlers.get(i);
                    added=true;
                }
            }

        }
        if(added==false){
            this.unhandledfaults.add(F);
        }
        }
    }
    
    public void addHandler(Handler H){
        if(this.handlers.contains(H)){

        }
        else{
        this.handlers.add(H);
        }
    }
    public void addHandlers(ArrayList<Handler> handlers){
        for (Handler H:handlers) {
            if(!this.handlers.contains(H)){
            this.addHandler(H);
            }
        }
    }
    public static Tau join(Tau tau1,Tau tau2){
        
        ArrayList<Handler> H=new ArrayList(tau2.gethandlers().subList(tau2.cutof(), tau2.gethandlers().size()));
        ArrayList<Fault> F=tau2.getunhandledFaults();
        tau1.changeType(tau2.getType());
        tau1.addFaults(F);
        tau1.addHandlers(H);
        return tau1;

    }
    public Handler Handler(){
        return this.H;
    }
    public ArrayList<Fault> getunhandledFaults(){
        return this.unhandledfaults;
    }
    public void changeType(Type T){
        this.T=T;
    }
    public Type getType(){
        return this.T;
    }
}



