package staticTypechecker.entities;
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
    private int hcutof;
    private int fcutof;
    private ArrayList<Fault> handledfaults=new ArrayList<Fault>();
    private ArrayList<Fault> unhandledfaults=new ArrayList<Fault>();
    private ArrayList<Handler> handlers= new ArrayList<Handler>();
    public Tau(Type T){
        this.T=T;
        this.ishandeled=false;
        this.hcutof=0;
        this.fcutof=0;
    }
    //public Tau(Type T,Fault F){
        //this.T=T;
        //this.ishandeled=false;
        //unhandledfaults.add(F);
        //hcutof=0;
        //fcutof=1;

    //}
    public Tau(Type T,Fault F,Handler H){
        this.T=T;
        this.ishandeled=false;
        this.unhandledfaults.add(F);
        this.handlers.add(H);
        this.hcutof=1;
        this.fcutof=1;
    }
    public Tau(Type T,Tau Ta){
        this.T=T;
        this.ishandeled=Ta.ishandled();
        for (Fault fault : Ta.getunhandledFaults()) {
            this.unhandledfaults.add(fault);
        }
        for (Handler handle : Ta.gethandlers()) {
            this.handlers.add(handle);
        }
        this.handlers=Ta.gethandlers();
        hcutof=Ta.hcutof();
        fcutof=Ta.fcutof();
    }
    public Boolean ishandled(){
        return this.ishandeled;
    }
    public int hcutof(){
        return this.hcutof;
    }
    public void sethcutof(int i){
        this.hcutof=i;
    }
    public int fcutof(){
        return this.fcutof;
    }
    public void setfcutof(int i){
        this.fcutof=i;
    }
    public ArrayList<Handler> gethandlers(){
        return this.handlers;
    }
    public void reorder(int hcutof,int fcutof){
        ArrayList<Handler> H=new ArrayList(this.handlers.subList(0, this.hcutof));
        this.handlers=H;
        this.hcutof=hcutof;
        this.ishandeled=false;
        if(this.unhandledfaults.size()>0){
        ArrayList<Fault> F=new ArrayList(this.unhandledfaults.subList(0, this.fcutof));
        ArrayList<Fault> temp=new ArrayList<Fault>(this.unhandledfaults.subList(this.fcutof, this.unhandledfaults.size()));
        this.unhandledfaults.clear();
        this.handledfaults.clear();
        this.unhandledfaults=F;
        addFaults(temp);
        }
        this.fcutof=fcutof;
    }
    public void cut(){
        ArrayList<Fault> F=new ArrayList(this.unhandledfaults.subList(0, this.fcutof));
        this.unhandledfaults.clear();
        this.handledfaults.clear();
        this.unhandledfaults=F;
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
        for (int i = this.handlers.size() - 1; i >= this.hcutof; i--) {
            if(this.ishandeled==false ){
                if(this.handlers.get(i).id()==id){
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
        
        ArrayList<Handler> H=new ArrayList(tau2.gethandlers().subList(tau2.hcutof(), tau2.gethandlers().size()));
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