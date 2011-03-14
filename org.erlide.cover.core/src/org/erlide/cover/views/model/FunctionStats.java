package org.erlide.cover.views.model;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.erlide.core.ErlangCore;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.util.ErlangFunction;


/**
 * Coverage statistics per function.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class FunctionStats extends StatsTreeObject {

    private static final long serialVersionUID = 1L;

    private int arity; 		// function arity
    private int lineStart;  // first line of the function
    private int lineEnd;	// last line of the function

    public FunctionStats() {
        super(ObjectType.FUNCTION);
    }
    
    public int getArity() {
        return arity;
    }

    public void setArity(final int arity) {
        this.arity = arity;
    }
    
    public int getLineStart() {
    	
    	if(lineStart != 0)
    		return lineStart;
    	
    	String mName = ((StatsTreeObject)getParent()).getLabel();
    	IErlModule m;
        try {
            m = ErlangCore.getModel().findModule(mName);
            IErlFunction f = m.findFunction(
                    new ErlangFunction(getLabel(), getArity()));
            
            lineStart = f.getLineStart();
        } catch (ErlModelException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        return lineStart;
    }
    
    public int getLineEnd() {
    	
    	if(lineEnd != 0)
    		return lineEnd;
    	
    	List<Integer> numList = new LinkedList<Integer>();
    	for(IStatsTreeObject obj : getParent().getChildren()) {
    		FunctionStats fs = (FunctionStats) obj;
    		numList.add(fs.getLineStart());
    	}
    	
    	Collections.sort(numList);
    	int idx;
    	if( (idx = numList.indexOf(getLineStart())) < numList.size() - 1) {
    		lineEnd = numList.get(idx+1);
    	} else {
    		lineEnd = -1;
    	}
    	
    	return lineEnd;
    }

}
