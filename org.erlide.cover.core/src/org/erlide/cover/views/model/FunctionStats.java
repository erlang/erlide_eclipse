package org.erlide.cover.views.model;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.util.ErlangFunction;

/**
 * Coverage statistics per function.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class FunctionStats extends StatsTreeObject {

    private int arity; 		// function arity
    private int lineStart;  // first line of the function
    private int lineEnd;	// last line of the function

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
    	IErlModule m = ErlangCore.getModel().findModule(mName);
        IErlFunction f = m.findFunction(
        		new ErlangFunction(getLabel(), getArity()));
        
        lineStart = f.getLineStart();
        return lineStart;
    }
    
    public int getLineEnd() {
    	
    	if(lineEnd != 0)
    		return lineEnd;
    	
    	String mName = ((StatsTreeObject)getParent()).getLabel();
    	IErlModule m = ErlangCore.getModel().findModule(mName);
    	
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
