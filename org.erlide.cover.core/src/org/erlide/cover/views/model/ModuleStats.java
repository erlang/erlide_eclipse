package org.erlide.cover.views.model;

import java.util.LinkedList;
import java.util.List;

/**
 * Module statistics
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class ModuleStats extends StatsTreeObject {

    private static final long serialVersionUID = 1L;

    private final List<LineResult> lineResults = 
    	new LinkedList<LineResult>(); 	// results per line
    
    public ModuleStats() {
        super(ObjectType.MODULE);
    }

    public List<LineResult> getLineResults() {
        return lineResults;
    }

    public void addLine(final LineResult lr) {
        lineResults.add(lr);
    }
    
    @Override
    public void addChild(final String name, final ICoverageObject child) {
    	
    	super.addChild(name, child);
    }

}
