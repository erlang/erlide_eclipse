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

    private List<LineResult> lineResults = 
        new LinkedList<LineResult>();   //results per line
    
    
    public List<LineResult> getLineResults() {
        return lineResults;
    }
    
    public void addLine(LineResult lr) {
        lineResults.add(lr);
    }
    
}
