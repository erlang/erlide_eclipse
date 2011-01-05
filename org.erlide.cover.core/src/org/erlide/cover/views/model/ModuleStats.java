package org.erlide.cover.views.model;

import java.util.LinkedList;
import java.util.List;

/**
 * Module statistics
 * 
 * @author Aleksandra Lipiec
 *
 */
public class ModuleStats extends StatsTreeObject {

    private String htmlPath;            //name of html file
    private List<LineResult> lineResults = 
        new LinkedList<LineResult>();   //results per line
    
    public String getHtmlPath() {
        return htmlPath;
    }
    
    public void setHtmlPath(String htmlPath) {
        this.htmlPath = htmlPath;
    }
    
    public List<LineResult> getLineResults() {
        return lineResults;
    }
    
    public void addLine(LineResult lr) {
        lineResults.add(lr);
    }
    
}
