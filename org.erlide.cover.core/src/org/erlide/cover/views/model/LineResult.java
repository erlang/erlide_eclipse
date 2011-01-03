package org.erlide.cover.views.model;

/**
 * Line coverage results
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class LineResult {
    
    private int lineNum;
    private int lineCalls;
    
    public LineResult(int lineNum, int lineCalls) {
        this.lineNum = lineNum;
        this.lineCalls = lineCalls;
    }
    
    public int getLineNum() {
        return lineNum;
    }

    public int getLineCalls() {
        return lineCalls;
    }

    public boolean called() {
        return lineCalls != 0;
    }
    
}
