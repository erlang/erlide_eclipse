package org.erlide.cover.views.model;

import java.io.Serializable;

/**
 * Line coverage results
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class LineResult implements Comparable<LineResult>, Serializable {

    private static final long serialVersionUID = 1L;

    private final int lineNum;
    private final int lineCalls;

    public LineResult(final int lineNum, final int lineCalls) {
        this.lineNum = lineNum;
        this.lineCalls = lineCalls;
    }

    /**
     * Get line number
     * 
     * @return
     */
    public int getLineNum() {
        return lineNum;
    }

    /**
     * How many times the line was called
     * 
     * @return
     */
    public int getLineCalls() {
        return lineCalls;
    }

    /**
     * If the line was called
     * 
     * @return
     */
    public boolean called() {
        return lineCalls != 0;
    }

    @Override
    public String toString() {
        final StringBuffer buf = new StringBuffer();

        buf.append(lineNum).append(" ").append(lineCalls);

        return buf.toString();
    }

    @Override
    public int compareTo(final LineResult lineRes2) {
        return lineNum - lineRes2.getLineNum();
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj instanceof LineResult
                && ((LineResult) obj).getLineNum() == lineNum) {
            return true;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return lineNum;
    }

}
