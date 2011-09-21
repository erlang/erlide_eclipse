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

    private String md5; // file hash (in order to check if
                        // marking annotations in editor makes sense)
    private final List<LineResult> lineResults = new LinkedList<LineResult>(); // results
                                                                               // per
                                                                               // line
    public boolean couldBeMarked = true; // if annotation could be marked for
                                         // that file (if it has not changed)

    public ModuleStats() {
        super(ObjectType.MODULE);
    }

    /**
     * Coverage per line
     * 
     * @return
     */
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

    public void setMd5(final String md5) {
        this.md5 = md5;
    }

    public String getMd5() {
        return md5;
    }

}
