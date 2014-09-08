package org.erlide.cover.views.model;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.util.ErlLogger;

/**
 * Coverage statistics per function.
 *
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 *
 */
public class FunctionStats extends StatsTreeObject {

    private static final long serialVersionUID = 1L;

    private int arity; // function arity
    private int lineStart; // first line of the function
    private int lineEnd; // last line of the function

    public FunctionStats() {
        super(ObjectType.FUNCTION);
    }

    public int getArity() {
        return arity;
    }

    public void setArity(final int arity) {
        this.arity = arity;
    }

    /**
     * First line of the function
     *
     * @return
     */
    public int getLineStart() {

        if (lineStart != 0) {
            return lineStart;
        }

        final String mName = ((StatsTreeObject) getParent()).getLabel();
        IErlModule m;
        try {
            m = ErlangEngine.getInstance().getModel().findModule(mName);
            final IErlFunction f = m.findFunction(new ErlangFunction(getLabel(),
                    getArity()));

            lineStart = f.getLineStart();
        } catch (final ErlModelException e) {
            ErlLogger.error(e);
        }

        return lineStart;
    }

    /**
     * Last line of the function
     *
     * @return
     */
    public int getLineEnd() {

        if (lineEnd != 0) {
            return lineEnd;
        }

        final List<Integer> numList = new LinkedList<Integer>();
        for (final ICoverageObject obj : getParent().getChildren()) {
            final FunctionStats fs = (FunctionStats) obj;
            numList.add(fs.getLineStart());
        }

        Collections.sort(numList);
        int idx;
        if ((idx = numList.indexOf(getLineStart())) < numList.size() - 1) {
            lineEnd = numList.get(idx + 1);
        } else {
            lineEnd = -1;
        }

        return lineEnd;
    }

}
