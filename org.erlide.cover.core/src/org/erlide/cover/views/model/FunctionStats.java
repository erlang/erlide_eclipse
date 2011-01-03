package org.erlide.cover.views.model;

/**
 * Coverage statistics per function.
 * 
 * @author Aleksandra Lipiec
 *
 */
public class FunctionStats extends StatsTreeObject {
    
    private int arity;      //function arity

    public int getArity() {
        return arity;
    }

    public void setArity(int arity) {
        this.arity = arity;
    }

}
