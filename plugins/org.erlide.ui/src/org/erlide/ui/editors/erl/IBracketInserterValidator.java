package org.erlide.ui.editors.erl;

interface IBracketInserterValidator {

    public abstract boolean earlyCancelCheck();

    public abstract boolean validInput();

}
