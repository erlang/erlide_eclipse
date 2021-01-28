package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangComment extends IErlangForm {
    /**
     * Comments on the same level on succesive lines are merged together. %-signs are not
     * included in the text.
     */
    Iterable<String> getText();

    /**
     * The number of % signs in front of the comment
     */
    int getLevel();
}
