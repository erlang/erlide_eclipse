package org.erlide.engine;

public interface TextChange {

    int getOffset();

    int getLength();

    String getText();

}
