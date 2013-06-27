package org.erlide.model;

public interface TextChange {

    int getOffset();

    int getLength();

    String getText();

}
