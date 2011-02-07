package org.erlide.core;

public enum ErlangStatus {

    INTERNAL_ERROR(100);

    private int value;

    ErlangStatus(final int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

}
