package org.erlide.engine;

public interface IEngineInput {

    void handleChangedInput(final String id, final TextChange change);

}
