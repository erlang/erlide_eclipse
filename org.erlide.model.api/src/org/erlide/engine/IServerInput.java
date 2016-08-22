package org.erlide.engine;

public interface IServerInput {

    void handleChangedInput(final String id, final TextChange change);

}
