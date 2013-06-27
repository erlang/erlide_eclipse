package org.erlide.engine;

import org.erlide.model.TextChange;

public interface IEngineInput {

    void handleChangedInput(final String id, final TextChange change);

}
