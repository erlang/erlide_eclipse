package org.erlide.engine.model.root;

import org.erlide.runtime.runtimeinfo.RuntimeVersion;

public interface IErlOtpLibrary extends IErlLibrary {

    RuntimeVersion getVersion();

}
