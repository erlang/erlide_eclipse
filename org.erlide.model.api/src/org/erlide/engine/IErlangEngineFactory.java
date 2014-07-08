package org.erlide.engine;

import org.erlide.runtime.api.IRpcSite;

public interface IErlangEngineFactory {

    IErlangEngine get(IRpcSite backend);

}
