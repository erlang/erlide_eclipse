package org.erlide.engine.services.search;

import org.erlide.engine.model.erlang.FunctionRef;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.rpc.IRpcFuture;

public interface XrefService extends ErlangService {

    void start();

    void stop();

    IRpcFuture addProject(IErlProject project);

    void update();

    FunctionRef[] functionUse(String mod, String fun, int arity);

    FunctionRef[] functionUse(FunctionRef ref);

}
