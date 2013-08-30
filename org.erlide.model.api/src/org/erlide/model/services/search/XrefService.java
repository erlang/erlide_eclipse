package org.erlide.model.services.search;

import org.erlide.model.erlang.FunctionRef;
import org.erlide.model.root.IErlProject;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.IRpcFuture;

public interface XrefService {

    public abstract void start(IRpcSite b);

    public abstract void stop(IRpcSite b);

    public abstract IRpcFuture addProject(IRpcSite b, IErlProject project);

    public abstract void update(IRpcSite b);

    public abstract FunctionRef[] functionUse(IRpcSite b, String mod,
            String fun, int arity);

    public abstract FunctionRef[] functionUse(IRpcSite b, FunctionRef ref);

}
