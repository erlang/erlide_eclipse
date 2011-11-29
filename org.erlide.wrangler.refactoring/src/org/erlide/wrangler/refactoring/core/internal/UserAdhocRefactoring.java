package org.erlide.wrangler.refactoring.core.internal;

import org.erlide.core.CoreScope;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.rpc.IRpcResult;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;

/**
 * Abstract class for ad hoc user-defined refactorings
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public abstract class UserAdhocRefactoring extends UserRefactoring {

    @Override
    public boolean fetchParPrompts() {
        if (fetched)
            return true;

        String callbackPath;
        try {
            if (CoreScope.getModel().findModule(getCallbackModule()) == null)
                return false;

            IErlProject project = CoreScope.getModel()
                    .findModule(getCallbackModule()).getProject();
            callbackPath = project.getWorkspaceProject().getLocation()
                    .append(project.getOutputLocation()).toString();
        } catch (ErlModelException e) {
            return false;
        }

        IRpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser("load_callback_mod_eclipse", "ss",
                        getCallbackModule(), callbackPath);
        if (!res.isOk())
            return false;

        return super.fetchParPrompts();
    }
}
