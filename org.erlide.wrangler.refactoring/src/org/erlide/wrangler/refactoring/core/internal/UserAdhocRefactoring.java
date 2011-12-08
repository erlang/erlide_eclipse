package org.erlide.wrangler.refactoring.core.internal;

import org.erlide.core.CoreScope;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;

/**
 * Class for common functionalities of adhoc refactorings ad hoc specific
 * methods should delegate to if
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class UserAdhocRefactoring {

    private UserRefactoring refac; // base refactoring

    public UserAdhocRefactoring(UserRefactoring refac) {
        this.refac = refac;
    }

    /**
     * Loading user's callback module
     * 
     * @return
     */
    public boolean load() {
        String callbackPath;
        try {
            if (CoreScope.getModel().findModule(refac.getCallbackModule()) == null)
                return false;

            IErlProject project = CoreScope.getModel()
                    .findModule(refac.getCallbackModule()).getProject();
            callbackPath = project.getWorkspaceProject().getLocation()
                    .append(project.getOutputLocation()).toString();
        } catch (ErlModelException e) {
            return false;
        }

        RpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser("load_callback_mod_eclipse", "ss",
                        refac.getCallbackModule(), callbackPath);
        if (!res.isOk())
            return false;
        return true;
    }

}
