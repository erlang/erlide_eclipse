package org.erlide.wrangler.refactoring.core.internal;

import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;

/**
 * Class for common functionalities of adhoc refactorings ad hoc specific
 * methods should delegate to if
 *
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class UserAdhocRefactoring {

    private final UserRefactoring refac; // base refactoring

    public UserAdhocRefactoring(final UserRefactoring refac) {
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
            if (ErlangEngine.getInstance().getModel()
                    .findModule(refac.getCallbackModule()) == null) {
                return false;
            }

            final IErlProject project = ErlangEngine
                    .getInstance()
                    .getModelUtilService()
                    .getProject(
                            ErlangEngine.getInstance().getModel()
                                    .findModule(refac.getCallbackModule()));
            callbackPath = project.getWorkspaceProject().getLocation()
                    .append(project.getProperties().getOutputDir()).toString();
        } catch (final ErlModelException e) {
            return false;
        }

        final RpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser("load_callback_mod_eclipse", "ss",
                        refac.getCallbackModule(), callbackPath);
        if (!res.isOk()) {
            return false;
        }
        return true;
    }

}
