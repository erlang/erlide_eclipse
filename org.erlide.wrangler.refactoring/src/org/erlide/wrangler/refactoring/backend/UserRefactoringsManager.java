package org.erlide.wrangler.refactoring.backend;

import java.util.Enumeration;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.erlide.core.rpc.IRpcResult;
import org.erlide.jinterface.ErlLogger;
import org.erlide.wrangler.refactoring.Activator;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Class for handling logic of installed user refactorings installed means:
 * currently in repository or added by a user
 * 
 * Singleton
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class UserRefactoringsManager {

    private static UserRefactoringsManager manager;

    private List<UserRefactoringInfo> elementaryRefacs; // gen_refac refacs
    private List<UserRefactoringInfo> compositeRefacs; // gen_composite_refac
                                                       // refacs

    private UserRefactoringsManager() {

    }

    public synchronized static UserRefactoringsManager getInstance() {
        if (manager == null)
            manager = new UserRefactoringsManager();
        return manager;
    }

    public synchronized List<UserRefactoringInfo> getElementary() {
        if (elementaryRefacs == null)
            scanForUserRefactorings();
        return elementaryRefacs;
    }

    public synchronized List<UserRefactoringInfo> getComposite() {
        if (compositeRefacs == null)
            scanForUserRefactorings();
        return compositeRefacs;
    }

    /**
     * Looks for user defined refactorings (in order to generate menu items for
     * them)
     */
    private void scanForUserRefactorings() {
        elementaryRefacs = new LinkedList<UserRefactoringInfo>();
        compositeRefacs = new LinkedList<UserRefactoringInfo>();

        Bundle coreBundle = Platform.getBundle(Activator.CORE_ID);

        @SuppressWarnings("rawtypes")
        Enumeration modules = coreBundle.findEntries("wrangler/ebin", "*.beam",
                true);

        List<OtpErlangObject> erlModules = new LinkedList<OtpErlangObject>();
        while (modules.hasMoreElements()) {
            String next = modules.nextElement().toString();
            String module = next.substring(next.lastIndexOf('/') + 1,
                    next.lastIndexOf('.'));
            erlModules.add(new OtpErlangString(module));
        }

        IRpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser(
                        "get_user_refactorings",
                        "x",
                        new OtpErlangList(erlModules
                                .toArray(new OtpErlangObject[0])));

        if (res.isOk() && res.getValue() instanceof OtpErlangList) {
            OtpErlangList genRefac = (OtpErlangList) ((OtpErlangTuple) ((OtpErlangList) res
                    .getValue()).elementAt(0)).elementAt(1);
            OtpErlangList genCompositeRefac = (OtpErlangList) ((OtpErlangTuple) ((OtpErlangList) (res
                    .getValue())).elementAt(1)).elementAt(1);

            for (OtpErlangObject obj : genRefac) {
                elementaryRefacs.add(new UserRefactoringInfo(obj.toString()));
            }
            for (OtpErlangObject obj : genCompositeRefac) {
                compositeRefacs.add(new UserRefactoringInfo(obj.toString()));
            }
        }

        ErlLogger.info("Refac modules found " + res.toString());

    }
}
