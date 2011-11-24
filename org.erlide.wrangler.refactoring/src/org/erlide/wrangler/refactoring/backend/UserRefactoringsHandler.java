package org.erlide.wrangler.refactoring.backend;

import java.util.Enumeration;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.erlide.core.rpc.IRpcResult;
import org.erlide.jinterface.ErlLogger;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Class for handling logic of installed user refactorings installed means:
 * currently in repository or added by a user
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class UserRefactoringsHandler {

    public static final String CORE = "org.erlide.wrangler.core";

    public static void scanForUserRefactorings() {

        Bundle coreBundle = Platform.getBundle(CORE);

        @SuppressWarnings("rawtypes")
        Enumeration modules = coreBundle.findEntries("wrangler/ebin", "*.beam",
                true);

        List<OtpErlangObject> erlModules = new LinkedList<OtpErlangObject>();
        while (modules.hasMoreElements()) {
            String next = modules.nextElement().toString();
            String module = next.substring(next.lastIndexOf('/') + 1,
                    next.lastIndexOf('.'));
            System.out.println(module);
            erlModules.add(new OtpErlangString(module));
        }

        IRpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser(
                        "get_user_refactorings",
                        "x",
                        new OtpErlangList(erlModules
                                .toArray(new OtpErlangObject[0])));

        ErlLogger.info("Refac modules found " + res.toString());

    }
}
