/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.WranglerRefactoringBackend;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.DuplicateCodeDetectionInputDialog;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Idenctical code detection refactoring runner
 * 
 * @author Gyorgy Orosz
 * 
 */
public class DuplicateDetectionAction extends AbstractDuplicatesSearcherAction {

    protected boolean onlyInfile;
    protected int minToks;
    protected int minClones;

    @SuppressWarnings("boxing")
    @Override
    protected IResultParser callRefactoring()
            throws WranglerRpcParsingException, CoreException, IOException,
            WranglerWarningException {
        String functionName;
        RpcResult result;

        // getting the path of the fragment

        final String suffixPath = getSuffixPath();
        ErlLogger.debug("Suffix binary at: " + suffixPath);
        final WranglerRefactoringBackend backend = WranglerBackendManager
                .getRefactoringBackend();
        final IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
                .getWranglerSelection();

        if (onlyInfile) {
            functionName = "duplicated_code_eclipse";
            final OtpErlangString fp = new OtpErlangString(sel.getFilePath());
            final OtpErlangString[] fpa = new OtpErlangString[1];
            fpa[0] = fp;
            final OtpErlangList fpl = new OtpErlangList(fpa);

            result = backend.callWithoutParser(
                    WranglerRefactoringBackend.UNLIMITED_TIMEOUT, functionName,
                    "xiiis", fpl, minToks, minClones,
                    GlobalParameters.getTabWidth(), suffixPath);
        } else {
            functionName = "duplicated_code_eclipse";
            result = backend.callWithoutParser(
                    WranglerRefactoringBackend.UNLIMITED_TIMEOUT, functionName,
                    "xiiis", sel.getSearchPath(), minToks, minClones,
                    GlobalParameters.getTabWidth(), suffixPath);
        }

        if (!result.isOk()) {
            throw new WranglerRpcParsingException("Rpc error");
        }
        return new DuplicateDetectionParser(result.getValue());
    }

    private String getSuffixPath() throws IOException, WranglerWarningException {
        final Bundle[] bs = Platform
                .getFragments(Platform
                        .getBundle(org.erlide.wrangler.refactoring.Activator.PLUGIN_ID));
        if (bs.length < 1) {
            ErlLogger.debug("Fragment is not loaded?! No C binary is run.");
            return "";
        }
        Bundle fragment = null;
        for (int i = 0; i < bs.length; ++i) {
            if (bs[i].getSymbolicName().equals(
                    "org.erlide.wrangler.refactoring.duplicatedcode")) {
                fragment = bs[i];
                break;
            }
        }
        java.net.URL url = FileLocator.find(fragment, new Path(""), null);
        url = FileLocator.resolve(url);
        IPath path = new Path(url.getPath());
        path = path.append("wrangler");
        path = path.append("bin");

        final String os = Platform.getOS();
        if (os.equals(Platform.OS_LINUX)) {
            path = path.append("linux");
            path = path.append("suffixtree");
        } else if (os.equals(Platform.OS_WIN32)) {
            path = path.append("win32");
            path = path.append("suffixtree.exe");
        } else if (os.equals(Platform.OS_MACOSX)) {
            path = path.append("macosx");
            path = path.append("suffixtree");
        } else {
            ErlLogger.debug("Not supported OS found, no C binary is used.");
            return "";
        }

        return path.toOSString();
    }

    @Override
    protected boolean getUserInput() {
        final Shell shell = PlatformUI.getWorkbench().getDisplay()
                .getActiveShell();

        final DuplicateCodeDetectionInputDialog inputd = new DuplicateCodeDetectionInputDialog(
                shell, "Identical code detection...");
        inputd.open();

        onlyInfile = inputd.onlyInFile();
        minToks = inputd.getMinToks();
        minClones = inputd.getMinClones();

        return inputd.isFinished();
    }
}
