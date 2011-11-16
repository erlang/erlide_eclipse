package org.erlide.wrangler.refactoring.core.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class ApplyAdhocRefactoring extends SimpleOneStepWranglerRefactoring {

    private String callbackModule; // callback module

    @Override
    public IRefactoringRpcMessage run(IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;

        OtpErlangList pos = new OtpErlangList(new OtpErlangInt[] {
                new OtpErlangInt(sel.getSelectionRange().getStartLine()),
                new OtpErlangInt(sel.getSelectionRange().getStartCol()) });
        OtpErlangList selectionBeg = new OtpErlangList(new OtpErlangInt[] {
                new OtpErlangInt(sel.getSelectionRange().getStartLine()),
                new OtpErlangInt(sel.getSelectionRange().getStartCol()) });
        OtpErlangList selectionEnd = new OtpErlangList(new OtpErlangInt[] {
                new OtpErlangInt(sel.getSelectionRange().getEndLine()),
                new OtpErlangInt(sel.getSelectionRange().getEndCol()) });
        OtpErlangList selectionPos = new OtpErlangList(new OtpErlangObject[] {
                selectionBeg, selectionEnd });
        OtpErlangList args = new OtpErlangList(new OtpErlangObject[] {
                new OtpErlangString(sel.getFilePath()), pos, selectionPos,
                new OtpErlangString(userInput), sel.getSearchPath(),
                new OtpErlangInt(GlobalParameters.getTabWidth()) });

        return WranglerBackendManager.getRefactoringBackend().call(
                "rename_var_eclipse", "sx", callbackModule, args);
    }

    @Override
    public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        return new RefactoringStatus(); // OK, no preconditins
    }

    @Override
    public String getName() {
        return "Apply ad hoc refactoring";
    }

    /**
     * Ad hoc refactoring needs name of the callback module to be called
     * 
     * @param module
     */
    public void setCallbackModuleName(String module) {
        callbackModule = module;
    }

}
