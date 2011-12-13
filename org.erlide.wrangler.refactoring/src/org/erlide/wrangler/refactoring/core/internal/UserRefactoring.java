package org.erlide.wrangler.refactoring.core.internal;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Abstract class for user-defined refactoring handlers
 * 
 * This class should be extended by classes dedicated to composite and
 * elementary refactorings
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public abstract class UserRefactoring extends SimpleOneStepWranglerRefactoring {

    private String callbackModule; // callback module
    private final List<String> parPrompts = new LinkedList<String>(); // parameter
    // prompts
    private List<String> parValues = new ArrayList<String>(0); // parameter
                                                               // values
                                                               // submited by
                                                               // user
    protected boolean fetched; // if parameter prompts are already fetched
    protected RefactoringStatus status; // refactoring status

    @Override
    public RefactoringStatus checkFinalConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        return status;
    }

    public String getCallbackModule() {
        return callbackModule;
    }

    /**
     * Ad hoc refactoring needs name of the callback module to be called
     * 
     * @param module
     */
    public void setCallbackModuleName(final String module) {
        callbackModule = module;
        fetched = false;
    }

    /**
     * Fetch parameter prompts from the right callback module
     * 
     * @param module
     */
    public boolean fetchParPrompts() {
        if (fetched) {
            return true;
        }

        final RpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser("input_par_prompts_eclipse", "s",
                        callbackModule);
        final OtpErlangList params = (OtpErlangList) ((OtpErlangTuple) res
                .getValue()).elementAt(1);
        parPrompts.clear();
        for (final OtpErlangObject obj : params.elements()) {
            parPrompts.add(obj.toString().replace("\"", ""));
        }

        fetched = true;

        return true;
    }

    /**
     * Getter for parameter prompts
     * 
     * @return
     */
    public List<String> getParPrompts() {
        return parPrompts;
    }

    /**
     * Clear parameter prompts list
     */
    protected void clearParPrompts() {
        parPrompts.clear();
    }

    /**
     * Add a new prompt
     * 
     * @param prompt
     */
    protected void addParPrompts(final String prompt) {
        parPrompts.add(prompt);
    }

    /**
     * Getter for parameter values
     * 
     * @return
     */
    public List<String> getParValues() {
        return parValues;
    }

    /**
     * Appends new parameter value
     * 
     * @param value
     */
    public void setParValue(final List<String> params) {
        parValues = params;
    }

    protected OtpErlangList prepareUserInput() {
        final OtpErlangObject[] params = new OtpErlangObject[parValues.size()];
        int i = 0;
        for (final String val : parValues) {
            params[i] = new OtpErlangString(val);
            i++;
        }
        return new OtpErlangList(params);
    }

    /**
     * Defines workflow of the refactoring, sets changed files and status
     * 
     * @param shell
     * @return
     */
    public RefactoringWorkflowController getWorkflowController(final Shell shell) {
        return new RefactoringWorkflowController(shell) {

            @Override
            public void doRefactoring() {
                final IErlSelection sel = GlobalParameters
                        .getWranglerSelection();
                final IRefactoringRpcMessage message = run(sel);
                if (message.isSuccessful()) {
                    changedFiles = message.getRefactoringChangeset();
                    status = new RefactoringStatus();
                } else {
                    status = RefactoringStatus.createFatalErrorStatus(message
                            .getMessageString());
                }
            }

        };
    }

}
