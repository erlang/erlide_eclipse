package org.erlide.wrangler.refactoring.core.internal;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.core.CoreScope;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.rpc.IRpcResult;
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
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class ApplyAdhocRefactoring extends SimpleOneStepWranglerRefactoring {

    private String callbackModule; // callback module
    private List<String> parPrompts = new LinkedList<String>(); // parameter
                                                                // prompts
    private List<String> parValues = new LinkedList<String>(); // parameter
                                                               // values
                                                               // submited by
                                                               // user
    private boolean fetched; // if parameter prompts are already fetched

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
                new OtpErlangString(prepareUserInput()), sel.getSearchPath(),
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
        fetched = false;
    }

    /**
     * Fetch parameter prompts from the right callback module
     * 
     * @param module
     */
    public boolean fetchParPrompts() {
        if (fetched)
            return true;

        String callbackPath;
        try {
            if (CoreScope.getModel().findModule(callbackModule) == null)
                return false;

            IErlProject project = CoreScope.getModel()
                    .findModule(callbackModule).getProject();
            callbackPath = project.getWorkspaceProject().getLocation()
                    .append(project.getOutputLocation()).toString();
        } catch (ErlModelException e) {
            return false;
        }

        IRpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser("load_callback_mod_eclipse", "ss",
                        callbackModule, callbackPath);
        if (!res.isOk())
            return false;

        res = WranglerBackendManager.getRefactoringBackend().callWithoutParser(
                "input_par_prompts_eclipse", "s", callbackModule);
        OtpErlangList params = (OtpErlangList) ((OtpErlangTuple) res.getValue())
                .elementAt(1);
        parPrompts.clear();
        for (OtpErlangObject obj : params.elements())
            parPrompts.add(obj.toString());

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
    public void addParValue(String value) {
        parValues.add(value);
    }

    private String prepareUserInput() {
        StringBuffer buf = new StringBuffer();
        for (String val : parValues)
            buf.append(val).append(" ");
        return buf.toString().trim();
    }

}
