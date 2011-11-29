package org.erlide.wrangler.refactoring.core.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.core.rpc.IRpcResult;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.CommandData;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Logic for the composite refactoring
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class ApplyCompositeRefactoring extends UserAdhocRefactoring {

    protected RefactoringStatus status;

    private CommandData command = new CommandData(); // all the data about
                                                     // cuddent command
    private boolean finished = false;

    @Override
    public IRefactoringRpcMessage run(IErlSelection selection) {

        command.addUserInput(getParValues());
        command.addTabWidth(GlobalParameters.getTabWidth());

        return WranglerBackendManager.getRefactoringBackend().call(
                "get_next_command_eclipse", "sx", command.command,
                new OtpErlangList(command.args));
    }

    public boolean fetchNextCommand() {
        IRpcResult res = WranglerBackendManager
                .getRefactoringBackend()
                .callWithoutParser(
                        "get_next_command_eclipse",
                        "x",
                        new OtpErlangList(new OtpErlangObject[] {
                                new OtpErlangAtom("ok"), new OtpErlangList() }));

        if (((OtpErlangTuple) res.getValue()).elementAt(0).toString()
                .equals("ok")) {
            finished = true;
            return false;
        } else if (((OtpErlangTuple) res.getValue()).elementAt(0).toString()
                .equals("next")) {
            OtpErlangTuple tuple = (OtpErlangTuple) ((OtpErlangTuple) res
                    .getValue()).elementAt(1);
            command.type = tuple.elementAt(0).toString().replace("\"", "");
            command.msg = tuple.elementAt(1).toString().replace("\"", "");
            command.command = tuple.elementAt(2).toString().replace("\"", "");
            command.args = ((OtpErlangList) tuple.elementAt(3)).elements();

            clearParPrompts();
            for (OtpErlangObject arg : command.args) {
                if (arg instanceof OtpErlangTuple
                        && ((OtpErlangTuple) arg).elementAt(0).toString()
                                .equals("prompt")) {
                    addParPrompts(((OtpErlangTuple) arg).elementAt(1)
                            .toString().replace("\"", ""));
                }
            }
            // new OtpErlangInt(GlobalParameters.getTabWidth()) na koncu
            // atrybutow ehi

            // yesno :P
            // arg

            return true;

        } else {
            return false;
            // error
        }

    }

    public boolean startCompositeRefactoring() {
        final IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
                .getWranglerSelection();

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
                prepareUserInput(), sel.getSearchPath(),
                new OtpErlangInt(GlobalParameters.getTabWidth()) });

        IRpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser("init_composite_refac_eclipse", "sx",
                        getCallbackModule(), args);
        return res.isOk();
    }

    public RefactoringWorkflowController getWorkflowController(final Shell shell) {
        return new RefactoringWorkflowController(shell) {

            @Override
            public void doRefactoring() {
                final IErlSelection sel = GlobalParameters
                        .getWranglerSelection();
                // TODO complete
                IRefactoringRpcMessage message = run(sel);
                if (message.isSuccessful() && finished) {
                    changedFiles = message.getRefactoringChangeset();
                    status = new RefactoringStatus();
                } else {
                    status = RefactoringStatus.createFatalErrorStatus(message
                            .getMessageString());
                }

            }

        };
    }

    // add custom workflow
    // enable adding new pages
    // check what should be returned by each refactoring
    // check what should be changed in wrangler code

    @Override
    public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        return new RefactoringStatus(); // OK, no preconditins
    }

    @Override
    public String getName() {
        return "Apply composite refactoring";
    }

    @Override
    public RefactoringStatus checkFinalConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        return status;
    }

    public CommandData getCommand() {
        return command;
    }

}
