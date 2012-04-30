package org.erlide.tracing.core.mvc.view;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.DialogCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.erlide.tracing.core.Constants;
import org.erlide.tracing.core.mvc.model.MatchSpec;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Cell editor for specifying match specifications.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class MatchSpecCellEditor extends DialogCellEditor {

    public MatchSpecCellEditor(final Composite parent) {
        super(parent);
    }

    @Override
    protected Object openDialogBox(final Control cellEditorWindow) {
        final MatchSpecInputDialog dialog = new MatchSpecInputDialog(
                cellEditorWindow.getShell(), "Create match spec",
                "Literal fun:", ((MatchSpec) getValue()).getFunctionString(),
                new MatchSpecValidator());
        dialog.open();
        return getValue();
    }

    private class MatchSpecInputDialog extends InputDialog {

        public MatchSpecInputDialog(final Shell parentShell,
                final String dialogTitle, final String dialogMessage,
                final String initialValue, final IInputValidator validator) {
            super(parentShell, dialogTitle, dialogMessage, initialValue,
                    validator);
        }

        @Override
        protected int getInputTextStyle() {
            return SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL;
        }
    }

    private class MatchSpecValidator implements IInputValidator {

        @Override
        public String isValid(final String newText) {
            if (newText == null || newText.length() == 0) {
                // no match spec
                ((MatchSpec) getValue()).setFunctionString("");
                ((MatchSpec) getValue()).setMsObject(null);
                return null;
            } else if ("x".equals(newText)) {
                // shortcut for matching exceptions and return values
                ((MatchSpec) getValue()).setFunctionString("x");
                ((MatchSpec) getValue()).setMsObject(new OtpErlangAtom("x"));
                return null;
            }
            try {
                final IBackend backend = BackendCore.getBackendManager()
                        .getIdeBackend();
                final OtpErlangTuple tuple = (OtpErlangTuple) backend.call(
                        Constants.ERLANG_HELPER_MODULE, Constants.FUN_STR2MS,
                        "s", new OtpErlangString(newText));
                if (((OtpErlangAtom) tuple.elementAt(0)).atomValue().equals(
                        "ok")) {
                    // correct match spec - update
                    ((MatchSpec) getValue()).setFunctionString(newText);
                    ((MatchSpec) getValue()).setMsObject(tuple.elementAt(1));
                    return null;
                } else {
                    // incorrect match spec
                    final OtpErlangAtom errorType = (OtpErlangAtom) tuple
                            .elementAt(1);
                    if (errorType.atomValue().equals("standard_info")) {
                        final OtpErlangTuple errorTuple = (OtpErlangTuple) tuple
                                .elementAt(2);
                        final StringBuilder builder = new StringBuilder("Line ");
                        builder.append(errorTuple.elementAt(0)).append(": ");
                        final OtpErlangList errorList = (OtpErlangList) errorTuple
                                .elementAt(2);
                        builder.append(((OtpErlangString) errorList
                                .elementAt(0)).stringValue());
                        if (errorList.elementAt(1) instanceof OtpErlangString) {
                            builder.append(((OtpErlangString) errorList
                                    .elementAt(1)).stringValue());
                        }
                        return builder.toString();
                    } else if (errorType.atomValue().equals("not_fun")) {
                        return "Given expression is not a function";
                    } else if (errorType.atomValue().equals("unbound_var")) {
                        final StringBuilder builder = new StringBuilder(
                                "Unbound variable: ");
                        builder.append(tuple.elementAt(2));
                        return builder.toString();
                    } else {
                        return tuple.elementAt(2).toString();
                    }
                }
            } catch (final Exception e) {
                ErlLogger.error(e);
                return "Backend problem: " + e.getMessage();
            }
        }
    }
}
