package org.erlide.ui.internal.information;

import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.erlide.backend.BackendCore;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.views.EdocView;
import org.erlide.util.ErlangFunctionCall;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangTuple;

/*
 * Disabled for now, possibly causes an infinite loop (end of 'changing')
 */
public class HandleEdocLinksLocationListener implements LocationListener {
    /**
     *
     */
    private final ErlangBrowserInformationControl control;
    private final EdocView edocView;

    public HandleEdocLinksLocationListener(
            final ErlangBrowserInformationControl control) {
        this.control = control;
        edocView = null;
    }

    public HandleEdocLinksLocationListener(final EdocView edocView) {
        control = null;
        this.edocView = edocView;
    }

    @Override
    public void changing(final LocationEvent event) {
        ErlangBrowserInformationControlInput input = null;
        if (control != null) {
            input = control.getInput();
        } else if (edocView != null) {
            input = edocView.getInput();
        }
        if (input != null) {
            final AbstractErlangEditor editor = input.getEditor();
            String moduleName = "";
            final Object inputElement = input.getInputElement();
            if (inputElement instanceof OpenResult) {
                final OpenResult or = (OpenResult) inputElement;
                moduleName = or.getName();
            }
            final ErlangFunctionCall functionCall = HoverUtil
                    .eventToErlangFunctionCall(moduleName, event);
            if (functionCall != null) {
                final IErlProject project = ErlangEngine.getInstance()
                        .getModelUtilService().getProject(editor.getModule());
                if (project == null) {
                    return;
                }
                final IOtpRpc backend = BackendCore.getBuildBackend(project);

                final OtpErlangTuple otpDoc = (OtpErlangTuple) ErlangEngine.getInstance()
                        .getOtpDocService().getOtpDoc(backend, functionCall);
                if (Util.isOk(otpDoc)) {
                    final String docStr = Util.stringValue(otpDoc.elementAt(1));
                    final StringBuffer result = new StringBuffer(docStr);
                    String docPath = "";
                    String anchor = "";
                    if (otpDoc.arity() > 4) {
                        docPath = Util.stringValue(otpDoc.elementAt(3));
                        anchor = Util.stringValue(otpDoc.elementAt(4));
                    }
                    if (result.length() > 0) {
                        final String html = HoverUtil.getHTML(result);
                        final Object element = new OpenResult(otpDoc.elementAt(2));
                        input = new ErlangBrowserInformationControlInput(input, editor,
                                element, html, 20,
                                HoverUtil.getDocumentationURL(docPath, anchor));
                    }
                }
            }
        }
        if (input != null) {
            if (control != null) {
                if (control.hasDelayedInputChangeListener()) {
                    control.notifyDelayedInputChange(input);
                } else {
                    control.setInput(input);
                }
            } else if (edocView != null) {
                // This might cause an infinite event loop
                // edocView.setInfo(input);
            }
        }
    }

    @Override
    public void changed(final LocationEvent event) {
    }
}
