package org.erlide.ui.internal.information;

import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.erlide.backend.BackendCore;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.engine.services.search.OtpDocService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.internal.ErlBrowserInformationControlInput;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.eclipse.text.BrowserInformationControl;
import org.erlide.ui.util.eclipse.text.BrowserInformationControlInput;
import org.erlide.ui.views.EdocView;
import org.erlide.util.ErlangFunctionCall;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangTuple;

public class HandleEdocLinksLocationListener implements LocationListener {
    /**
     *
     */
    private final BrowserInformationControl control;
    private final EdocView edocView;

    public HandleEdocLinksLocationListener(final BrowserInformationControl control) {
        this.control = control;
        edocView = null;
    }

    public HandleEdocLinksLocationListener(final EdocView edocView) {
        control = null;
        this.edocView = edocView;
    }

    @Override
    public void changing(final LocationEvent event) {
        ErlBrowserInformationControlInput input = null;
        if (control != null) {
            final BrowserInformationControlInput input2 = control.getInput();
            if (input2 instanceof ErlBrowserInformationControlInput) {
                input = (ErlBrowserInformationControlInput) input2;
            }
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
            final ErlangFunctionCall functionCall = HoverUtil.eventToErlangFunctionCall(
                    moduleName, event);
            if (functionCall != null) {
                final IErlProject project = ErlangEngine.getInstance()
                        .getModelUtilService().getProject(editor.getModule());
                if (project == null) {
                    return;
                }
                final IOtpRpc backend = BackendCore.getBuildBackend(project);

                final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
                        .toString();
                final OtpErlangTuple otpDoc = (OtpErlangTuple) ErlangEngine.getInstance()
                        .getService(OtpDocService.class)
                        .getOtpDoc(backend, functionCall, stateDir);
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
                        final String html = HoverUtil.getHTMLAndReplaceJSLinks(result);
                        final Object element = new OpenResult(otpDoc.elementAt(2));
                        input = new ErlBrowserInformationControlInput(input, editor,
                                element, html, 20, HoverUtil.getDocumentationURL(docPath,
                                        anchor));
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
                edocView.setInfo(input);
            }
        }
    }

    @Override
    public void changed(final LocationEvent event) {
    }
}
