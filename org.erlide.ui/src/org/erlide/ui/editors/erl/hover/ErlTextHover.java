/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl.hover;

import java.net.URL;
import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IVariable;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.AbstractReusableInformationControlCreator;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextHoverExtension;
import org.eclipse.jface.text.ITextHoverExtension2;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.information.IInformationProviderExtension2;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.EditorsUI;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendManager;
import org.erlide.core.model.erlang.ErlToken;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlPreprocessorDef;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.util.ModelUtils;
import org.erlide.core.services.search.ErlideDoc;
import org.erlide.core.services.search.OpenResult;
import org.erlide.runtime.IRpcSite;
import org.erlide.ui.actions.OpenAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.internal.ErlBrowserInformationControlInput;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.internal.information.ErlInformationPresenter;
import org.erlide.ui.internal.information.HoverUtil;
import org.erlide.ui.internal.information.PresenterControlCreator;
import org.erlide.ui.prefs.plugin.EditorPreferencePage;
import org.erlide.ui.util.eclipse.text.BrowserInformationControl;
import org.erlide.ui.util.eclipse.text.HTMLPrinter;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.Util;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlTextHover implements ITextHover,
        IInformationProviderExtension2, ITextHoverExtension,
        ITextHoverExtension2 {

    private static URL fgStyleSheet = null;
    private IInformationControlCreator fHoverControlCreator;
    private PresenterControlCreator fPresenterControlCreator;
    private final ErlangEditor fEditor;

    public ErlTextHover(final ErlangEditor editor) {
        fEditor = editor;
        initStyleSheet();
    }

    @Override
    public IRegion getHoverRegion(final ITextViewer textViewer, final int offset) {
        return internalGetHoverRegion(offset, fEditor);
    }

    private static IRegion internalGetHoverRegion(final int offset,
            final ErlangEditor editor) {
        if (editor == null) {
            return null;
        }
        editor.reconcileNow();
        final ErlToken token = editor.getScanner().getTokenAt(offset);
        if (token == null) {
            return null;
        }
        return new Region(token.getOffset(), token.getLength());
    }

    private void initStyleSheet() {
        final Bundle bundle = Platform.getBundle(ErlideUIPlugin.PLUGIN_ID);
        if (fgStyleSheet != null) {
            return;
        }
        fgStyleSheet = bundle.getEntry("/edoc.css"); //$NON-NLS-1$
        if (fgStyleSheet != null) {

            try {
                fgStyleSheet = FileLocator.toFileURL(fgStyleSheet);
            } catch (final Exception e) {
            }
        }
    }

    /**
     * @param textViewer
     * @param offset
     * @param length
     */
    private static String makeDebuggerVariableHover(
            final ITextViewer textViewer, final int offset, final int length) {
        final IAdaptable adaptable = DebugUITools.getDebugContext();
        if (adaptable != null) {
            final IStackFrame frame = (IStackFrame) adaptable
                    .getAdapter(IStackFrame.class);
            try {
                if (frame != null && frame.hasVariables()) {
                    String varName = "";
                    try {
                        varName = textViewer.getDocument().get(offset, length);
                    } catch (final BadLocationException e) {
                    }
                    if (varName.length() > 0) {
                        final String firstLetter = varName.substring(0, 1);
                        if (firstLetter.toUpperCase().equals(firstLetter)) {
                            final IVariable[] vars = frame.getVariables();
                            for (final IVariable variable : vars) {
                                if (variable.getName().equals(varName)) {
                                    final String value = variable.getValue()
                                            .getValueString();
                                    return makeVariablePresentation(varName,
                                            value);
                                }
                            }
                        }
                    }
                }
            } catch (final DebugException e) {
            }
        }
        return "";
    }

    private static String makeVariablePresentation(final String varName,
            final String value) {
        return varName + " = " + value;
    }

    @Override
    public IInformationControlCreator getInformationPresenterControlCreator() {
        if (fPresenterControlCreator == null) {
            fPresenterControlCreator = new PresenterControlCreator(fEditor);
        }
        return fPresenterControlCreator;
    }

    @Override
    public IInformationControlCreator getHoverControlCreator() {
        if (fHoverControlCreator == null) {
            fHoverControlCreator = new HoverControlCreator(
                    getInformationPresenterControlCreator());
        }
        return fHoverControlCreator;
    }

    public static final class HoverControlCreator extends
            AbstractReusableInformationControlCreator {
        IInformationControlCreator fInformationPresenterControlCreator;

        public HoverControlCreator(
                final IInformationControlCreator informationPresenterControlCreator) {
            fInformationPresenterControlCreator = informationPresenterControlCreator;
        }

        @Override
        protected IInformationControl doCreateInformationControl(
                final Shell parent) {
            IInformationControl control;
            if (BrowserInformationControl.isAvailable(parent)) {
                control = new BrowserInformationControl(parent,
                        JFaceResources.DIALOG_FONT,
                        EditorsUI.getTooltipAffordanceString()) {
                    @Override
                    public IInformationControlCreator getInformationPresenterControlCreator() {
                        return fInformationPresenterControlCreator;
                    }

                    @Override
                    public void setSize(int width, int height) {
                        // default size is too small
                        final Point bounds = getSizeConstraints();
                        if (bounds != null) {
                            if (bounds.x != SWT.DEFAULT) {
                                width = Math.min(bounds.x, width * 2);
                            }
                            if (bounds.y != SWT.DEFAULT) {
                                height = Math.min(bounds.y, height * 2);
                            }
                        }
                        super.setSize(width, height);
                    }
                };
            } else {
                control = new DefaultInformationControl(parent,
                        EditorsUI.getTooltipAffordanceString(),
                        new ErlInformationPresenter(true));
            }
            return control;
        }
    }

    public static ErlBrowserInformationControlInput getHoverInfoForOffset(
            final int offset, final ErlangEditor editor) {
        final ITextViewer textViewer = editor.getViewer();
        final IRegion region = internalGetHoverRegion(offset, editor);
        if (region != null) {
            return internalGetHoverInfo(editor, textViewer, region);
        }
        return null;
    }

    @Override
    public String getHoverInfo(final ITextViewer textViewer,
            final IRegion hoverRegion) {
        if (isHoverDisabled()) {
            return null;
        }
        final ErlBrowserInformationControlInput input = (ErlBrowserInformationControlInput) getHoverInfo2(
                textViewer, hoverRegion);
        return input == null ? "" : input.getHtml();
    }

    private static boolean isHoverDisabled() {
        return !EditorPreferencePage.getEnableHover();
    }

    @Override
    public Object getHoverInfo2(final ITextViewer textViewer,
            final IRegion hoverRegion) {
        if (isHoverDisabled()) {
            return null;
        }
        return internalGetHoverInfo(fEditor, textViewer, hoverRegion);
    }

    private static ErlBrowserInformationControlInput internalGetHoverInfo(
            final ErlangEditor editor, final ITextViewer textViewer,
            final IRegion hoverRegion) {
        if (editor == null) {
            return null;
        }
        final IErlModule module = editor.getModule();
        if (module == null) {
            return null;
        }
        final StringBuffer result = new StringBuffer();
        Object element = null;
        // TODO our model is too coarse, here we need access to expressions
        final Collection<OtpErlangObject> fImports = ModelUtils
                .getImportsAsList(module);

        final int offset = hoverRegion.getOffset();
        final int length = hoverRegion.getLength();
        final String debuggerVar = makeDebuggerVariableHover(textViewer,
                offset, length);
        if (debuggerVar.length() > 0) {
            result.append(debuggerVar);
        }
        final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
                .toString();
        final IErlProject erlProject = ModelUtils.getProject(module);

        final IBackendManager backendManager = BackendCore.getBackendManager();
        final IBackend ide = backendManager.getIdeBackend();
        String docPath = "";
        String anchor = "";
        try {
            final IProject project = erlProject == null ? null : erlProject
                    .getWorkspaceProject();
            final IRpcSite b = erlProject == null ? ide.getRpcSite()
                    : backendManager.getBuildBackend(project).getRpcSite();

            final IErlModel model = ErlModelManager.getErlangModel();
            final String externalModulesString = erlProject != null ? erlProject
                    .getExternalModulesString() : null;
            final OtpErlangTuple t = (OtpErlangTuple) ErlideDoc.getOtpDoc(
                    ide.getRpcSite(), b, offset, stateDir,
                    module.getScannerName(), fImports, externalModulesString,
                    model.getPathVars());
            // ErlLogger.debug("otp doc %s", t);
            if (Util.isOk(t)) {
                element = new OpenResult(t.elementAt(2));
                final String docStr = Util.stringValue(t.elementAt(1));
                result.append(docStr);
                if (t.arity() > 4) {
                    docPath = Util.stringValue(t.elementAt(3));
                    anchor = Util.stringValue(t.elementAt(4));
                }
            } else {
                final OpenResult or = new OpenResult(t);
                element = or;
                final Object found = OpenAction.findOpenResult(editor, module,
                        b, erlProject, or, offset);
                // ErlLogger.debug("found:" + found);
                if (found instanceof IErlFunction) {
                    final IErlFunction function = (IErlFunction) found;
                    final String comment = HoverUtil
                            .getDocumentationString(function.getComments());
                    if (comment.length() == 0) {
                        return null;
                    }
                    result.append(HTMLPrinter.asHtml(comment));
                } else if (found instanceof IErlPreprocessorDef) {
                    final IErlPreprocessorDef preprocessorDef = (IErlPreprocessorDef) found;
                    result.append(preprocessorDef.getExtra());
                }
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
            return null;
        }
        final String strResult = HoverUtil.getHTMLAndReplaceJSLinks(result);
        return new ErlBrowserInformationControlInput(null, editor, element,
                strResult, 20, docPath, anchor);
    }
}
