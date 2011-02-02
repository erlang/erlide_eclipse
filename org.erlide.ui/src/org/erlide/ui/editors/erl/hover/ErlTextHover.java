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
import org.erlide.backend.util.StringUtils;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.information.ErlInformationPresenter;
import org.erlide.ui.information.PresenterControlCreator;
import org.erlide.ui.internal.ErlBrowserInformationControlInput;
import org.erlide.ui.util.eclipse.text.BrowserInformationControl;
import org.erlide.ui.util.eclipse.text.HTMLPrinter;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDoc;
import erlang.OpenResult;

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

    public IRegion getHoverRegion(final ITextViewer textViewer, final int offset) {
        if (fEditor == null) {
            return null;
        }
        fEditor.reconcileNow();
        final ErlToken token = fEditor.getModule().getScannerTokenAt(offset);
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

    public IInformationControlCreator getInformationPresenterControlCreator() {
        if (fPresenterControlCreator == null) {
            fPresenterControlCreator = new PresenterControlCreator(fEditor);
        }
        return fPresenterControlCreator;
    }

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
                        // TODO default size is too small
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

    public static String getHoverTextForOffset(final int offset,
            final ErlangEditor editor) {
        final ErlTextHover h = new ErlTextHover(editor);
        final ITextViewer tv = editor.getViewer();
        final IRegion r = h.getHoverRegion(tv, offset);
        if (r != null) {
            return h.getHoverInfo(tv, r);
        }
        return null;
    }

    public String getHoverInfo(final ITextViewer textViewer,
            final IRegion hoverRegion) {
        final ErlBrowserInformationControlInput input = (ErlBrowserInformationControlInput) getHoverInfo2(
                textViewer, hoverRegion);
        return input == null ? "" : input.getHtml();
    }

    public Object getHoverInfo2(final ITextViewer textViewer,
            final IRegion hoverRegion) {
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
        // try {
        // element = module.getElementAt(hoverRegion.getOffset());
        // } catch (Exception e) {
        // }
        final Collection<OtpErlangObject> fImports = ModelUtils
                .getImportsAsList(module);

        final int offset = hoverRegion.getOffset();
        OtpErlangObject r1 = null;
        final int length = hoverRegion.getLength();
        final String debuggerVar = makeDebuggerVariableHover(textViewer,
                offset, length);
        if (debuggerVar.length() > 0) {
            result.append(debuggerVar);
        }
        final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
                .toString();
        final IErlProject erlProject = module.getProject();

        final BackendManager backendManager = ErlangCore.getBackendManager();
        final Backend ide = backendManager.getIdeBackend();
        try {
            final IProject project = erlProject == null ? null : erlProject
                    .getProject();
            final Backend b = erlProject == null ? ide : backendManager
                    .getBuildBackend(project);

            final IErlModel model = ErlangCore.getModel();
            r1 = ErlideDoc.getOtpDoc(ide, b, offset, stateDir,
                    ErlangToolkit.createScannerModuleName(module), fImports,
                    model.getExternalModules(erlProject), model.getPathVars());
            // ErlLogger.debug("getHoverInfo getDocFromScan " + r1);
            final OtpErlangTuple t = (OtpErlangTuple) r1;
            if (Util.isOk(t)) {
                final String docStr = Util.stringValue(t.elementAt(1));
                final OpenResult or = new OpenResult(t.elementAt(2));
                result.append(docStr);
                element = or;
            } else {
                // TODO here we should check like in 'open'
                final OtpErlangObject o0 = t.elementAt(0);
                final OtpErlangObject o1 = t.elementAt(1);
                if (o0 instanceof OtpErlangAtom && o1 instanceof OtpErlangAtom) {
                    final OtpErlangAtom a0 = (OtpErlangAtom) o0;
                    final OtpErlangAtom a1 = (OtpErlangAtom) o1;
                    final String openKind = a0.atomValue();
                    if (openKind.equals("error")) {
                        return null;
                    }
                    String definedName = a1.atomValue();
                    // TODO code below should be cleaned up, we should factorize
                    // and
                    // use same code for content assist, open and hover
                    if (openKind.equals("local") || openKind.equals("external")) {
                        IErlModule module2 = null;
                        OtpErlangLong arityLong = null;
                        if (openKind.equals("local")) {
                            arityLong = (OtpErlangLong) t.elementAt(2);
                            module2 = module;
                        } else if (openKind.equals("external")) {
                            final OtpErlangAtom a2 = (OtpErlangAtom) t
                                    .elementAt(2);
                            final String mod = ModelUtils.resolveMacroValue(
                                    definedName, module);
                            definedName = a2.atomValue();
                            module2 = ModelUtils.findExternalModule(mod, null,
                                    erlProject, true);
                        }
                        if (module2 == null) {
                            return null;
                        }
                        int arity = -1;
                        try {
                            if (arityLong != null) {
                                arity = arityLong.intValue();
                            }
                        } catch (final OtpErlangRangeException e) {
                        }
                        final ErlangFunction erlangFunction = new ErlangFunction(
                                definedName, arity);
                        IErlFunction f = null;
                        try {
                            module2.open(null);
                            f = ModelUtils
                                    .findFunction(module2, erlangFunction);
                        } catch (final ErlModelException e) {
                        }
                        if (f == null) {
                            return null;
                        }
                        final String comment = f.getComment();
                        if (comment == null) {
                            return null;
                        }
                        result.append(comment);
                    } else {
                        if (definedName.charAt(0) == '?') {
                            definedName = definedName.substring(1);
                        }
                        final IErlElement.Kind kindToFind = openKind
                                .equals("record") ? IErlElement.Kind.RECORD_DEF
                                : IErlElement.Kind.MACRO_DEF;
                        final String externalIncludes = model
                                .getExternalIncludes(erlProject);
                        IErlPreprocessorDef pd = ModelUtils
                                .findPreprocessorDef(module, definedName,
                                        kindToFind, externalIncludes);
                        if (pd == null) {
                            pd = ModelUtils.findPreprocessorDef(module,
                                    StringUtils.unquote(definedName),
                                    kindToFind, externalIncludes);
                        }
                        if (pd != null) {
                            element = pd;
                            result.append(pd.getExtra());
                        }
                    }
                }
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        if (result.length() > 0) {
            HTMLPrinter.insertPageProlog(result, 0, fgStyleSheet);
            HTMLPrinter.addPageEpilog(result);
        }
        // TODO set element
        return new ErlBrowserInformationControlInput(null, element,
                result.toString(), 20);
    }

}
