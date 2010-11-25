/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.actions;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.core.erlang.util.PluginUtils;
import org.erlide.core.erlang.util.ResourceUtil;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.prefs.plugin.NavigationPreferencePage;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;

import erlang.ErlideOpen;
import erlang.OpenResult;

/**
 * This action opens a Erlang editor on a Erlang element or file.
 * <p>
 * The action is applicable to selections containing elements of type
 * <code>ICompilationUnit</code>, <code>IMember</code> or <code>IFile</code>.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 2.0
 */
public class OpenAction extends SelectionDispatchAction {

    /**
     * Creates a new <code>OpenAction</code>. The action requires that the
     * selection provided by the site's selection provider is of type <code>
     * org.eclipse.jface.viewers.IStructuredSelection</code> .
     * 
     * @param site
     *            the site providing context information for this action
     * @param externalModules
     *            the externalModules file that can be searched for references
     *            to external modules
     */
    public OpenAction(final IWorkbenchSite site) {
        super(site);
        setText(ActionMessages.OpenAction_label);
        setToolTipText(ActionMessages.OpenAction_tooltip);
        setDescription(ActionMessages.OpenAction_description);
        PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "erl.open");

    }

    public OpenAction(final ErlangEditor erlangEditor) {
        super(erlangEditor.getSite());
        setText(ActionMessages.OpenAction_open_declaration_label);
        setToolTipText(ActionMessages.OpenAction_tooltip);
        setDescription(ActionMessages.OpenAction_description);
        PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "erl.open");
    }

    /*
     * (non-Javadoc) Method declared on SelectionDispatchAction.
     */
    @Override
    public void selectionChanged(final ITextSelection selection) {
    }

    /*
     * (non-Javadoc) Method declared on SelectionDispatchAction.
     */
    @Override
    public void selectionChanged(final IStructuredSelection selection) {
        setEnabled(checkEnabled(selection));
    }

    private boolean checkEnabled(final IStructuredSelection selection) {
        if (selection.isEmpty()) {
            return false;
        }
        for (final Object element : selection.toArray()) {
            if (element instanceof ISourceReference) {
                continue;
            }
            if (element instanceof IFile) {
                continue;
            }
            if (element instanceof IStorage) {
                continue;// FIXME We don't handle IStorage, do we?
            }
            if (element instanceof IErlModule) {
                continue;
            }
            return false;
        }
        return true;
    }

    /*
     * (non-Javadoc) Method declared on SelectionDispatchAction.
     */
    @Override
    public void run(final ITextSelection selection) {
        final ErlangEditor editor = (ErlangEditor) getSite().getPage()
                .getActiveEditor();
        editor.reconcileNow();
        final IErlModule module = editor.getModule();
        if (module == null) {
            return;
        }
        final Backend b = ErlangCore.getBackendManager().getIdeBackend();
        final int offset = selection.getOffset();
        try {
            final IErlProject erlProject = module.getErlProject();
            final IErlModel model = ErlangCore.getModel();
            final OpenResult res = ErlideOpen.open(b,
                    ErlangToolkit.createScannerModuleName(module), offset,
                    ErlModelUtils.getImportsAsList(module),
                    model.getExternalModules(erlProject), model.getPathVars());
            ErlLogger.debug("open " + res);
            openOpenResult(editor, module, b, offset, erlProject, res);
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }

    /*
     * (non-Javadoc) Method declared on SelectionDispatchAction.
     */
    @Override
    public void run(final IStructuredSelection selection) {
        if (!checkEnabled(selection)) {
            return;
        }
        for (final Object i : selection.toArray()) {
            if (i instanceof IErlElement) {
                try {
                    ErlModelUtils.openElement((IErlElement) i);
                } catch (final CoreException e) {
                    ErlLogger.error(e);
                }
            }
        }
    }

    public static void openOpenResult(final ErlangEditor editor,
            final IErlModule module, final Backend backend, final int offset,
            final IErlProject erlProject, final OpenResult res)
            throws CoreException, ErlModelException, PartInitException,
            BadLocationException, OtpErlangRangeException, BackendException {
        final IErlModel model = ErlangCore.getModel();
        final IProject project = erlProject == null ? null : erlProject
                .getProject();
        final IErlElement element = editor.getElementAt(offset, true);
        IErlElement foundElement = null;
        ISourceRange foundSourceRange = null;
        final boolean checkAllProjects = NavigationPreferencePage
                .getCheckAllProjects();
        if (res.isExternalCall()) {
            foundElement = findExternalCallOrType(module, res, project,
                    element, checkAllProjects);
        } else if (res.isInclude()) {
            foundElement = findInclude(module, erlProject, res, model, project);
        } else if (res.isLocalCall()) {
            foundElement = findLocalCall(module, backend, erlProject, res,
                    project, element, checkAllProjects);
        } else if (res.isVariable() && element instanceof ISourceReference) {
            final ISourceReference sref = (ISourceReference) element;
            final ISourceRange range = sref.getSourceRange();
            final String elementText = editor.getDocument().get(
                    range.getOffset(), range.getLength());
            foundSourceRange = ErlModelUtils.findVariable(backend, range,
                    res.getName(), elementText);
        } else if (res.isRecord() || res.isMacro()) {
            final IErlElement.Kind kind = res.isMacro() ? IErlElement.Kind.MACRO_DEF
                    : IErlElement.Kind.RECORD_DEF;
            foundElement = ErlModelUtils.findPreprocessorDef(backend, project,
                    module, res.getName(), kind,
                    model.getExternalIncludes(erlProject));
        }
        if (foundElement != null) {
            ErlModelUtils.openElement(foundElement);
        } else if (foundSourceRange != null) {
            ErlModelUtils.openSourceRange(module, foundSourceRange);
        }
    }

    private static IErlElement findInclude(final IErlModule module,
            final IErlProject erlProject, final OpenResult res,
            final IErlModel model, final IProject project) throws CoreException {
        IContainer parent = null;
        if (module != null) {
            final IResource resource = module.getResource();
            parent = resource.getParent();
        }
        final IResource r = ResourceUtil
                .recursiveFindNamedResourceWithReferences(project,
                        res.getName(),
                        PluginUtils.getIncludePathFilter(project, parent));
        if (r instanceof IFile) {
            final IFile file = (IFile) r;
            return model.findModule(file);
        } else {
            final String includeFile = ModelUtils.findIncludeFile(project,
                    res.getName(), model.getExternalIncludes(erlProject));
            if (includeFile != null) {
                return ModelUtils.openExternal(project, includeFile);
            }
        }
        return null;
    }

    private static IErlElement findLocalCall(final IErlModule module,
            final Backend backend, final IErlProject erlProject,
            final OpenResult res, final IProject project,
            final IErlElement element, final boolean checkAllProjects)
            throws ErlModelException, BackendException {
        if (ErlModelUtils.isTypeDefOrRecordDef(element)) {
            return ModelUtils.findTypespec(module, res.getFun());
        }
        final IErlFunction foundElement = ModelUtils.findFunction(module,
                res.getFunction());
        if (foundElement != null) {
            return foundElement;
        }
        // not local imports
        OtpErlangObject res2 = null;
        String moduleName = null;
        if (module != null) {
            final IErlImport ei = module.findImport(res.getFunction());
            if (ei != null) {
                moduleName = ei.getImportModule();
                final IErlModel model = ErlangCore.getModel();
                res2 = ErlideOpen.getSourceFromModule(backend,
                        model.getPathVars(), moduleName,
                        model.getExternalModules(erlProject));
            }
        }
        if (res2 instanceof OtpErlangString && moduleName != null) {
            final OtpErlangString otpErlangString = (OtpErlangString) res2;
            final String modulePath = otpErlangString.stringValue();
            return ErlModelUtils.findExternalFunction(moduleName,
                    res.getFunction(), modulePath, project, checkAllProjects,
                    module);
        } else {
            return ErlModelUtils.findExternalFunction(moduleName,
                    res.getFunction(), null, project, checkAllProjects, module);
        }
    }

    private static IErlElement findExternalCallOrType(final IErlModule module,
            final OpenResult res, final IProject project,
            final IErlElement element, final boolean checkAllProjects)
            throws CoreException {
        if (ErlModelUtils.isTypeDefOrRecordDef(element)) {
            return ErlModelUtils.findExternalType(module, res.getName(),
                    res.getFun(), res.getPath(), project, checkAllProjects);
        }
        final IErlElement result = ErlModelUtils.findExternalFunction(
                res.getName(), res.getFunction(), res.getPath(), project,
                checkAllProjects, module);
        if (result instanceof IErlFunction) {
            return result;
        }
        return ErlModelUtils.findExternalFunction(res.getName(),
                new ErlangFunction(res.getFun(), ErlangFunction.ANY_ARITY),
                res.getPath(), project, checkAllProjects, module);
    }

}
