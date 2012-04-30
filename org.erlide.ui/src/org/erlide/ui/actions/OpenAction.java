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

import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.core.internal.model.erlang.ModelInternalUtils;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlImport;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.erlang.ISourceReference;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.core.model.util.ModelUtils;
import org.erlide.core.services.search.ErlideOpen;
import org.erlide.core.services.search.OpenResult;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.prefs.plugin.NavigationPreferencePage;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;

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

    @Override
    public void selectionChanged(final ITextSelection selection) {
    }

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

    @Override
    public void run(final ITextSelection selection) {
        final ErlangEditor editor = (ErlangEditor) getSite().getPage()
                .getActiveEditor();
        editor.reconcileNow();
        final IErlModule module = editor.getModule();
        if (module == null) {
            return;
        }
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        final int offset = selection.getOffset();
        try {
            final IErlProject project = module.getProject();
            final IErlModel model = ErlModelManager.getErlangModel();
            final String externalModulesString = project == null ? "" : project
                    .getExternalModulesString();
            final OpenResult res = ErlideOpen.open(b, module, offset,
                    ModelUtils.getImportsAsList(module), externalModulesString,
                    model.getPathVars());
            ErlLogger.debug("open " + res);
            openOpenResult(editor, module, b, offset, project, res);
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }

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
            final IErlModule module, final IBackend backend, final int offset,
            final IErlProject erlProject, final OpenResult res)
            throws CoreException, ErlModelException, PartInitException,
            BadLocationException, OtpErlangRangeException, BackendException,
            RpcException {
        final Object found = findOpenResult(editor, module, backend,
                erlProject, res, offset);
        if (found instanceof IErlElement) {
            ErlModelUtils.openElement((IErlElement) found);
        } else if (found instanceof ISourceRange) {
            ErlModelUtils.openSourceRange(module, (ISourceRange) found);
        }
    }

    public static Object findOpenResult(final ErlangEditor editor,
            final IErlModule module, final IBackend backend,
            final IErlProject erlProject, final OpenResult res, final int offset)
            throws CoreException, RpcException, BackendException,
            ErlModelException, BadLocationException, OtpErlangRangeException {
        final IErlElement element = editor.getElementAt(offset, true);
        final IErlElementLocator.Scope scope = NavigationPreferencePage
                .getCheckAllProjects() ? IErlElementLocator.Scope.ALL_PROJECTS
                : IErlElementLocator.Scope.REFERENCED_PROJECTS;
        final IErlElementLocator model = ErlModelManager.getErlangModel();
        Object found = null;
        if (res.isExternalCall()) {
            found = findExternalCallOrType(module, res, erlProject, element,
                    scope);
        } else if (res.isInclude()) {
            found = ModelInternalUtils.findInclude(module, erlProject, res,
                    model);
        } else if (res.isLocalCall()) {
            found = findLocalCall(module, backend, erlProject, res, element,
                    scope);
        } else if (res.isVariable() && element instanceof ISourceReference) {
            final ISourceReference sref = (ISourceReference) element;
            final ISourceRange range = sref.getSourceRange();
            final String elementText = editor.getDocument().get(
                    range.getOffset(), range.getLength());
            found = ModelInternalUtils.findVariable(backend, range,
                    res.getName(), elementText);
        } else if (res.isRecord() || res.isMacro()) {
            final Kind kind = res.isMacro() ? Kind.MACRO_DEF : Kind.RECORD_DEF;
            found = ModelUtils.findPreprocessorDef(module, res.getName(), kind);
        } else if (res.isField()) {
            final IErlRecordDef def = (IErlRecordDef) ModelUtils
                    .findPreprocessorDef(module, res.getFun(), Kind.RECORD_DEF);
            if (def != null) {
                found = def.getFieldNamed(res.getName());
            }
        }
        return found;
    }

    public static boolean isTypeDefOrRecordDef(final IErlElement element,
            final OpenResult res) {
        if (element != null) {
            if (element.getKind() == IErlElement.Kind.RECORD_DEF) {
                return true;
            }
            if (element.getKind() == IErlElement.Kind.TYPESPEC) {
                if (!res.getFun().equals(element.getName())) {
                    return true;
                }
            }
        }
        return false;
    }

    private static IErlElement findLocalCall(final IErlModule module,
            final IBackend backend, final IErlProject erlProject,
            final OpenResult res, final IErlElement element,
            final IErlElementLocator.Scope scope) throws RpcException,
            CoreException {
        if (isTypeDefOrRecordDef(element, res)) {
            return ModelUtils.findTypespec(module, res.getFun());
        }
        final IErlFunction foundElement = module
                .findFunction(res.getFunction());
        if (foundElement != null) {
            return foundElement;
        }
        // imported functions
        OtpErlangObject res2 = null;
        String moduleName = null;
        final IErlImport ei = module.findImport(res.getFunction());
        if (ei != null) {
            final IErlModel model = ErlModelManager.getErlangModel();
            moduleName = ei.getImportModule();
            res2 = ErlideOpen.getSourceFromModule(backend, model.getPathVars(),
                    moduleName, erlProject.getExternalModulesString());
        }
        if (res2 instanceof OtpErlangString && moduleName != null) {
            // imported from otp module
            final OtpErlangString otpErlangString = (OtpErlangString) res2;
            final String modulePath = otpErlangString.stringValue();
            return ModelUtils.findFunction(moduleName, res.getFunction(),
                    modulePath, erlProject, scope, module);
        } else {
            // functions defined in include files
            final Collection<IErlModule> allIncludedFiles = module
                    .findAllIncludedFiles();
            for (final IErlModule includedModule : allIncludedFiles) {
                final IErlFunction function = includedModule.findFunction(res
                        .getFunction());
                if (function != null) {
                    return function;
                }
            }
            return null;
        }
    }

    private static IErlElement findExternalCallOrType(final IErlModule module,
            final OpenResult res, final IErlProject project,
            final IErlElement element, final IErlElementLocator.Scope scope)
            throws CoreException {
        if (isTypeDefOrRecordDef(element, res)) {
            return ModelUtils.findTypeDef(module, res.getName(), res.getFun(),
                    res.getPath(), project, scope);
        }
        final IErlFunction result = ModelUtils.findFunction(res.getName(),
                res.getFunction(), res.getPath(), project, scope, module);
        if (result != null) {
            return result;
        }
        return ModelUtils.findFunction(res.getName(),
                new ErlangFunction(res.getFun(), ErlangFunction.ANY_ARITY),
                res.getPath(), project, scope, module);
    }

}
