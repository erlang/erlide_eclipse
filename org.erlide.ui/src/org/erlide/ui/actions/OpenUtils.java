package org.erlide.ui.actions;

import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlRecordDef;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.engine.services.search.OpenService;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.ui.prefs.plugin.NavigationPreferencePage;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;

public class OpenUtils {

    private final ModelFindService modelFindService;

    public OpenUtils() {
        modelFindService = ErlangEngine.getInstance().getModelFindService();
    }

    public void openOpenResult(final ITextEditor editor, final IErlModule module,
            final int offset, final IErlProject erlProject, final OpenResult openResult,
            final IErlElement element) throws CoreException, ErlModelException,
            PartInitException, BadLocationException, OtpErlangRangeException,
            RpcException {
        if (editor == null) {
            return;
        }
        final Object found = findOpenResult(editor, module, erlProject, openResult,
                element);
        if (found instanceof IErlElement) {
            ErlModelUtils.openElement((IErlElement) found);
        } else if (found instanceof ISourceRange) {
            ErlModelUtils.openSourceRange(module, (ISourceRange) found);
        }
    }

    public Object findOpenResult(final ITextEditor editor, final IErlModule module,
            final IErlProject project, final OpenResult openResult,
            final IErlElement element) throws CoreException, ErlModelException,
            OtpErlangRangeException, RpcException, BadLocationException {
        final IErlElementLocator.Scope scope = NavigationPreferencePage
                .getCheckAllProjects() ? IErlElementLocator.Scope.ALL_PROJECTS
                : IErlElementLocator.Scope.REFERENCED_PROJECTS;
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        Object found = null;
        if (openResult.isExternalCall()) {
            found = findExternalCallOrType(module, openResult, project, element, scope);
        } else if (openResult.isInclude()) {
            found = modelFindService.findInclude(model, project, module,
                    openResult.getName(), openResult.getPath());
        } else if (openResult.isLocalCall()) {
            found = findLocalCall(module, project, openResult, element, scope);
        } else if (openResult.isVariable() && element instanceof ISourceReference) {
            final ISourceReference sref = (ISourceReference) element;
            final ISourceRange range = sref.getSourceRange();
            final String elementText = editor.getDocumentProvider()
                    .getDocument(editor.getEditorInput())
                    .get(range.getOffset(), range.getLength());
            found = modelFindService.findVariable(range, openResult.getName(),
                    elementText);
        } else if (openResult.isRecord() || openResult.isMacro()) {
            final ErlElementKind kind = openResult.isMacro() ? ErlElementKind.MACRO_DEF
                    : ErlElementKind.RECORD_DEF;
            found = modelFindService.findPreprocessorDef(module, openResult.getName(),
                    kind);
        } else if (openResult.isField()) {
            final IErlRecordDef def = (IErlRecordDef) modelFindService
                    .findPreprocessorDef(module, openResult.getFun(),
                            ErlElementKind.RECORD_DEF);
            if (def != null) {
                found = def.getFieldNamed(openResult.getName());
            }
        }
        return found;
    }

    private IErlElement findLocalCall(final IErlModule module,
            final IErlProject erlProject, final OpenResult res,
            final IErlElement element, final IErlElementLocator.Scope scope)
            throws RpcException, CoreException {
        if (isTypeDefOrRecordDef(element, res)) {
            return modelFindService.findTypespec(module, res.getFun());
        }
        final IErlFunction foundElement = module.findFunction(res.getFunction());
        if (foundElement != null) {
            return foundElement;
        }
        // imported functions
        OtpErlangObject res2 = null;
        String moduleName = null;
        final IErlImport ei = module.findImport(res.getFunction());
        if (ei != null) {
            final IErlModel model = ErlangEngine.getInstance().getModel();
            moduleName = ei.getImportModule();
            res2 = ErlangEngine
                    .getInstance()
                    .getService(OpenService.class)
                    .getSourceFromModule(model.getPathVars(), moduleName,
                            erlProject.getProperties().getExternalModules());
        }
        if (res2 instanceof OtpErlangString && moduleName != null) {
            // imported from otp module
            final OtpErlangString otpErlangString = (OtpErlangString) res2;
            final String modulePath = otpErlangString.stringValue();
            final IErlElementLocator model = ErlangEngine.getInstance().getModel();
            return modelFindService.findFunction(model, erlProject, module, moduleName,
                    modulePath, res.getFunction(), scope);
        }
        // functions defined in include files
        final Collection<IErlModule> allIncludedFiles = ErlangEngine.getInstance()
                .getModelSearcherService().findAllIncludedFiles(module);
        for (final IErlModule includedModule : allIncludedFiles) {
            final IErlFunction function = includedModule.findFunction(res.getFunction());
            if (function != null) {
                return function;
            }
        }
        return null;
    }

    public boolean isTypeDefOrRecordDef(final IErlElement element, final OpenResult res) {
        if (element != null) {
            if (element.getKind() == ErlElementKind.RECORD_DEF) {
                return true;
            }
            if (element.getKind() == ErlElementKind.TYPESPEC) {
                if (!res.getFun().equals(element.getName())) {
                    return true;
                }
            }
        }
        return false;
    }

    private IErlElement findExternalCallOrType(final IErlModule module,
            final OpenResult res, final IErlProject project, final IErlElement element,
            final IErlElementLocator.Scope scope) throws CoreException {
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        if (isTypeDefOrRecordDef(element, res)) {
            return modelFindService.findTypeDef(model, project, module, res.getName(),
                    res.getFun(), res.getPath(), scope);
        }
        final IErlFunction result = modelFindService.findFunction(model, project, module,
                res.getName(), res.getPath(), res.getFunction(), scope);
        if (result != null) {
            return result;
        }
        return modelFindService.findFunction(model, project, module, res.getName(),
                res.getPath(),
                new ErlangFunction(res.getFun(), ErlangFunction.ANY_ARITY), scope);
    }

}
