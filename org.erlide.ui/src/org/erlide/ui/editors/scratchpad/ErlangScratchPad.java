package org.erlide.ui.editors.scratchpad;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.ITextViewerExtension;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.ISaveablePart2;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.ui.actions.CompositeActionGroup;
import org.erlide.ui.actions.ErlangSearchActionGroup;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.ErlangSourceViewerConfiguration;
import org.erlide.ui.editors.erl.folding.IErlangFoldingStructureProvider;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

public class ErlangScratchPad extends AbstractErlangEditor implements ISaveablePart2 {

    private ColorManager colorManager;
    private IErlangFoldingStructureProvider fProjectionModelUpdater;
    private CompositeActionGroup fActionGroups;
    private CompositeActionGroup fContextMenuGroup;

    /**
     * Simple constructor
     *
     */
    public ErlangScratchPad() {
        super();
        registerListeners();
    }

    private void registerListeners() {
    }

    /**
     * Simple disposer
     *
     * @see org.eclipse.ui.IWorkbenchPart#dispose()
     */
    @Override
    public void dispose() {
        if (colorManager != null) {
            colorManager.dispose();
            colorManager = null;
        }

        final ISourceViewer sourceViewer = getSourceViewer();
        if (sourceViewer instanceof ITextViewerExtension) {
            ((ITextViewerExtension) sourceViewer)
                    .removeVerifyKeyListener(getBracketInserter());
        }
        if (fActionGroups != null) {
            fActionGroups.dispose();
            fActionGroups = null;
        }
        if (fProjectionModelUpdater != null) {
            fProjectionModelUpdater.uninstall();
        }

        super.dispose();
    }

    @Override
    protected void initializeEditor() {
        colorManager = new ColorManager();
        setDocumentProvider(new TextFileDocumentProvider());

        final IPreferenceStore store = getErlangEditorPreferenceStore();
        setPreferenceStore(store);

        final ErlangSourceViewerConfiguration cfg = new ErlangScratchPadConfiguration(
                getPreferenceStore(), colorManager, this);
        setSourceViewerConfiguration(cfg);
    }

    @Override
    public IErlProject getProject() {
        final IFile file = getFile();
        if (file != null) {
            final IProject project = file.getProject();
            if (project != null) {
                return ErlangEngine.getInstance().getModel().findProject(project);
            }
        }
        return null;
    }

    public static ChainedPreferenceStore getErlangEditorPreferenceStore() {
        final IPreferenceStore generalTextStore = EditorsUI.getPreferenceStore();
        return new ChainedPreferenceStore(new IPreferenceStore[] {
                ErlideUIPlugin.getDefault().getPreferenceStore(), generalTextStore });
    }

    @Override
    public void createPartControl(final Composite parent) {
        super.createPartControl(parent);

        setupBracketInserter();
    }

    @Override
    protected void createActions() {
        super.createActions();
        ActionGroup esg;
        fActionGroups = new CompositeActionGroup(
                new ActionGroup[] { esg = new ErlangSearchActionGroup(this) });
        fContextMenuGroup = new CompositeActionGroup(new ActionGroup[] { esg });

        createCommonActions();

        // if (ErlideUtil.isTest()) {
        // testAction = new TestAction(ErlangEditorMessages
        // .getBundleForConstructedKeys(), "Test.", this, getModule());
        // testAction
        // .setActionDefinitionId(IErlangEditorActionDefinitionIds.TEST);
        // setAction("Test", testAction);
        // markAsStateDependentAction("Test", true);
        // markAsSelectionDependentAction("Test", true);
        // // PlatformUI.getWorkbench().getHelpSystem().setHelp(indentAction,
        // // IErlangHelpContextIds.INDENT_ACTION);
        // }

    }

    @Override
    protected void editorContextMenuAboutToShow(final IMenuManager menu) {
        super.editorContextMenuAboutToShow(menu);

        // if (ErlideUtil.isTest()) {
        // menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, testAction);
        // }
        addCommonActions(menu);
        final ActionContext context = new ActionContext(getSelectionProvider()
                .getSelection());
        fContextMenuGroup.setContext(context);
        fContextMenuGroup.fillContextMenu(menu);
        fContextMenuGroup.setContext(null);
    }

    // Auto-saving when quitting or closing, through ISaveablePart2

    @Override
    public int promptToSaveOnClose() {
        doSave(getProgressMonitor());
        return NO;
    }

    @Override
    protected void initializeKeyBindingScopes() {
        setKeyBindingScopes(new String[] { "org.erlide.ui.erlangEditorScope" }); //$NON-NLS-1$
    }

    @Override
    public void reconcileNow() {
    }

    @Override
    public IErlElement getElementAt(final int offset, final boolean b) {
        return null;
    }

    @Override
    public IErlModule getModule() {
        return null;
    }

    private IFile getFile() {
        final IEditorInput editorInput = getEditorInput();
        if (editorInput instanceof IFileEditorInput) {
            final IFileEditorInput input = (IFileEditorInput) editorInput;
            return input.getFile();
        }
        return null;
    }

    @Override
    protected ScannerService getNewScanner() {
        final IFile file = getFile();
        if (file == null) {
            return null;
        }
        try {
            final String filePath = file.getLocation().toPortableString();
            String initialText;
            initialText = Util.getInputStreamAsString(file.getContents(),
                    file.getCharset());
            final ScannerService scanner = ErlangEngine.getInstance()
                    .getScannerProviderService().get(getScannerName());
            scanner.initialScan(initialText, filePath, false);
            return scanner;
        } catch (final CoreException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    @Override
    public String getScannerName() {
        final IFile file = getFile();
        if (file != null) {
            final IPath fullPath = file.getFullPath();
            final String scannerName = "scratchPad"
                    + fullPath.toPortableString().hashCode() + "_"
                    + fullPath.removeFileExtension().lastSegment();
            return scannerName;
        }
        return null;
    }

    @Override
    protected void addFoldingSupport(final ISourceViewer viewer) {
    }

}
