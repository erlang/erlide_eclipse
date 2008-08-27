/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import java.util.Iterator;
import java.util.List;
import java.util.ResourceBundle;
import java.util.Stack;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPositionCategoryException;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IPositionUpdater;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITextViewerExtension;
import org.eclipse.jface.text.ITextViewerExtension2;
import org.eclipse.jface.text.ITextViewerExtension4;
import org.eclipse.jface.text.ITextViewerExtension5;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.TextUtilities;
import org.eclipse.jface.text.information.IInformationProvider;
import org.eclipse.jface.text.information.IInformationProviderExtension;
import org.eclipse.jface.text.information.IInformationProviderExtension2;
import org.eclipse.jface.text.information.InformationPresenter;
import org.eclipse.jface.text.link.ILinkedModeListener;
import org.eclipse.jface.text.link.LinkedModeModel;
import org.eclipse.jface.text.link.LinkedModeUI;
import org.eclipse.jface.text.link.LinkedPosition;
import org.eclipse.jface.text.link.LinkedPositionGroup;
import org.eclipse.jface.text.link.LinkedModeUI.ExitFlags;
import org.eclipse.jface.text.link.LinkedModeUI.IExitPolicy;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.search.ui.IContextMenuConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.AnnotationPreference;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.eclipse.ui.texteditor.ContentAssistAction;
import org.eclipse.ui.texteditor.IEditorStatusLine;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.ResourceAction;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.eclipse.ui.texteditor.TextOperationAction;
import org.eclipse.ui.texteditor.link.EditorLinkedModeUI;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.eclipse.ui.views.properties.IPropertySource;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.actions.CompositeActionGroup;
import org.erlide.ui.actions.ErlangSearchActionGroup;
import org.erlide.ui.actions.IndentAction;
import org.erlide.ui.actions.OpenAction;
import org.erlide.ui.actions.ShowOutlineAction;
import org.erlide.ui.actions.ToggleCommentAction;
import org.erlide.ui.editors.erl.test.TestAction;
import org.erlide.ui.editors.folding.IErlangFoldingStructureProvider;
import org.erlide.ui.editors.outline.IOutlineContentCreator;
import org.erlide.ui.editors.outline.IOutlineSelectionHandler;
import org.erlide.ui.editors.util.HTMLTextPresenter;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.prefs.plugin.ErlidePreferencePage;
import org.erlide.ui.prefs.plugin.SmartTypingPreferencePage;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.ui.views.ErlangPropertySource;
import org.erlide.ui.views.outline.ErlangContentProvider;
import org.erlide.ui.views.outline.ErlangLabelProvider;
import org.erlide.ui.views.outline.ErlangOutlinePage;

import erlang.ErlideScanner2;

/**
 * The actual editor itself
 * 
 * 
 * @author Eric Merrit [cyberlync at gmail dot com]
 */
public class ErlangEditor extends TextEditor implements IOutlineContentCreator,
		IOutlineSelectionHandler {

	private ColorManager colorManager;

	private ErlangOutlinePage myOutlinePage;

	private IPropertySource myPropertySource;

	private ProjectionSupport fProjectionSupport;

	private IErlangFoldingStructureProvider fProjectionModelUpdater;

	private OpenAction openAction;

	private IndentAction indentAction;

	private ToggleCommentAction toggleCommentAction;

	private TestAction testAction;

	/** The selection changed listeners */
	private EditorSelectionChangedListener fEditorSelectionChangedListener;

	protected AbstractSelectionChangedListener fOutlineSelectionChangedListener = new OutlineSelectionChangedListener();

	InformationPresenter fInformationPresenter;

	private ShowOutlineAction fShowOutline;

	private Object fSelection;

	/** Preference key for matching brackets */
	protected final static String MATCHING_BRACKETS = PreferenceConstants.EDITOR_MATCHING_BRACKETS;

	/** Preference key for matching brackets color */
	protected final static String MATCHING_BRACKETS_COLOR = PreferenceConstants.EDITOR_MATCHING_BRACKETS_COLOR;

	/** The bracket inserter. */
	private final BracketInserter fBracketInserter = new BracketInserter();

	private final IPreferenceChangeListener fPreferenceChangeListener = new PreferenceChangeListener();

	private ActionGroup fActionGroups;

	private ActionGroup fContextMenuGroup;

	/**
	 * Simple constructor
	 * 
	 */
	public ErlangEditor() {
		super();
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

		disposeScanner();

		final ISourceViewer sourceViewer = getSourceViewer();
		if (sourceViewer instanceof ITextViewerExtension) {
			((ITextViewerExtension) sourceViewer)
					.removeVerifyKeyListener(fBracketInserter);
		}
		final IEclipsePreferences node = ErlidePreferencePage.getPrefsNode();
		node.removePreferenceChangeListener(fPreferenceChangeListener);
		if (fActionGroups != null) {
			fActionGroups.dispose();
			fActionGroups = null;
		}
		super.dispose();
	}

	@Override
	protected void initializeEditor() {
		colorManager = new ColorManager();
		setDocumentProvider(new TextFileDocumentProvider());

		// Platform.getAdapterManager().registerAdapters(adapterFactory,
		// IResource.class);

		final IPreferenceStore generalTextStore = EditorsUI
				.getPreferenceStore();
		final IPreferenceStore store = new ChainedPreferenceStore(
				new IPreferenceStore[] {
						ErlideUIPlugin.getDefault().getPreferenceStore(),
						generalTextStore });
		setPreferenceStore(store);

		final EditorConfiguration cfg = new EditorConfiguration(
				getPreferenceStore(), this, colorManager);
		setSourceViewerConfiguration(cfg);
		ErlModelUtils.reenableScanner(this);
	}

	public IErlScanner getScanner() {
		return ErlModelUtils.getScanner(this);
	}

	public void disposeScanner() {
		ErlModelUtils.disposeScanner(this);
		ErlModelUtils.disposeParser(this);
	}

	public ICharacterPairMatcher getBracketMatcher() {
		return ((EditorConfiguration) getSourceViewerConfiguration())
				.getBracketMatcher();
	}

	@Override
	protected void initializeKeyBindingScopes() {
		setKeyBindingScopes(new String[] { "org.erlide.ui.erlangEditorScope" }); //$NON-NLS-1$
	}

	/**
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#handlePreferenceStoreChanged(org.eclipse.jface.util.PropertyChangeEvent)
	 */
	@Override
	protected void handlePreferenceStoreChanged(final PropertyChangeEvent event) {
		super.handlePreferenceStoreChanged(event);
		final String name = event.getProperty();
		if (name.startsWith(PreferenceConstants.EDITOR_PREFIX)) {
			ErlLogger.debug("prefs changed");
			final RGB rgb = getRGB(event.getNewValue());
			// TODO handle text style changes too!
			if (null != rgb) {
				final ErlHighlightScanner scanner = ((EditorConfiguration) getSourceViewerConfiguration())
						.getHighlightScanner();

				scanner.handleColorChange(event.getProperty(), rgb);
				getSourceViewer().invalidateTextPresentation();
			}
		}
	}

	/**
	 * Try to get an rgb value
	 * 
	 * @param value
	 *            the object
	 * @return
	 */
	private static RGB getRGB(final Object value) {
		if (value instanceof RGB) {
			return (RGB) value;
		} else if (value instanceof String) {
			return StringConverter.asRGB((String) value);
		}
		return null;
	}

	class PreferenceChangeListener implements IPreferenceChangeListener {
		public void preferenceChange(final PreferenceChangeEvent event) {
			final String key = event.getKey();
			if (key.indexOf('/') != -1
					&& key.split("/")[0]
							.equals(SmartTypingPreferencePage.SMART_TYPING_KEY)) {
				getSmartTypingPrefs();
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	protected final boolean isActiveEditor() {
		final IWorkbenchWindow window = getSite().getWorkbenchWindow();
		final IWorkbenchPage page = window.getActivePage();
		if (page == null) {
			return false;
		}
		final IEditorPart activeEditor = page.getActiveEditor();
		return activeEditor != null && activeEditor.equals(this);
	}

	@Override
	protected void createActions() {
		super.createActions();
		String externalModules;
		final IEditorInput input = getEditorInput();
		if (input instanceof IFileEditorInput) {
			final IFileEditorInput fileInput = (IFileEditorInput) input;
			final ErlangProjectProperties prefs = new ErlangProjectProperties(
					fileInput.getFile().getProject());
			externalModules = prefs.getExternalModules();
		} else {
			externalModules = "";
		}

		// ActionGroup oeg, ovg, jsg;
		ActionGroup esg;
		fActionGroups = new CompositeActionGroup(new ActionGroup[] {
		// oeg= new OpenEditorActionGroup(this),
				// ovg= new OpenViewActionGroup(this),
				esg = new ErlangSearchActionGroup(this) });
		fContextMenuGroup = new CompositeActionGroup(new ActionGroup[] { esg });

		openAction = new OpenAction(getSite(), externalModules);
		openAction
				.setActionDefinitionId(IErlangEditorActionDefinitionIds.OPEN_EDITOR);
		setAction(IErlangEditorActionDefinitionIds.OPEN, openAction);

		final Action act = new ContentAssistAction(ErlangEditorMessages
				.getBundleForConstructedKeys(), "ContentAssistProposal.", this);
		act
				.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
		setAction("ContentAssistProposal", act);
		markAsStateDependentAction("ContentAssistProposal", true);

		ResourceAction resAction = new TextOperationAction(ErlangEditorMessages
				.getBundleForConstructedKeys(),
				"ShowEDoc.", this, ISourceViewer.INFORMATION, true); //$NON-NLS-1$
		resAction = new InformationDispatchAction(ErlangEditorMessages
				.getBundleForConstructedKeys(),
				"ShowEDoc.", (TextOperationAction) resAction); //$NON-NLS-1$
		resAction
				.setActionDefinitionId(IErlangEditorActionDefinitionIds.SHOW_EDOC);
		setAction("ShowEDoc", resAction); //$NON-NLS-1$
		PlatformUI.getWorkbench().getHelpSystem().setHelp(resAction,
				IErlangHelpContextIds.SHOW_EDOC_ACTION);

		indentAction = new IndentAction(ErlangEditorMessages
				.getBundleForConstructedKeys(), "Indent.", this); //$NON-NLS-1$
		indentAction
				.setActionDefinitionId(IErlangEditorActionDefinitionIds.INDENT);
		setAction("Indent", indentAction); //$NON-NLS-1$
		markAsStateDependentAction("Indent", true); //$NON-NLS-1$
		markAsSelectionDependentAction("Indent", true); //$NON-NLS-1$
		PlatformUI.getWorkbench().getHelpSystem().setHelp(indentAction,
				IErlangHelpContextIds.INDENT_ACTION);
		if (BackendManager.isTest()) {
			testAction = new TestAction(ErlangEditorMessages
					.getBundleForConstructedKeys(), "Test.", this, getModule());
			testAction
					.setActionDefinitionId(IErlangEditorActionDefinitionIds.TEST);
			setAction("Test", testAction);
			markAsStateDependentAction("Test", true);
			markAsSelectionDependentAction("Test", true);
			// PlatformUI.getWorkbench().getHelpSystem().setHelp(indentAction,
			// IErlangHelpContextIds.INDENT_ACTION);
		}

		final Action action = new IndentAction(ErlangEditorMessages
				.getBundleForConstructedKeys(), "Indent.", this);
		setAction("IndentOnTab", action);
		markAsStateDependentAction("IndentOnTab", true);
		markAsSelectionDependentAction("IndentOnTab", true);

		toggleCommentAction = new ToggleCommentAction(ErlangEditorMessages
				.getBundleForConstructedKeys(), "ToggleComment.", this);
		toggleCommentAction
				.setActionDefinitionId(IErlangEditorActionDefinitionIds.TOGGLE_COMMENT);
		setAction("ToggleComment", toggleCommentAction);
		markAsStateDependentAction("ToggleComment", true);
		markAsSelectionDependentAction("ToggleComment", true);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(toggleCommentAction,
				IErlangHelpContextIds.TOGGLE_COMMENT_ACTION);

		fShowOutline = new ShowOutlineAction(ErlangEditorMessages
				.getBundleForConstructedKeys(), "ShowOutline.", this);
		fShowOutline
				.setActionDefinitionId(IErlangEditorActionDefinitionIds.SHOW_OUTLINE);
		setAction(IErlangEditorActionDefinitionIds.SHOW_OUTLINE, fShowOutline);
		markAsContentDependentAction(
				IErlangEditorActionDefinitionIds.SHOW_OUTLINE, true);

	}

	@Override
	protected void editorContextMenuAboutToShow(final IMenuManager menu) {
		super.editorContextMenuAboutToShow(menu);

		if (BackendManager.isTest()) {
			menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, testAction);
		}
		menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, fShowOutline);
		menu.prependToGroup(IContextMenuConstants.GROUP_OPEN,
				toggleCommentAction);
		menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, indentAction);
		menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, openAction);
		final ActionContext context = new ActionContext(getSelectionProvider()
				.getSelection());
		fContextMenuGroup.setContext(context);
		fContextMenuGroup.fillContextMenu(menu);
		fContextMenuGroup.setContext(null);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object getAdapter(final Class required) {
		if (IContentOutlinePage.class.equals(required)) {
			if (myOutlinePage == null) {
				myOutlinePage = createOutlinePage();
			}
			return myOutlinePage;
		}
		if (IPropertySource.class.equals(required)) {
			if (myPropertySource == null) {
				ErlLogger.debug("make prop source...");
				myPropertySource = new ErlangPropertySource(this);
			}
			return myPropertySource;
		}

		if (required == IErlangFoldingStructureProvider.class) {
			return fProjectionModelUpdater;
		}

		if (fProjectionSupport != null) {
			final Object adapter = fProjectionSupport.getAdapter(
					getSourceViewer(), required);
			if (adapter != null) {
				return adapter;
			}
		}

		return super.getAdapter(required);
	}

	@Override
	protected ISourceViewer createSourceViewer(final Composite parent,
			final IVerticalRuler ruler, final int styles) {
		// return new ErlangSourceViewer(parent, ruler, styles);
		final ISourceViewer viewer = new ProjectionViewer(parent, ruler,
				getOverviewRuler(), true, styles);
		getSourceViewerDecorationSupport(viewer);

		/*
		 * This is a performance optimization to reduce the computation of the
		 * text presentation triggered by {@link #setVisibleDocument(IDocument)}
		 */
		// if (javaSourceViewer != null && isFoldingEnabled() && (store == null
		// ||
		// !store.getBoolean(PreferenceConstants.EDITOR_SHOW_SEGMENTS)))
		// javaSourceViewer.prepareDelayedProjection();
		final ProjectionViewer projectionViewer = (ProjectionViewer) viewer;
		fProjectionSupport = new ProjectionSupport(projectionViewer,
				getAnnotationAccess(), getSharedColors());
		fProjectionSupport
				.addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.error"); //$NON-NLS-1$
		fProjectionSupport
				.addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.warning"); //$NON-NLS-1$
		// TODO fProjectionSupport.setHoverControlCreator(new
		// IInformationControlCreator()
		// {
		// public IInformationControl createInformationControl(Shell shell) {
		// return new CustomSourceInformationControl(shell,
		// IDocument.DEFAULT_CONTENT_TYPE);
		// }
		// });

		fProjectionSupport.install();

		fProjectionModelUpdater = ErlideUIPlugin.getDefault()
				.getFoldingStructureProviderRegistry()
				.getCurrentFoldingProvider();
		if (fProjectionModelUpdater != null) {
			fProjectionModelUpdater.install(this, projectionViewer);
		}

		return viewer;
	}

	@Override
	protected void configureSourceViewerDecorationSupport(
			final SourceViewerDecorationSupport support) {
		support.setCharacterPairMatcher(getBracketMatcher());
		support.setMatchingCharacterPainterPreferenceKeys(MATCHING_BRACKETS,
				MATCHING_BRACKETS_COLOR);

		super.configureSourceViewerDecorationSupport(support);
	}

	/**
	 * Jumps to the matching bracket.
	 */
	public void gotoMatchingBracket() {

		final ISourceViewer sourceViewer = getSourceViewer();
		final IDocument document = sourceViewer.getDocument();
		if (document == null) {
			return;
		}

		final IRegion selection = getSignedSelection(sourceViewer);

		final int selectionLength = Math.abs(selection.getLength());
		if (selectionLength > 1) {
			setStatusLineErrorMessage(ErlangEditorMessages.GotoMatchingBracket_error_invalidSelection);
			sourceViewer.getTextWidget().getDisplay().beep();
			return;
		}

		// #26314
		final int sourceCaretOffset = selection.getOffset()
				+ selection.getLength();
		// TODO fix me!
		// if (isSurroundedByBrackets(document, sourceCaretOffset))
		// sourceCaretOffset -= selection.getLength();

		final IRegion region = getBracketMatcher().match(document,
				sourceCaretOffset);
		if (region == null) {
			setStatusLineErrorMessage(ErlangEditorMessages.GotoMatchingBracket_error_noMatchingBracket);
			sourceViewer.getTextWidget().getDisplay().beep();
			return;
		}

		final int offset = region.getOffset();
		final int length = region.getLength();

		if (length < 1) {
			return;
		}

		final int anchor = getBracketMatcher().getAnchor();
		// http://dev.eclipse.org/bugs/show_bug.cgi?id=34195
		int targetOffset = ICharacterPairMatcher.RIGHT == anchor ? offset + 1
				: offset + length;

		boolean visible = false;
		if (sourceViewer instanceof ITextViewerExtension5) {
			final ITextViewerExtension5 extension = (ITextViewerExtension5) sourceViewer;
			visible = extension.modelOffset2WidgetOffset(targetOffset) > -1;
		} else {
			final IRegion visibleRegion = sourceViewer.getVisibleRegion();
			// http://dev.eclipse.org/bugs/show_bug.cgi?id=34195
			visible = targetOffset >= visibleRegion.getOffset()
					&& targetOffset <= visibleRegion.getOffset()
							+ visibleRegion.getLength();
		}

		if (!visible) {
			setStatusLineErrorMessage(ErlangEditorMessages.GotoMatchingBracket_error_bracketOutsideSelectedElement);
			sourceViewer.getTextWidget().getDisplay().beep();
			return;
		}

		if (selection.getLength() < 0) {
			targetOffset -= selection.getLength();
		}

		sourceViewer.setSelectedRange(targetOffset, selection.getLength());
		sourceViewer.revealRange(targetOffset, selection.getLength());
	}

	/**
	 * Returns the signed current selection. The length will be negative if the
	 * resulting selection is right-to-left (RtoL).
	 * <p>
	 * The selection offset is model based.
	 * </p>
	 * 
	 * @param sourceViewer
	 *            the source viewer
	 * @return a region denoting the current signed selection, for a resulting
	 *         RtoL selections length is < 0
	 */
	protected IRegion getSignedSelection(final ISourceViewer sourceViewer) {
		final StyledText text = sourceViewer.getTextWidget();
		final Point selection = text.getSelectionRange();

		if (text.getCaretOffset() == selection.x) {
			selection.x = selection.x + selection.y;
			selection.y = -selection.y;
		}

		selection.x = widgetOffset2ModelOffset(sourceViewer, selection.x);

		return new Region(selection.x, selection.y);
	}

	/**
	 * Sets the given message as error message to this editor's status line.
	 * 
	 * @param msg
	 *            message to be set
	 */
	@Override
	protected void setStatusLineErrorMessage(final String msg) {
		final IEditorStatusLine statusLine = (IEditorStatusLine) getAdapter(IEditorStatusLine.class);
		if (statusLine != null) {
			statusLine.setMessage(true, msg, null);
		}
	}

	/**
	 * Sets the given message as message to this editor's status line.
	 * 
	 * @param msg
	 *            message to be set
	 * @since 3.0
	 */
	@Override
	protected void setStatusLineMessage(final String msg) {
		final IEditorStatusLine statusLine = (IEditorStatusLine) getAdapter(IEditorStatusLine.class);
		if (statusLine != null) {
			statusLine.setMessage(false, msg, null);
		}
	}

	@Override
	protected void doSetInput(final IEditorInput input) throws CoreException {
		disposeScanner();

		super.doSetInput(input);

		if (myOutlinePage != null) {
			// TODO should we use model events here?
			myOutlinePage.setInput(input);
		}
	}

	@Override
	protected void doSetSelection(final ISelection selection) {
		super.doSetSelection(selection);
		synchronizeOutline();
	}

	private void synchronizeOutline() {
		synchronizeOutlinePage(computeHighlightRangeSourceReference());
	}

	protected ISourceReference computeHighlightRangeSourceReference() {
		final ISourceViewer sourceViewer = getSourceViewer();
		if (sourceViewer == null) {
			return null;
		}

		final StyledText styledText = sourceViewer.getTextWidget();
		if (styledText == null) {
			return null;
		}

		int caret = 0;
		if (sourceViewer instanceof ITextViewerExtension5) {
			final ITextViewerExtension5 extension = (ITextViewerExtension5) sourceViewer;
			caret = extension.widgetOffset2ModelOffset(styledText
					.getCaretOffset());
		} else {
			final int offset = sourceViewer.getVisibleRegion().getOffset();
			caret = offset + styledText.getCaretOffset();
		}

		final IErlElement element = getElementAt(caret, false);

		if (!(element instanceof ISourceReference)) {
			return null;
		}

		return (ISourceReference) element;
	}

	public IErlModule getModule() {
		return ErlModelUtils.getModule(getEditorInput());
	}

	/**
	 * Returns the most narrow element including the given offset. If
	 * <code>reconcile</code> is <code>true</code> the editor's input
	 * element is reconciled in advance. If it is <code>false</code> this
	 * method only returns a result if the editor's input element does not need
	 * to be reconciled.
	 * 
	 * @param offset
	 *            the offset included by the retrieved element
	 * @param reconcile
	 *            <code>true</code> if working copy should be reconciled
	 * @return the most narrow element which includes the given offset
	 * @throws ErlModelException
	 */
	public IErlElement getElementAt(final int offset, final boolean reconcile) {
		final IErlModule module = getModule();
		if (module != null) {
			try {
				if (reconcile) {
					synchronized (module) {
						module.open(null);
						return module.getElementAt(offset);
					}
				} else if (module.isConsistent()) {
					return module.getElementAt(offset);
				}
			} catch (final Exception e) {
			}
		}
		return null;
	}

	// private void fullReconcileModule(IErlModule module, IDocument document) {
	// module.insertText(0, document.get());
	// }

	protected abstract class AbstractSelectionChangedListener implements
			ISelectionChangedListener {

		/**
		 * Installs this selection changed listener with the given selection
		 * provider. If the selection provider is a post selection provider,
		 * post selection changed events are the preferred choice, otherwise
		 * normal selection changed events are requested.
		 * 
		 * @param selectionProvider
		 */
		public void install(final ISelectionProvider selectionProvider) {
			if (selectionProvider == null) {
				return;
			}

			if (selectionProvider instanceof IPostSelectionProvider) {
				final IPostSelectionProvider provider = (IPostSelectionProvider) selectionProvider;
				provider.addPostSelectionChangedListener(this);
			} else {
				selectionProvider.addSelectionChangedListener(this);
			}
		}

		/**
		 * Removes this selection changed listener from the given selection
		 * provider.
		 * 
		 * @param selectionProvider
		 *            the selection provider
		 */
		public void uninstall(final ISelectionProvider selectionProvider) {
			if (selectionProvider == null) {
				return;
			}

			if (selectionProvider instanceof IPostSelectionProvider) {
				final IPostSelectionProvider provider = (IPostSelectionProvider) selectionProvider;
				provider.removePostSelectionChangedListener(this);
			} else {
				selectionProvider.removeSelectionChangedListener(this);
			}

		}
	}

	/**
	 * Updates the Erlang outline page selection and this editor's range
	 * indicator.
	 */
	class EditorSelectionChangedListener extends
			AbstractSelectionChangedListener {

		/*
		 * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged
		 *      (org.eclipse.jface.viewers.SelectionChangedEvent)
		 */
		public void selectionChanged(final SelectionChangedEvent event) {
			ErlangEditor.this.selectionChanged();
		}
	}

	/**
	 * Updates the selection in the editor's widget with the selection of the
	 * outline page.
	 */
	class OutlineSelectionChangedListener extends
			AbstractSelectionChangedListener {

		public void selectionChanged(final SelectionChangedEvent event) {
			doSelectionChanged(event);
		}
	}

	protected void doSelectionChanged(final SelectionChangedEvent event) {
		ISourceReference reference = null;

		final ISelection selection = event.getSelection();
		final Iterator<?> iter = ((IStructuredSelection) selection).iterator();
		while (iter.hasNext()) {
			final Object o = iter.next();
			if (o instanceof ISourceReference) {
				reference = (ISourceReference) o;
				break;
			}
		}
		if (!isActivePart() && ErlideUIPlugin.getActivePage() != null) {
			ErlideUIPlugin.getActivePage().bringToTop(this);
		}

		setSelection(reference, true);
	}

	protected void selectionChanged() {
		if (getSelectionProvider() == null) {
			return;
		}
		final ISourceReference element = computeHighlightRangeSourceReference();
		synchronizeOutlinePage(element);
		setSelection(element, false);
		// updateStatusLine();
	}

	/**
	 * Creates the outline page used with this editor.
	 * 
	 * @return the created Java outline page
	 */
	protected ErlangOutlinePage createOutlinePage() {
		final ErlangOutlinePage page = new ErlangOutlinePage(
				getDocumentProvider(), this);
		fOutlineSelectionChangedListener.install(page);
		page.setInput(getEditorInput());
		return page;
	}

	/**
	 * Informs the editor that its outliner has been closed.
	 */
	public void outlinePageClosed() {
		if (myOutlinePage != null) {
			fOutlineSelectionChangedListener.uninstall(myOutlinePage);
			myOutlinePage = null;
			resetHighlightRange();
		}
	}

	/**
	 * Synchronizes the outliner selection with the given element position in
	 * the editor.
	 * 
	 * @param element
	 *            the java element to select
	 */
	protected void synchronizeOutlinePage(final ISourceReference element) {
		synchronizeOutlinePage(element, true);
	}

	/**
	 * Synchronizes the outliner selection with the given element position in
	 * the editor.
	 * 
	 * @param element
	 *            the java element to select
	 * @param checkIfOutlinePageActive
	 *            <code>true</code> if check for active outline page needs to
	 *            be done
	 */
	protected void synchronizeOutlinePage(final ISourceReference element,
			final boolean checkIfOutlinePageActive) {
		if (myOutlinePage != null // && element != null
		// && !(checkIfOutlinePageActive && isErlangOutlinePageActive())
		) {
			fOutlineSelectionChangedListener.uninstall(myOutlinePage);
			myOutlinePage.select(element);
			fOutlineSelectionChangedListener.install(myOutlinePage);
		}
	}

	/**
	 * Synchronizes the outliner selection with the actual cursor position in
	 * the editor.
	 */
	public void synchronizeOutlinePageSelection() {
		synchronizeOutlinePage(computeHighlightRangeSourceReference());
	}

	public void setSelection(final ISourceReference reference,
			boolean moveCursor) {
		if (getSelectionProvider() == null) {
			return;
		}

		final ISelection selection = getSelectionProvider().getSelection();
		if (selection instanceof TextSelection) {
			final TextSelection textSelection = (TextSelection) selection;
			if (moveCursor
					&& (textSelection.getOffset() != 0 || textSelection
							.getLength() != 0)) {
				markInNavigationHistory();
			}
		}

		if (reference != null) {

			StyledText textWidget = null;

			final ISourceViewer sourceViewer = getSourceViewer();
			if (sourceViewer == null) {
				return;
			}
			textWidget = sourceViewer.getTextWidget();

			if (textWidget == null) {
				return;
			}

			try {
				ISourceRange range = null;
				range = reference.getSourceRange();

				if (range == null) {
					return;
				}

				int offset = range.getOffset();
				int length = range.getLength();

				if (offset < 0 || length < 0) {
					return;
				}

				setHighlightRange(offset, length, moveCursor);

				if (!moveCursor) {
					return;
				}

				offset = -1;
				length = -1;

				/*
				 * if (reference instanceof IErlComment) { range =
				 * reference.getSourceRange(); if (range != null) { offset =
				 * range.getOffset(); length = range.getLength(); } } else
				 */
				if (reference instanceof IErlMember) {
					range = ((IErlMember) reference).getNameRange();
					if (range != null) {
						offset = range.getOffset();
						length = range.getLength();
					}
				} else if (reference instanceof IErlAttribute) {
					range = ((IErlAttribute) reference).getNameRange();
					if (range != null) {
						offset = range.getOffset();
						length = range.getLength();
					}

				} else if (reference instanceof IErlFunctionClause) {
					range = ((IErlFunctionClause) reference).getNameRange();
					if (range != null) {
						offset = range.getOffset();
						length = range.getLength();
					}
				}
				if (offset > -1 && length > 0) {

					try {
						textWidget.setRedraw(false);
						sourceViewer.revealRange(offset, length);
						sourceViewer.setSelectedRange(offset, length);
					} finally {
						textWidget.setRedraw(true);
					}

					markInNavigationHistory();
				}

			} catch (final ErlModelException x) {
			} catch (final IllegalArgumentException x) {
			}

		} else if (moveCursor) {
			resetHighlightRange();
			markInNavigationHistory();
		}
	}

	public void setSelection(final IErlElement element) {

		if (element == null || element instanceof IErlModule) {
			return;
		}

		if (element instanceof ISourceReference) {
			final ISourceReference reference = (ISourceReference) element;
			// set highlight range
			setSelection(reference, true);
			// set outliner selection
			// try {
			// ErlLogger.debug(".. sel" + reference.getSource()
			// + myOutlinePage);
			// } catch (final ErlModelException e) {
			// e.printStackTrace();
			// }
			if (myOutlinePage != null) {
				fOutlineSelectionChangedListener.uninstall(myOutlinePage);
				myOutlinePage.select(reference);
				fOutlineSelectionChangedListener.install(myOutlinePage);
			}
		}
	}

	/* NOT USED */
	/*
	 * private boolean isErlangOutlinePageActive() { final IWorkbenchPart part =
	 * getActivePart(); return part instanceof ContentOutline &&
	 * ((ContentOutline) part).getCurrentPage() == myOutlinePage; }
	 */

	private IWorkbenchPart getActivePart() {
		final IWorkbenchWindow window = getSite().getWorkbenchWindow();
		final IPartService service = window.getPartService();
		final IWorkbenchPart part = service.getActivePart();
		return part;
	}

	@Override
	public void createPartControl(final Composite parent) {
		super.createPartControl(parent);

		getSmartTypingPrefs();

		final ISourceViewer sourceViewer = getSourceViewer();
		if (sourceViewer instanceof ITextViewerExtension) {
			((ITextViewerExtension) sourceViewer)
					.prependVerifyKeyListener(fBracketInserter);
		}

		final ProjectionViewer v = (ProjectionViewer) getSourceViewer();
		v.doOperation(ProjectionViewer.TOGGLE);

		fEditorSelectionChangedListener = new EditorSelectionChangedListener();
		fEditorSelectionChangedListener.install(getSelectionProvider());

		final IInformationControlCreator informationControlCreator = new IInformationControlCreator() {

			public IInformationControl createInformationControl(Shell shell) {
				boolean cutDown = false;
				int style = cutDown ? SWT.NONE : SWT.V_SCROLL | SWT.H_SCROLL;
				return new DefaultInformationControl(shell, SWT.RESIZE
						| SWT.TOOL, style, new HTMLTextPresenter(cutDown));
			}
		};

		fInformationPresenter = new InformationPresenter(
				informationControlCreator);
		fInformationPresenter.setSizeConstraints(60, 10, true, true);
		fInformationPresenter.install(getSourceViewer());

		final IEclipsePreferences node = ErlidePreferencePage.getPrefsNode();
		node.addPreferenceChangeListener(fPreferenceChangeListener);
	}

	@SuppressWarnings("boxing")
	void getSmartTypingPrefs() {
		final List<Boolean> autoClosePrefs = SmartTypingPreferencePage
				.getPreferences();
		fBracketInserter.setCloseAtomsEnabled(autoClosePrefs
				.get(SmartTypingPreferencePage.ATOMS));
		fBracketInserter.setCloseBracketsEnabled(autoClosePrefs
				.get(SmartTypingPreferencePage.BRACKETS));
		fBracketInserter.setCloseStringsEnabled(autoClosePrefs
				.get(SmartTypingPreferencePage.STRINGS));
		fBracketInserter.setCloseBracesEnabled(autoClosePrefs
				.get(SmartTypingPreferencePage.BRACES));
		fBracketInserter.setCloseParensEnabled(autoClosePrefs
				.get(SmartTypingPreferencePage.PARENS));
	}

	protected boolean isActivePart() {
		final IWorkbenchPart part = getActivePart();
		return part != null && part.equals(this);
	}

	/**
	 * This action behaves in two different ways: If there is no current text
	 * hover, the javadoc is displayed using information presenter. If there is
	 * a current text hover, it is converted into a information presenter in
	 * order to make it sticky.
	 */
	class InformationDispatchAction extends TextEditorAction {

		/** The wrapped text operation action. */
		private final TextOperationAction fTextOperationAction;

		/**
		 * Creates a dispatch action.
		 * 
		 * @param resourceBundle
		 *            the resource bundle
		 * @param prefix
		 *            the prefix
		 * @param textOperationAction
		 *            the text operation action
		 */
		public InformationDispatchAction(final ResourceBundle resourceBundle,
				final String prefix,
				final TextOperationAction textOperationAction) {
			super(resourceBundle, prefix, ErlangEditor.this);
			if (textOperationAction == null) {
				throw new IllegalArgumentException();
			}
			fTextOperationAction = textOperationAction;
		}

		/*
		 * @see org.eclipse.jface.action.IAction#run()
		 */
		@SuppressWarnings("synthetic-access")
		@Override
		public void run() {

			/**
			 * Information provider used to present the information.
			 * 
			 * @since 3.0
			 */
			class InformationProvider implements IInformationProvider,
					IInformationProviderExtension,
					IInformationProviderExtension2 {

				private final IRegion fHoverRegion;

				private final String fHoverInfo;

				private final IInformationControlCreator fControlCreator;

				InformationProvider(final IRegion hoverRegion,
						final String hoverInfo,
						final IInformationControlCreator controlCreator) {
					fHoverRegion = hoverRegion;
					fHoverInfo = hoverInfo;
					fControlCreator = controlCreator;
				}

				/*
				 * @seeorg.eclipse.jface.text.information.IInformationProvider#
				 * getSubject(org.eclipse.jface.text.ITextViewer, int)
				 */
				public IRegion getSubject(final ITextViewer textViewer,
						final int invocationOffset) {
					return fHoverRegion;
				}

				public Object getInformation2(final ITextViewer textViewer,
						final IRegion subject) {
					return fHoverInfo;
				}

				/*
				 * @see org.eclipse.jface.text.information.IInformationProviderExtension2
				 *      #getInformationPresenterControlCreator()
				 * 
				 * @since 3.0
				 */
				public IInformationControlCreator getInformationPresenterControlCreator() {
					return fControlCreator;
				}

				@Deprecated
				public String getInformation(final ITextViewer textViewer,
						final IRegion subject) {
					return null;
				}
			}

			final ISourceViewer sourceViewer = getSourceViewer();
			if (sourceViewer == null) {
				fTextOperationAction.run();
				return;
			}

			if (sourceViewer instanceof ITextViewerExtension4) {
				final ITextViewerExtension4 extension4 = (ITextViewerExtension4) sourceViewer;
				if (extension4.moveFocusToWidgetToken()) {
					return;
				}
			}

			if (!(sourceViewer instanceof ITextViewerExtension2)) {
				fTextOperationAction.run();
				return;
			}

			final ITextViewerExtension2 textViewerExtension2 = (ITextViewerExtension2) sourceViewer;

			// does a text hover exist?
			final ITextHover textHover = textViewerExtension2
					.getCurrentTextHover();
			if (textHover == null) {
				// TODO this crashes...
				// fTextOperationAction.run();
				return;
			}

			final Point hoverEventLocation = textViewerExtension2
					.getHoverEventLocation();
			final int offset = computeOffsetAtLocation(sourceViewer,
					hoverEventLocation.x, hoverEventLocation.y);
			if (offset == -1) {
				fTextOperationAction.run();
				return;
			}

			try {
				// get the text hover content
				final String contentType = TextUtilities.getContentType(
						sourceViewer.getDocument(),
						IErlangPartitions.ERLANG_PARTITIONING, offset, true);

				final IRegion hoverRegion = textHover.getHoverRegion(
						sourceViewer, offset);
				if (hoverRegion == null) {
					return;
				}

				final String hoverInfo = textHover.getHoverInfo(sourceViewer,
						hoverRegion);

				IInformationControlCreator controlCreator = null;
				if (textHover instanceof IInformationProviderExtension2) {
					controlCreator = ((IInformationProviderExtension2) textHover)
							.getInformationPresenterControlCreator();
				}

				final IInformationProvider informationProvider = new InformationProvider(
						hoverRegion, hoverInfo, controlCreator);

				fInformationPresenter.setOffset(offset);
				fInformationPresenter
						.setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING);
				fInformationPresenter.setInformationProvider(
						informationProvider, contentType);
				fInformationPresenter.showInformation();

			} catch (final BadLocationException e) {
			}
		}

		// modified version from TextViewer
		private int computeOffsetAtLocation(final ITextViewer textViewer,
				final int x, final int y) {

			final StyledText styledText = textViewer.getTextWidget();
			final IDocument document = textViewer.getDocument();

			if (document == null) {
				return -1;
			}

			try {
				final int widgetLocation = styledText
						.getOffsetAtLocation(new Point(x, y));
				if (textViewer instanceof ITextViewerExtension5) {
					final ITextViewerExtension5 extension = (ITextViewerExtension5) textViewer;
					return extension.widgetOffset2ModelOffset(widgetLocation);
				}
				final IRegion visibleRegion = textViewer.getVisibleRegion();
				return widgetLocation + visibleRegion.getOffset();
			} catch (final IllegalArgumentException e) {
				return -1;
			}

		}
	}

	/**
	 * Returns the annotation closest to the given range respecting the given
	 * direction. If an annotation is found, the annotations current position is
	 * copied into the provided annotation position.
	 * 
	 * @param offset
	 *            the region offset
	 * @param length
	 *            the region length
	 * @param forward
	 *            <code>true</code> for forwards, <code>false</code> for
	 *            backward
	 * @param annotationPosition
	 *            the position of the found annotation
	 * @return the found annotation
	 */
	@SuppressWarnings("null")
	private Annotation getNextAnnotation(final int offset, final int length,
			boolean forward, final Position annotationPosition) {

		Annotation nextAnnotation = null;
		Position nextAnnotationPosition = null;
		Annotation containingAnnotation = null;
		Position containingAnnotationPosition = null;
		boolean currentAnnotation = false;

		final IDocument document = getDocumentProvider().getDocument(
				getEditorInput());
		final int endOfDocument = document.getLength();
		int distance = Integer.MAX_VALUE;

		final IAnnotationModel model = getDocumentProvider()
				.getAnnotationModel(getEditorInput());
		final Iterator<Annotation> e = new ErlangAnnotationIterator(model,
				true, true);
		while (e.hasNext()) {
			final Annotation a = e.next();
			if (a instanceof IErlangAnnotation
					&& ((IErlangAnnotation) a).hasOverlay()
					|| !isNavigationTarget(a)) {
				continue;
			}

			final Position p = model.getPosition(a);
			if (p == null) {
				continue;
			}

			if (forward && p.offset == offset || !forward
					&& p.offset + p.getLength() == offset + length) {// ||
				// p.includes(offset))
				// {
				if (containingAnnotation == null || forward
						&& p.length >= containingAnnotationPosition.length
						|| !forward
						&& p.length >= containingAnnotationPosition.length) {
					containingAnnotation = a;
					containingAnnotationPosition = p;
					currentAnnotation = p.length == length;
				}
			} else {
				int currentDistance = 0;

				if (forward) {
					currentDistance = p.getOffset() - offset;
					if (currentDistance < 0) {
						currentDistance = endOfDocument + currentDistance;
					}

					if (currentDistance < distance
							|| currentDistance == distance
							&& p.length < nextAnnotationPosition.length) {
						distance = currentDistance;
						nextAnnotation = a;
						nextAnnotationPosition = p;
					}
				} else {
					currentDistance = offset + length
							- (p.getOffset() + p.length);
					if (currentDistance < 0) {
						currentDistance = endOfDocument + currentDistance;
					}

					if (currentDistance < distance
							|| currentDistance == distance
							&& p.length < nextAnnotationPosition.length) {
						distance = currentDistance;
						nextAnnotation = a;
						nextAnnotationPosition = p;
					}
				}
			}
		}
		if (containingAnnotationPosition != null
				&& (!currentAnnotation || nextAnnotation == null)) {
			annotationPosition.setOffset(containingAnnotationPosition
					.getOffset());
			annotationPosition.setLength(containingAnnotationPosition
					.getLength());
			return containingAnnotation;
		}
		if (nextAnnotationPosition != null) {
			annotationPosition.setOffset(nextAnnotationPosition.getOffset());
			annotationPosition.setLength(nextAnnotationPosition.getLength());
		}

		return nextAnnotation;
	}

	/**
	 * Returns whether the given annotation is configured as a target for the
	 * "Go to Next/Previous Annotation" actions
	 * 
	 * @param annotation
	 *            the annotation
	 * @return <code>true</code> if this is a target, <code>false</code>
	 *         otherwise
	 * @since 3.0
	 */
	@Override
	protected boolean isNavigationTarget(final Annotation annotation) {
		final Preferences preferences = EditorsUI.getPluginPreferences();
		final AnnotationPreference preference = getAnnotationPreferenceLookup()
				.getAnnotationPreference(annotation);
		// See bug 41689
		// String key= forward ? preference.getIsGoToNextNavigationTargetKey() :
		// preference.getIsGoToPreviousNavigationTargetKey();
		final String key = preference == null ? null : preference
				.getIsGoToNextNavigationTargetKey();
		return key != null && preferences.getBoolean(key);
	}

	@Override
	public Annotation gotoAnnotation(final boolean forward) {
		final ITextSelection selection = (ITextSelection) getSelectionProvider()
				.getSelection();
		final Position position = new Position(0, 0);
		if (false) {
			getNextAnnotation(selection.getOffset(), selection.getLength(),
					forward, position);
			selectAndReveal(position.getOffset(), position.getLength());
			return null;
		}
		final Annotation annotation = getNextAnnotation(selection.getOffset(),
				selection.getLength(), forward, position);
		setStatusLineErrorMessage(null);
		setStatusLineMessage(null);
		if (annotation != null) {
			updateAnnotationViews(annotation);
			selectAndReveal(position.getOffset(), position.getLength());
			setStatusLineMessage(annotation.getText());
		}
		return annotation;
	}

	private void updateAnnotationViews(final Annotation annotation) {
		// TODO Auto-generated method stub

	}

	// // the window is asymmetric, to allow finding number of arguments
	// public TokenWindow getTokenWindow(int window) {
	// final IErlScanner scanner = getScanner();
	//
	// final ISelection sel = getSelectionProvider().getSelection();
	// final ITextSelection s = (ITextSelection) sel;
	//
	// final TokenWindow tokenWindow = scanner.getTokenWindow(s.getOffset(),
	// window);
	// return tokenWindow;
	// }

	public final ISourceViewer getViewer() {
		return getSourceViewer();
	}

	public final IDocument getDocument() {
		final ISourceViewer v = getViewer();
		if (v == null) {
			return null;
		}
		return v.getDocument();
	}

	public ViewerComparator createDefaultOutlineComparator() {
		return null;
	}

	public ViewerComparator createOutlineComparator() {
		return null;
	}

	public ITreeContentProvider createOutlineContentProvider() {
		return new ErlangContentProvider(false);
	}

	public ILabelProvider createOutlineLabelProvider() {
		return new ErlangLabelProvider();
	}

	public Object getOutlineInput() {
		return getModule();
	}

	public ISortableContentOutlinePage getContentOutline() {
		return myOutlinePage;
	}

	public void updateSelection(final SelectionChangedEvent event) {
		final ISelection sel = event.getSelection();
		if (sel instanceof IStructuredSelection) {
			final IStructuredSelection structuredSelection = (IStructuredSelection) sel;
			updateSelection(structuredSelection.getFirstElement());
		}
	}

	public void updateSelection(final Object sel) {
	}

	public void selectionChanged(final SelectionChangedEvent event) {
		if (event.getSource() == getSelectionProvider()) {
			return;
		}
		final ISelection sel = event.getSelection();
		if (sel instanceof ITextSelection) {
			return;
		}
		if (sel instanceof IStructuredSelection) {
			fSelection = ((IStructuredSelection) sel).getFirstElement();
		} else {
			fSelection = null;
		}
	}

	public Object getSelection() {
		return fSelection;
	}

	@Override
	public void doSave(final IProgressMonitor progressMonitor) {
		// TODO: maybe this should be in a resource change listener?
		super.doSave(progressMonitor);
		((EditorConfiguration) getSourceViewerConfiguration())
				.resetReconciler();
	}

	private static class ExclusivePositionUpdater implements IPositionUpdater {

		/** The position category. */
		private final String fCategory;

		/**
		 * Creates a new updater for the given <code>category</code>.
		 * 
		 * @param category
		 *            the new category.
		 */
		public ExclusivePositionUpdater(final String category) {
			fCategory = category;
		}

		/*
		 * @see org.eclipse.jface.text.IPositionUpdater#update(org.eclipse.jface.
		 *      text.DocumentEvent)
		 */
		public void update(final DocumentEvent event) {

			final int eventOffset = event.getOffset();
			final int eventOldLength = event.getLength();
			final int eventNewLength = event.getText() == null ? 0 : event
					.getText().length();
			final int deltaLength = eventNewLength - eventOldLength;

			try {
				final Position[] positions = event.getDocument().getPositions(
						fCategory);

				for (int i = 0; i != positions.length; i++) {

					final Position position = positions[i];

					if (position.isDeleted()) {
						continue;
					}

					final int offset = position.getOffset();
					final int length = position.getLength();
					final int end = offset + length;

					if (offset >= eventOffset + eventOldLength) {
						// position comes
						// after change - shift
						position.setOffset(offset + deltaLength);
					} else if (end <= eventOffset) {
						// position comes way before change -
						// leave alone
					} else if (offset <= eventOffset
							&& end >= eventOffset + eventOldLength) {
						// event completely internal to the position - adjust
						// length
						position.setLength(length + deltaLength);
					} else if (offset < eventOffset) {
						// event extends over end of position - adjust length
						final int newEnd = eventOffset;
						position.setLength(newEnd - offset);
					} else if (end > eventOffset + eventOldLength) {
						// event extends from before position into it - adjust
						// offset
						// and length
						// offset becomes end of event, length adjusted
						// accordingly
						final int newOffset = eventOffset + eventNewLength;
						position.setOffset(newOffset);
						position.setLength(end - newOffset);
					} else {
						// event consumes the position - delete it
						position.delete();
					}
				}
			} catch (final BadPositionCategoryException e) {
				// ignore and return
			}
		}

		/**
		 * Returns the position category.
		 * 
		 * @return the position category
		 */
		public String getCategory() {
			return fCategory;
		}

	}

	public static class BracketLevel {
		int fOffset;
		int fLength;
		LinkedModeUI fUI;
		Position fFirstPosition;
		Position fSecondPosition;
	}

	class BracketInserter implements VerifyKeyListener, ILinkedModeListener {

		private boolean fCloseBraces = false;
		private boolean fCloseBrackets = false;
		private boolean fCloseStrings = false;
		private boolean fCloseParens = false;
		private boolean fCloseAtoms = false;

		private final String CATEGORY = toString();
		private final IPositionUpdater fUpdater = new ExclusivePositionUpdater(
				CATEGORY);
		private final Stack<BracketLevel> fBracketLevelStack = new Stack<BracketLevel>();

		public void setCloseBracketsEnabled(final boolean enabled) {
			fCloseBrackets = enabled;
		}

		public void setCloseAtomsEnabled(final boolean enabled) {
			fCloseAtoms = enabled;
		}

		public void setCloseParensEnabled(final boolean enabled) {
			fCloseParens = enabled;
		}

		public void setCloseBracesEnabled(final boolean enabled) {
			fCloseBraces = enabled;
		}

		public void setCloseStringsEnabled(final boolean enabled) {
			fCloseStrings = enabled;
		}

		private boolean isStopper(final String kind) {
			return kind.equals("(") || kind.equals(")") || kind.equals("{")
					|| kind.equals("}") || kind.equals("[") || kind.equals("]")
					|| kind.equals("'") || kind.equals("\"")
					|| kind.equals("atom");
		}

		/*
		 * @see org.eclipse.swt.custom.VerifyKeyListener#verifyKey(org.eclipse.swt
		 *      .events.VerifyEvent)
		 */
		@SuppressWarnings("synthetic-access")
		public void verifyKey(final VerifyEvent event) {

			// early pruning to slow down normal typing as little as possible
			if (!event.doit || getInsertMode() != SMART_INSERT) {
				return;
			}
			switch (event.character) {
			case '(':
			case '{':
			case '[':
			case '\'':
			case '\"':
				break;
			default:
				return;
			}

			final ISourceViewer sourceViewer = getSourceViewer();
			final IDocument document = sourceViewer.getDocument();

			final Point selection = sourceViewer.getSelectedRange();
			final int offset = selection.x;
			final int length = selection.y;

			try {
				// final IRegion startLine = document
				// .getLineInformationOfOffset(offset);
				final IRegion endLine = document
						.getLineInformationOfOffset(offset + length);

				List<ErlToken> tokens = null;
				final int getOffset = offset + length, getLength = endLine
						.getOffset()
						+ endLine.getLength() - getOffset;
				final String str = getDocument().get(getOffset, getLength);
				try {
					tokens = ErlideScanner2.lightScanString(str, 0);
				} catch (final BackendException e) {
				}

				String kind = "";
				if (tokens != null && tokens.size() > 0) {
					kind = tokens.get(0).getKind();
				} else if (str.length() > 0) {
					kind = str.substring(0, 1);
				}
				if (isStopper(kind)) {
					return;
				}

				switch (event.character) {
				case '(':
					if (!fCloseParens || kind.equals(")")) {
						return;
					}
					break;

				case '[':
					if (!fCloseBrackets || kind.equals("]")) {
						return;
					}
					break;
				case '{':
					if (!fCloseBraces || kind.equals("}")) {
						return;
					}
					break;
				case '\'':
					if (!fCloseAtoms || kind.equals("'")) {
						return;
					}
					break;
				case '"':
					if (!fCloseStrings || kind.equals("\"")) {
						return;
					}
					break;

				default:
					return;
				}

				if (!validateEditorInputState()) {
					return;
				}

				final char character = event.character;
				final char closingCharacter = getPeerCharacter(character);
				final StringBuffer buffer = new StringBuffer();
				buffer.append(character);
				buffer.append(closingCharacter);

				document.replace(offset, length, buffer.toString());

				final BracketLevel level = new BracketLevel();
				fBracketLevelStack.push(level);

				final LinkedPositionGroup group = new LinkedPositionGroup();
				group.addPosition(new LinkedPosition(document, offset + 1, 0,
						LinkedPositionGroup.NO_STOP));

				final LinkedModeModel model = new LinkedModeModel();
				model.addLinkingListener(this);
				model.addGroup(group);
				model.forceInstall();

				level.fOffset = offset;
				level.fLength = 2;

				// set up position tracking for our magic peers
				if (fBracketLevelStack.size() == 1) {
					document.addPositionCategory(CATEGORY);
					document.addPositionUpdater(fUpdater);
				}
				level.fFirstPosition = new Position(offset, 1);
				level.fSecondPosition = new Position(offset + 1, 1);
				document.addPosition(CATEGORY, level.fFirstPosition);
				document.addPosition(CATEGORY, level.fSecondPosition);

				level.fUI = new EditorLinkedModeUI(model, sourceViewer);
				level.fUI.setSimpleMode(true);
				level.fUI.setExitPolicy(new ExitPolicy(closingCharacter,
						getEscapeCharacter(closingCharacter),
						fBracketLevelStack));
				level.fUI.setExitPosition(sourceViewer, offset + 2, 0,
						Integer.MAX_VALUE);
				level.fUI.setCyclingMode(LinkedModeUI.CYCLE_NEVER);
				level.fUI.enter();

				final IRegion newSelection = level.fUI.getSelectedRegion();
				sourceViewer.setSelectedRange(newSelection.getOffset(),
						newSelection.getLength());

				event.doit = false;

			} catch (final BadLocationException e) {
				ErlLogger.error(e);
			} catch (final BadPositionCategoryException e) {
				ErlLogger.error(e);
			}
		}

		/*
		 * @see org.eclipse.jface.text.link.ILinkedModeListener#left(org.eclipse.
		 *      jface.text.link.LinkedModeModel, int)
		 */
		@SuppressWarnings("synthetic-access")
		public void left(final LinkedModeModel environment, final int flags) {

			final BracketLevel level = fBracketLevelStack.pop();

			if (flags != ILinkedModeListener.EXTERNAL_MODIFICATION) {
				return;
			}

			// remove brackets
			final ISourceViewer sourceViewer = getSourceViewer();
			final IDocument document = sourceViewer.getDocument();
			if (document instanceof IDocumentExtension) {
				final IDocumentExtension extension = (IDocumentExtension) document;
				extension.registerPostNotificationReplace(null,
						new IDocumentExtension.IReplace() {

							public void perform(final IDocument d,
									final IDocumentListener owner) {
								if ((level.fFirstPosition.isDeleted || level.fFirstPosition.length == 0)
										&& !level.fSecondPosition.isDeleted
										&& level.fSecondPosition.offset == level.fFirstPosition.offset) {
									try {
										document.replace(
												level.fSecondPosition.offset,
												level.fSecondPosition.length,
												""); //$NON-NLS-1$
									} catch (final BadLocationException e) {
										ErlLogger.error(e);
									}
								}

								if (fBracketLevelStack.size() == 0) {
									document.removePositionUpdater(fUpdater);
									try {
										document
												.removePositionCategory(CATEGORY);
									} catch (final BadPositionCategoryException e) {
										ErlLogger.error(e);
									}
								}
							}
						});
			}

		}

		/*
		 * @see org.eclipse.jface.text.link.ILinkedModeListener#suspend(org.eclipse
		 *      .jface.text.link.LinkedModeModel)
		 */
		public void suspend(final LinkedModeModel environment) {
		}

		/*
		 * @see org.eclipse.jface.text.link.ILinkedModeListener#resume(org.eclipse
		 *      .jface.text.link.LinkedModeModel, int)
		 */
		public void resume(final LinkedModeModel environment, final int flags) {
		}
	}

	private static char getPeerCharacter(final char character) {
		switch (character) {
		case '(':
			return ')';

		case ')':
			return '(';

		case '{':
			return '}';

		case '}':
			return '{';

		case '[':
			return ']';

		case ']':
			return '[';

		case '"':
			return character;

		case '\'':
			return character;

		default:
			throw new IllegalArgumentException();
		}
	}

	static char getEscapeCharacter(final char character) {
		switch (character) {
		case '"':
		case '\'':
			return '\\';
		default:
			return 0;
		}
	}

	private class ExitPolicy implements IExitPolicy {

		final char fExitCharacter;
		final char fEscapeCharacter;
		final Stack<BracketLevel> fStack;
		final int fSize;

		public ExitPolicy(final char exitCharacter, final char escapeCharacter,
				final Stack<BracketLevel> stack) {
			fExitCharacter = exitCharacter;
			fEscapeCharacter = escapeCharacter;
			fStack = stack;
			fSize = fStack.size();
		}

		/*
		 * @see org.eclipse.jdt.internal.ui.text.link.LinkedPositionUI.ExitPolicy
		 *      #doExit(org.eclipse.jdt.internal.ui.text.link.LinkedPositionManager,
		 *      org.eclipse.swt.events.VerifyEvent, int, int)
		 */
		@SuppressWarnings("synthetic-access")
		public ExitFlags doExit(final LinkedModeModel model,
				final VerifyEvent event, final int offset, final int length) {

			if (fSize == fStack.size() && !isMasked(offset)) {
				if (event.character == fExitCharacter) {
					final BracketLevel level = fStack.peek();
					if (level.fFirstPosition.offset > offset
							|| level.fSecondPosition.offset < offset) {
						return null;
					}
					if (level.fSecondPosition.offset == offset && length == 0) {
						// don't enter the character if if its the closing peer
						return new ExitFlags(ILinkedModeListener.UPDATE_CARET,
								false);
					}
				}
				// when entering an anonymous class between the parenthesis', we
				// don't want
				// to jump after the closing parenthesis when return is pressed
				if (event.character == SWT.CR && offset > 0) {
					final IDocument document = getSourceViewer().getDocument();
					try {
						if (document.getChar(offset - 1) == '{') {
							return new ExitFlags(ILinkedModeListener.EXIT_ALL,
									true);
						}
					} catch (final BadLocationException e) {
					}
				}
			}
			return null;
		}

		@SuppressWarnings("synthetic-access")
		private boolean isMasked(final int offset) {
			final IDocument document = getSourceViewer().getDocument();
			try {
				return fEscapeCharacter == document.getChar(offset - 1);
			} catch (final BadLocationException e) {
			}
			return false;
		}
	}

	public ActionGroup getActionGroup() {
		return fActionGroups;
	}

}
