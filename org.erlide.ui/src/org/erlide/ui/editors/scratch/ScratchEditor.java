/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.scratch;

/**
 * 
 * 
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.runtime.backend.BackendEvalResult;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.util.IContextMenuConstants;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * An editor for Erlang scratchs.
 */
public class ScratchEditor extends AbstractDecoratedTextEditor {

	public enum Result {
		DISPLAY, RUN, INSPECT;
	}

	private Result fResultMode; // one of the RESULT_* constants

	List<IScratchStateChangedListener> fScratchStateListeners;

	private boolean fEvaluating;

	private int fScratchStart;

	private int fScratchEnd;

	private Image fOldTitleImage = null;

	// private String fResult;

	private OtpErlangObject fBindings = new OtpErlangList();

	public ScratchEditor() {
		super();
		setDocumentProvider(new ScratchFileDocumentProvider());
		fScratchStateListeners = new ArrayList<IScratchStateChangedListener>(4);
		// setPreferenceStore(ErlangPlugin.getDefault().getPreferenceStore());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchPart#dispose()
	 */
	@Override
	public void dispose() {
		fScratchStateListeners = null;
		super.dispose();
	}

	/**
	 * Actions for the editor popup menu
	 * 
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#createActions()
	 */
	@Override
	protected void createActions() {
		super.createActions();
		if (getFile() != null) {
			setAction("Evaluate", new EvaluateAction(this)); //$NON-NLS-1$
			setAction("Stop", new StopAction(this)); //$NON-NLS-1$
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#editorContextMenuAboutToShow(org.eclipse.jface.action.IMenuManager)
	 */
	@Override
	protected void editorContextMenuAboutToShow(IMenuManager menu) {
		super.editorContextMenuAboutToShow(menu);
		addGroup(menu, ITextEditorActionConstants.GROUP_EDIT,
				IContextMenuConstants.GROUP_GENERATE);
		addGroup(menu, ITextEditorActionConstants.GROUP_FIND,
				IContextMenuConstants.GROUP_SEARCH);
		addGroup(menu, IContextMenuConstants.GROUP_SEARCH,
				IContextMenuConstants.GROUP_SHOW);
		if (getFile() != null) {
			addAction(menu, IContextMenuConstants.GROUP_ADDITIONS, "Evaluate");
			addAction(menu, IContextMenuConstants.GROUP_ADDITIONS, "Stop");
		}
	}

	public boolean isEvaluating() {
		return fEvaluating;
	}

	public void evalSelection(Result resultMode) {
		if (!isInErlProject()) {
			reportNotInErlProjectError();
			return;
		}
		if (isEvaluating()) {
			return;
		}

		evaluationStarts();

		fResultMode = resultMode;
		build();

		fireEvalStateChanged();

		final ITextSelection selection = (ITextSelection) getSelectionProvider()
				.getSelection();
		final String scratch = selection.getText().trim();
		fScratchStart = selection.getOffset();
		fScratchEnd = fScratchStart + selection.getLength();

		evaluate(scratch);

	}

	protected void build() {
		final IErlProject erlProject = getErlProject();
		if (erlProject == null) {
			return;
		}
		final boolean build = !erlProject.getProject().getWorkspace()
				.isAutoBuilding()
				|| !erlProject.hasBuildState();

		if (build) {
			if (!performIncrementalBuild()) {
				return;
			}
		}

		checkMultipleEditors();
	}

	protected boolean performIncrementalBuild() {
		final IRunnableWithProgress r = new IRunnableWithProgress() {

			public void run(IProgressMonitor pm)
					throws InvocationTargetException {
				try {
					getErlProject().getProject().build(
							IncrementalProjectBuilder.INCREMENTAL_BUILD, pm);
				} catch (CoreException e) {
					throw new InvocationTargetException(e);
				}
			}
		};
		try {
			PlatformUI.getWorkbench().getProgressService().run(true, false, r);
		} catch (final InterruptedException e) {
			evaluationEnds();
			return false;
		} catch (final InvocationTargetException e) {
			evaluationEnds();
			return false;
		}
		return true;
	}

	protected void checkMultipleEditors() {
		// multiple editors are opened on the same page
	}

	protected IErlProject getErlProject() {
		try {
			return findErlProject();
		} catch (final ErlModelException e) {
			return null;
		}
	}

	public void addScratchStateChangedListener(
			IScratchStateChangedListener listener) {
		if (fScratchStateListeners != null
				&& !fScratchStateListeners.contains(listener)) {
			fScratchStateListeners.add(listener);
		}
	}

	public void removeScratchStateChangedListener(
			IScratchStateChangedListener listener) {
		if (fScratchStateListeners != null) {
			fScratchStateListeners.remove(listener);
		}
	}

	protected void fireEvalStateChanged() {
		final Runnable r = new Runnable() {

			public void run() {
				Shell shell = getShell();
				if (fScratchStateListeners != null && shell != null
						&& !shell.isDisposed()) {
					List<IScratchStateChangedListener> v = new ArrayList<IScratchStateChangedListener>(
							fScratchStateListeners);
					for (int i = 0; i < v.size(); i++) {
						IScratchStateChangedListener l = v.get(i);
						l.scratchStateChanged(ScratchEditor.this);
					}
				}
			}
		};
		final Shell shell = getShell();
		if (shell != null) {
			getShell().getDisplay().asyncExec(r);
		}
	}

	protected void evaluate(String scratch) {
		if (scratch.charAt(scratch.length() - 1) != '.') {
			scratch += '.';
		}

		// FIXME IBackend
		final BackendEvalResult r = BackendUtil.eval(null, scratch, fBindings);

		if (r.isOk()) {
			fBindings = r.getBindings();
			switch (fResultMode) {
			case DISPLAY:
				displayResult(r.getValue());
				break;
			case INSPECT:
				// String res = formatInspect(r.getValue());
				break;
			case RUN:
				// no action
				break;
			}
		} else {
			// FIXME IBackend
			final String msg = BackendUtil.format(null, "~p", r
					.getErrorReason());
			showError("Evaluation failed. Reason: \n" + msg);
		}
		evaluationEnds();
	}

	/**
	 * @param r
	 */
	protected String formatInspect(OtpErlangObject r) {
		String res = r.toString();
		final int resLength = res.length();
		if (resLength > 30) {
			res = res.substring(0, 15)
					+ ScratchMessages.getString("ScratchEditor.ellipsis") + res.substring(resLength - 15, resLength); //$NON-NLS-1$
		}
		res = res.replace('\n', ' ');
		res = res.replace('\r', ' ');
		res = res.replace('\t', ' ');
		return res;
	}

	protected void showError(IStatus status) {
		evaluationEnds();
		if (!status.isOK()) {
			ErrorDialog
					.openError(
							getShell(),
							ScratchMessages
									.getString("ScratchEditor.error.evaluating2"), null, status); //$NON-NLS-1$
		}
	}

	protected void showError(String message) {
		final Status status = new Status(IStatus.ERROR,
				ErlideUIPlugin.PLUGIN_ID, IStatus.ERROR, message, null);
		showError(status);
	}

	protected void displayResult(OtpErlangObject result) {
		// FIXME IBackend
		final String message = BackendUtil.format(null, "~p", result);// result.toString();
		try {
			getSourceViewer().getDocument().replace(fScratchEnd, 0, message);
			selectAndReveal(fScratchEnd, message.length());
		} catch (final BadLocationException e) {
		}
	}

	protected void showAllErrors(final String[] errors) {
		final IDocument document = getSourceViewer().getDocument();
		final String delimiter = document.getLegalLineDelimiters()[0];

		final StringBuffer errorString = new StringBuffer();
		for (String element : errors) {
			errorString.append(element).append(delimiter);
		}

		final Runnable r = new Runnable() {

			@SuppressWarnings("synthetic-access")
			public void run() {
				try {
					getSourceViewer().getDocument().replace(fScratchStart, 0,
							errorString.toString());
					selectAndReveal(fScratchStart, errorString.length());
				} catch (BadLocationException e) {
				}
			}
		};
		async(r);
	}

	protected IErlProject findErlProject() throws ErlModelException {
		final Object input = getEditorInput();
		if (input instanceof IFileEditorInput) {
			final IFileEditorInput file = (IFileEditorInput) input;
			final IProject p = file.getFile().getProject();
			try {
				if (p.getNature(ErlangPlugin.NATURE_ID) != null) {
					return ErlangCore.getModelManager().create(p);
				}
			} catch (final CoreException ce) {
				throw new ErlModelException(ce);
			}
		}
		return null;
	}

	protected synchronized void evaluationStarts() {
		fEvaluating = true;
		setTitleImage();
		fireEvalStateChanged();
		showStatus(ScratchMessages.getString("ScratchEditor.evaluating")); //$NON-NLS-1$
		getSourceViewer().setEditable(false);
	}

	/**
	 * Sets the tab image to indicate whether in the process of evaluating or
	 * not.
	 */
	protected void setTitleImage() {
		Image image = null;
		if (fEvaluating) {
			fOldTitleImage = getTitleImage();
			// image =
			// JavaDebugImages.get(JavaDebugImages.IMG_OBJS_SNIPPET_EVALUATING);
		} else {
			image = fOldTitleImage;
			fOldTitleImage = null;
		}
		if (image != null) {
			setTitleImage(image);
		}
	}

	protected void evaluationEnds() {
		final Runnable r = new Runnable() {

			@SuppressWarnings("synthetic-access")
			public void run() {
				fEvaluating = false;
				setTitleImage();
				fireEvalStateChanged();
				showStatus(""); //$NON-NLS-1$
				getSourceViewer().setEditable(true);
			}
		};
		async(r);
	}

	protected void showStatus(String message) {
		final IEditorSite site = (IEditorSite) getSite();
		final EditorActionBarContributor contributor = (EditorActionBarContributor) site
				.getActionBarContributor();
		contributor.getActionBars().getStatusLineManager().setMessage(message);
	}

	protected Shell getShell() {
		return getSite().getShell();
	}

	/**
	 * Return the <code>IFile</code> associated with the current editor input.
	 * Will return <code>null</code> if the current editor input is for an
	 * external file
	 */
	public IFile getFile() {
		final IEditorInput input = getEditorInput();
		if (input instanceof IFileEditorInput) {
			return ((IFileEditorInput) input).getFile();
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#updateSelectionDependentActions()
	 */
	@Override
	protected void updateSelectionDependentActions() {
		super.updateSelectionDependentActions();
		fireEvalStateChanged();
	}

	/**
	 * Returns whether this editor has been opened on a resource that is in a
	 * Erlang project.
	 */
	protected boolean isInErlProject() {
		try {
			return findErlProject() != null;
		} catch (final ErlModelException jme) {
		}
		return false;
	}

	/**
	 * Displays an error dialog indicating that evaluation cannot occur outside
	 * of a Erlang Project.
	 */
	protected void reportNotInErlProjectError() {
		String projectName = null;
		final Object input = getEditorInput();
		if (input instanceof IFileEditorInput) {
			final IFileEditorInput file = (IFileEditorInput) input;
			final IProject p = file.getFile().getProject();
			projectName = p.getName();
		}
		String message = ""; //$NON-NLS-1$
		if (projectName != null) {
			message = projectName
					+ ScratchMessages
							.getString("ScratchEditor._is_not_a_Erlang_Project._n_1"); //$NON-NLS-1$
		}
		showError(message
				+ ScratchMessages
						.getString("ScratchEditor.Unable_to_perform_evaluation_outside_of_a_Erlang_Project_2")); //$NON-NLS-1$
	}

	/**
	 * Asks the user for the workspace path of a file resource and saves the
	 * document there.
	 * 
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#performSaveAs(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	protected void performSaveAs(IProgressMonitor progressMonitor) {
		final Shell shell = getSite().getShell();
		final SaveAsDialog dialog = new SaveAsDialog(shell);
		dialog.open();
		final IPath path = dialog.getResult();

		if (path == null) {
			if (progressMonitor != null) {
				progressMonitor.setCanceled(true);
			}
			return;
		}

		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IFile file = workspace.getRoot().getFile(path);
		final IEditorInput newInput = new FileEditorInput(file);

		final WorkspaceModifyOperation op = new WorkspaceModifyOperation() {

			@Override
			public void execute(final IProgressMonitor monitor)
					throws CoreException {
				IDocumentProvider dp = getDocumentProvider();
				dp.saveDocument(monitor, newInput, dp
						.getDocument(getEditorInput()), true);
			}
		};

		boolean success = false;
		try {
			getDocumentProvider().aboutToChange(newInput);
			PlatformUI.getWorkbench().getProgressService().busyCursorWhile(op);
			success = true;
		} catch (final InterruptedException x) {
		} catch (final InvocationTargetException x) {
			final String title = ScratchMessages
					.getString("ScratchEditor.Problems_During_Save_As..._3"); //$NON-NLS-1$
			final String msg = ScratchMessages
					.getString("ScratchEditor.Save_could_not_be_completed.__4") + x.getTargetException().getMessage(); //$NON-NLS-1$
			MessageDialog.openError(shell, title, msg);
		} finally {
			getDocumentProvider().changed(newInput);
			if (success) {
				setInput(newInput);
			}
		}

		if (progressMonitor != null) {
			progressMonitor.setCanceled(!success);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.ISaveablePart#isSaveAsAllowed()
	 */
	@Override
	public boolean isSaveAsAllowed() {
		return true;
	}

	/**
	 * Returns the working directory attribute for this scrapbook
	 */
	protected String getWorkingDirectoryAttribute() {
		final IFile file = getFile();
		if (file != null) {
			// try
			// {
			// //return ScrapbookLauncher.getWorkingDirectoryAttribute(file);
			// }
			// catch (CoreException e)
			// {
			// }
		}
		return null;
	}

	/**
	 * Executes the given runnable in the Display thread
	 */
	protected void async(Runnable r) {
		final Control control = getVerticalRuler().getControl();
		if (!control.isDisposed()) {
			control.getDisplay().asyncExec(r);
		}
	}

	protected void showAndSelect(final String text, final int offset) {
		final Runnable r = new Runnable() {

			@SuppressWarnings("synthetic-access")
			public void run() {
				try {
					getSourceViewer().getDocument().replace(offset, 0, text);
				} catch (BadLocationException e) {
				}
				selectAndReveal(offset, text.length());
			}
		};
		async(r);
	}

}
