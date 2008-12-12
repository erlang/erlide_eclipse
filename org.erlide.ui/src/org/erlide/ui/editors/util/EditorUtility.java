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

package org.erlide.ui.editors.util;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.ide.IGotoMarker;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.runtime.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.prefs.PreferenceConstants;

/**
 * A number of routines for working with JavaElements in editors.
 * 
 * Use 'isOpenInEditor' to test if an element is already open in a editor Use
 * 'openInEditor' to force opening an element in a editor With 'getWorkingCopy'
 * you get the working copy (element in the editor) of an element
 */
public class EditorUtility {

	public static boolean isEditorInput(final Object element,
			final IEditorPart editor) {
		if (editor != null) {
			try {
				return editor.getEditorInput().equals(getEditorInput(element));
			} catch (final ErlModelException e) {
				ErlLogger.warn(e);
			}
		}
		return false;
	}

	/**
	 * Tests if a CU is currently shown in an editor
	 * 
	 * @return the IEditorPart if shown, null if element is not open in an
	 *         editor
	 */
	public static IEditorPart isOpenInEditor(final Object inputElement) {
		IEditorInput input = null;

		try {
			input = getEditorInput(inputElement);
		} catch (final ErlModelException e) {
			ErlLogger.warn(e);
		}

		if (input != null) {
			final IWorkbenchPage p = ErlideUIPlugin.getActivePage();
			if (p != null) {
				return p.findEditor(input);
			}
		}

		return null;
	}

	/**
	 * Opens a Java editor for an element such as <code>IJavaElement</code>,
	 * <code>IFile</code>, or <code>IStorage</code>. The editor is
	 * activated by default.
	 * 
	 * @return the IEditorPart or null if wrong element type or opening failed
	 */
	public static IEditorPart openInEditor(final Object inputElement)
			throws ErlModelException, PartInitException {
		return openInEditor(inputElement, true);
	}

	/**
	 * Opens a Java editor for an element (IJavaElement, IFile, IStorage...)
	 * 
	 * @return the IEditorPart or null if wrong element type or opening failed
	 */
	public static IEditorPart openInEditor(final Object inputElement,
			final boolean activate) throws ErlModelException, PartInitException {

		if (inputElement instanceof IFile) {
			return openInEditor((IFile) inputElement, activate);
		}

		final IEditorInput input = getEditorInput(inputElement);
		if (input != null) {
			return openInEditor(input, getEditorID(input, inputElement),
					activate);
		}

		return null;
	}

	/**
	 * Selects a Java Element in an editor
	 */
	public static void revealInEditor(final IEditorPart part,
			final IErlElement element) {
		if (element == null) {
			return;
		}

		if (part instanceof ErlangEditor) {
			((ErlangEditor) part).setSelection(element);
			return;
		}
	}

	/**
	 * Selects and reveals the given region in the given editor part.
	 */
	public static void revealInEditor(final IEditorPart part,
			final IRegion region) {
		if (part != null && region != null) {
			revealInEditor(part, region.getOffset(), region.getLength());
		}
	}

	/**
	 * Selects and reveals the given offset and length in the given editor part.
	 */
	public static void revealInEditor(final IEditorPart editor,
			final int offset, final int length) {
		if (editor instanceof ITextEditor) {
			((ITextEditor) editor).selectAndReveal(offset, length);
			return;
		}

		// Support for non-text editor - try IGotoMarker interface
		if (editor instanceof IGotoMarker) {
			final IEditorInput input = editor.getEditorInput();
			if (input instanceof IFileEditorInput) {
				final IGotoMarker gotoMarkerTarget = (IGotoMarker) editor;
				final WorkspaceModifyOperation op = new WorkspaceModifyOperation() {

					@Override
					protected void execute(IProgressMonitor monitor)
							throws CoreException {
						IMarker marker = null;
						try {
							marker = ((IFileEditorInput) input).getFile()
									.createMarker(IMarker.TEXT);
							marker.setAttribute(IMarker.CHAR_START, offset);
							marker.setAttribute(IMarker.CHAR_END, offset
									+ length);

							gotoMarkerTarget.gotoMarker(marker);

						} finally {
							if (marker != null) {
								marker.delete();
							}
						}
					}
				};

				try {
					op.run(null);
				} catch (final InvocationTargetException ex) {
					// reveal failed
				} catch (final InterruptedException e) {
					Assert.isTrue(false, "this operation can not be canceled"); //$NON-NLS-1$
				}
			}
			return;
		}

		/*
		 * Workaround: send out a text selection XXX: Needs to be improved, see
		 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=32214
		 */
		if (editor != null
				&& editor.getEditorSite().getSelectionProvider() != null) {
			final IEditorSite site = editor.getEditorSite();
			if (site == null) {
				return;
			}

			final ISelectionProvider provider = editor.getEditorSite()
					.getSelectionProvider();
			if (provider == null) {
				return;
			}

			provider.setSelection(new TextSelection(offset, length));
		}
	}

	private static IEditorPart openInEditor(final IFile file,
			final boolean activate) throws PartInitException {
		if (file != null) {
			final IWorkbenchPage p = ErlideUIPlugin.getActivePage();
			if (p != null) {
				final IEditorPart editorPart = IDE
						.openEditor(p, file, activate);
				initializeHighlightRange(editorPart);
				return editorPart;
			}
		}
		return null;
	}

	private static IEditorPart openInEditor(final IEditorInput input,
			final String editorID, final boolean activate)
			throws PartInitException {
		if (input != null) {
			final IWorkbenchPage p = ErlideUIPlugin.getActivePage();
			if (p != null) {
				final IEditorPart editorPart = p.openEditor(input, editorID,
						activate);
				initializeHighlightRange(editorPart);
				return editorPart;
			}
		}
		return null;
	}

	private static void initializeHighlightRange(final IEditorPart editorPart) {
		if (editorPart instanceof ITextEditor) {
			final IAction toggleAction = editorPart
					.getEditorSite()
					.getActionBars()
					.getGlobalActionHandler(
							ITextEditorActionDefinitionIds.TOGGLE_SHOW_SELECTED_ELEMENT_ONLY);
			if (toggleAction != null) {
				boolean enable;
				if (editorPart instanceof ErlangEditor) {
					enable = ErlideUIPlugin.getDefault().getPreferenceStore()
							.getBoolean(
									PreferenceConstants.EDITOR_SHOW_SEGMENTS);
				} else {
					enable = toggleAction.isEnabled()
							&& toggleAction.isChecked();
				}
				if (enable) {
					if (toggleAction instanceof TextEditorAction) {
						// Reset the action
						((TextEditorAction) toggleAction).setEditor(null);
						// Restore the action
						((TextEditorAction) toggleAction)
								.setEditor((ITextEditor) editorPart);
					} else {
						// Un-check
						toggleAction.run();
						// Check
						toggleAction.run();
					}
				}
			}
		}
	}

	/**
	 * @deprecated Made it public again for java debugger UI.
	 */
	@Deprecated
	public static String getEditorID(final IEditorInput input,
			final Object inputObject) {
		IEditorDescriptor editorDescriptor;
		try {
			if (input instanceof IFileEditorInput) {
				editorDescriptor = IDE
						.getEditorDescriptor(((IFileEditorInput) input)
								.getFile());
			} else {
				editorDescriptor = IDE.getEditorDescriptor(input.getName());
			}
		} catch (final PartInitException e) {
			return null;
		}

		if (editorDescriptor != null) {
			return editorDescriptor.getId();
		}

		return null;
	}

	private static IEditorInput getEditorInput(IErlElement element)
			throws ErlModelException {
		while (element != null) {
			if (element instanceof IErlModule) {
				final IErlModule module = (IErlModule) element;
				final IResource resource = module.getResource();
				if (resource instanceof IFile) {
					return new FileEditorInput((IFile) resource);
				}
			}
			element = element.getParent();
		}

		return null;
	}

	public static IEditorInput getEditorInput(final Object input)
			throws ErlModelException {
		if (input instanceof IErlElement) {
			return getEditorInput((IErlElement) input);
		}

		if (input instanceof IFile) {
			return new FileEditorInput((IFile) input);
		}

		// if (input instanceof IStorage)
		// return new JarEntryEditorInput((IStorage)input);

		return null;
	}

	/**
	 * If the current active editor edits a erlang element return it, else
	 * return null
	 */
	public static IErlElement getActiveEditorErlangInput() {
		final IWorkbenchPage page = ErlideUIPlugin.getActivePage();
		if (page != null) {
			final IEditorPart part = page.getActiveEditor();
			if (part != null) {
				final IEditorInput editorInput = part.getEditorInput();
				if (editorInput != null) {
					return (IErlElement) editorInput
							.getAdapter(IErlElement.class);
					// return JavaUI.getEditorInputJavaElement(editorInput);
				}
			}
		}
		return null;
	}

	/**
	 * Maps the localized modifier name to a code in the same manner as
	 * #findModifier.
	 * 
	 * @param modifierName
	 *            the modifier name
	 * @return the SWT modifier bit, or <code>0</code> if no match was found
	 * @since 2.1.1
	 */
	public static int findLocalizedModifier(final String modifierName) {
		if (modifierName == null) {
			return 0;
		}

		if (modifierName.equalsIgnoreCase(Action.findModifierString(SWT.CTRL))) {
			return SWT.CTRL;
		}
		if (modifierName.equalsIgnoreCase(Action.findModifierString(SWT.SHIFT))) {
			return SWT.SHIFT;
		}
		if (modifierName.equalsIgnoreCase(Action.findModifierString(SWT.ALT))) {
			return SWT.ALT;
		}
		if (modifierName.equalsIgnoreCase(Action
				.findModifierString(SWT.COMMAND))) {
			return SWT.COMMAND;
		}

		return 0;
	}

	/**
	 * Returns the modifier string for the given SWT modifier modifier bits.
	 * 
	 * @param stateMask
	 *            the SWT modifier bits
	 * @return the modifier string
	 * @since 2.1.1
	 */
	// public static String getModifierString(int stateMask) {
	// String modifierString= ""; //$NON-NLS-1$
	// if ((stateMask & SWT.CTRL) == SWT.CTRL)
	// modifierString= appendModifierString(modifierString, SWT.CTRL);
	// if ((stateMask & SWT.ALT) == SWT.ALT)
	// modifierString= appendModifierString(modifierString, SWT.ALT);
	// if ((stateMask & SWT.SHIFT) == SWT.SHIFT)
	// modifierString= appendModifierString(modifierString, SWT.SHIFT);
	// if ((stateMask & SWT.COMMAND) == SWT.COMMAND)
	// modifierString= appendModifierString(modifierString, SWT.COMMAND);
	//
	// return modifierString;
	// }
	/**
	 * Appends to modifier string of the given SWT modifier bit to the given
	 * modifierString.
	 * 
	 * @param modifierString
	 *            the modifier string
	 * @param modifier
	 *            an int with SWT modifier bit
	 * @return the concatenated modifier string
	 * @since 2.1.1
	 */
	// private static String appendModifierString(String modifierString, int
	// modifier) {
	// if (modifierString == null)
	// modifierString= ""; //$NON-NLS-1$
	// String newModifierString= Action.findModifierString(modifier);
	// if (modifierString.length() == 0)
	// return newModifierString;
	// return
	// Messages.format(JavaEditorMessages.EditorUtility_concatModifierStrings,
	// new
	// String[] {modifierString, newModifierString});
	// }
	/**
	 * Returns the Java project for a given editor input or <code>null</code>
	 * if no corresponding Java project exists.
	 * 
	 * @param input
	 *            the editor input
	 * @return the corresponding Java project
	 * 
	 * @since 3.0
	 */
	// public static IJavaProject getJavaProject(IEditorInput input) {
	// IJavaProject jProject= null;
	// if (input instanceof IFileEditorInput) {
	// IProject project= ((IFileEditorInput)input).getFile().getProject();
	// if (project != null) {
	// jProject= JavaCore.create(project);
	// if (!jProject.exists())
	// jProject= null;
	// }
	// } else if (input instanceof IClassFileEditorInput) {
	// jProject= ((IClassFileEditorInput)input).getClassFile().getJavaProject();
	// }
	// return jProject;
	// }
	/**
	 * Returns an array of all editors that have an unsaved content. If the
	 * identical content is presented in more than one editor, only one of those
	 * editor parts is part of the result.
	 * 
	 * @return an array of all dirty editor parts.
	 */
	public static IEditorPart[] getDirtyEditors() {
		final Set<IEditorInput> inputs = new HashSet<IEditorInput>();
		final List<IEditorPart> result = new ArrayList<IEditorPart>(0);
		final IWorkbench workbench = PlatformUI.getWorkbench();
		final IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();
		for (final IWorkbenchWindow element : windows) {
			final IWorkbenchPage[] pages = element.getPages();
			for (final IWorkbenchPage element0 : pages) {
				final IEditorPart[] editors = element0.getDirtyEditors();
				for (final IEditorPart ep : editors) {
					final IEditorInput input = ep.getEditorInput();
					if (!inputs.contains(input)) {
						inputs.add(input);
						result.add(ep);
					}
				}
			}
		}
		return result.toArray(new IEditorPart[result.size()]);
	}

	static public IFile openExternal(final String path) throws CoreException {
		final IWorkspace ws = ResourcesPlugin.getWorkspace();
		// TODO is this a good way?
		final String prjName = "External Files";
		final IProject project = ws.getRoot().getProject(prjName);
		if (!project.exists()) {
			project.create(null);
			project.open(null);
			final IProjectDescription description = project.getDescription();
			description.setNatureIds(new String[] { ErlangPlugin.NATURE_ID });
			project.setDescription(description, null);
		}
		if (!project.isOpen()) {
			project.open(null);
		}
		if (path == null) {
			return null;
		}
		final IPath location = new Path(path);
		final IFile file = project.getFile(location.lastSegment());
		if (!file.isLinked()) {
			file.createLink(location, IResource.NONE, null);
		}
		final IErlProject p = ErlangCore.getModel().getErlangProject(prjName);
		p.open(null);
		return file;
	}

}
