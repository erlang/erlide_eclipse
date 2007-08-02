/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.properties;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.INewWizard;
import org.erlide.basicui.dialogfields.DialogField;
import org.erlide.basicui.dialogfields.ITreeListAdapter;
import org.erlide.basicui.dialogfields.LayoutUtil;
import org.erlide.basicui.dialogfields.ListDialogField;
import org.erlide.basicui.dialogfields.SelectionButtonDialogField;
import org.erlide.basicui.dialogfields.StringDialogField;
import org.erlide.basicui.dialogfields.TreeListDialogField;
import org.erlide.basicui.prefs.IDialogFieldListener;
import org.erlide.basicui.util.PixelConverter;

public class SourceContainerWorkbookPage extends BuildPathBasePage {

	private class OpenBuildPathWizardAction extends AbstractOpenWizardAction
			implements IPropertyChangeListener {

		private final BuildPathWizard fWizard;
		private final List fSelectedElements;

		public OpenBuildPathWizardAction(BuildPathWizard wizard) {
			fWizard = wizard;
			addPropertyChangeListener(this);
			fSelectedElements = fFoldersList.getSelectedElements();
		}

		/**
		 * {@inheritDoc}
		 */
		protected INewWizard createWizard() throws CoreException {
			return fWizard;
		}

		/**
		 * {@inheritDoc}
		 */
		public void propertyChange(PropertyChangeEvent event) {
			if (event.getProperty().equals(IAction.RESULT)) {
				if (event.getNewValue().equals(Boolean.TRUE)) {
					finishWizard();
				} else {
					fWizard.cancel();
				}
			}
		}

		protected void finishWizard() {
			List insertedElements = fWizard.getInsertedElements();
			refresh(insertedElements, fWizard.getRemovedElements(), fWizard
					.getModifiedElements(), fWizard.getOutputLocation());

			if (insertedElements.isEmpty()) {
				fFoldersList.postSetSelection(new StructuredSelection(
						fSelectedElements));
			}
		}

	}

	private static AddSourceFolderWizard newSourceFolderWizard(
			CPListElement element, List/* <CPListElement> */existingElements,
			String outputLocation, boolean newFolder) {
		CPListElement[] existing = (CPListElement[]) existingElements
				.toArray(new CPListElement[existingElements.size()]);
		AddSourceFolderWizard wizard = new AddSourceFolderWizard(existing,
				element, new Path(outputLocation).makeAbsolute(), false,
				newFolder, newFolder, newFolder ? CPListElement
						.isProjectSourceFolder(existing, element
								.getJavaProject()) : false, newFolder);
		wizard.setDoFlushChange(false);
		return wizard;
	}

	private static AddSourceFolderWizard newLinkedSourceFolderWizard(
			CPListElement element, List/* <CPListElement> */existingElements,
			String outputLocation, boolean newFolder) {
		CPListElement[] existing = (CPListElement[]) existingElements
				.toArray(new CPListElement[existingElements.size()]);
		AddSourceFolderWizard wizard = new AddSourceFolderWizard(existing,
				element, new Path(outputLocation).makeAbsolute(), true,
				newFolder, newFolder, newFolder ? CPListElement
						.isProjectSourceFolder(existing, element
								.getJavaProject()) : false, newFolder);
		wizard.setDoFlushChange(false);
		return wizard;
	}

	private static EditFilterWizard newEditFilterWizard(CPListElement element,
			List/* <CPListElement> */existingElements, String outputLocation) {
		CPListElement[] existing = (CPListElement[]) existingElements
				.toArray(new CPListElement[existingElements.size()]);
		EditFilterWizard result = new EditFilterWizard(existing, element,
				new Path(outputLocation).makeAbsolute());
		result.setDoFlushChange(false);
		return result;
	}

	private ListDialogField fClassPathList;
	private IJavaProject fCurrJProject;

	private Control fSWTControl;
	private TreeListDialogField fFoldersList;

	private StringDialogField fOutputLocationField;

	private SelectionButtonDialogField fUseFolderOutputs;

	private final int IDX_ADD = 0;
	private final int IDX_ADD_LINK = 1;
	private final int IDX_EDIT = 3;
	private final int IDX_REMOVE = 4;

	public SourceContainerWorkbookPage(ListDialogField classPathList,
			StringDialogField outputLocationField) {
		fClassPathList = classPathList;

		fOutputLocationField = outputLocationField;

		fSWTControl = null;

		SourceContainerAdapter adapter = new SourceContainerAdapter();

		String[] buttonLabels;

		buttonLabels = new String[] {
				NewWizardMessages.SourceContainerWorkbookPage_folders_add_button,
				NewWizardMessages.SourceContainerWorkbookPage_folders_link_source_button,
				/* 1 */null,
				NewWizardMessages.SourceContainerWorkbookPage_folders_edit_button,
				NewWizardMessages.SourceContainerWorkbookPage_folders_remove_button };

		fFoldersList = new TreeListDialogField(adapter, buttonLabels,
				new CPListLabelProvider());
		fFoldersList.setDialogFieldListener(adapter);
		fFoldersList
				.setLabelText(NewWizardMessages.SourceContainerWorkbookPage_folders_label);

		fFoldersList.setViewerSorter(new CPListElementSorter());
		fFoldersList.enableButton(IDX_EDIT, false);

		fUseFolderOutputs = new SelectionButtonDialogField(SWT.CHECK);
		fUseFolderOutputs.setSelection(false);
		fUseFolderOutputs
				.setLabelText(NewWizardMessages.SourceContainerWorkbookPage_folders_check);
		fUseFolderOutputs.setDialogFieldListener(adapter);
	}

	public void init(IJavaProject jproject) {
		fCurrJProject = jproject;
		updateFoldersList();
	}

	private void updateFoldersList() {
		ArrayList folders = new ArrayList();

		boolean useFolderOutputs = false;
		List cpelements = fClassPathList.getElements();
		for (int i = 0; i < cpelements.size(); i++) {
			CPListElement cpe = (CPListElement) cpelements.get(i);
			if (cpe.getEntryKind() == IClasspathEntry.CPE_SOURCE) {
				folders.add(cpe);
				boolean hasOutputFolder = (cpe
						.getAttribute(CPListElement.OUTPUT) != null);
				if (hasOutputFolder) {
					useFolderOutputs = true;
				}

			}
		}
		fFoldersList.setElements(folders);
		fUseFolderOutputs.setSelection(useFolderOutputs);

		for (int i = 0; i < folders.size(); i++) {
			CPListElement cpe = (CPListElement) folders.get(i);
			IPath[] ePatterns = (IPath[]) cpe
					.getAttribute(CPListElement.EXCLUSION);
			IPath[] iPatterns = (IPath[]) cpe
					.getAttribute(CPListElement.INCLUSION);
			boolean hasOutputFolder = (cpe.getAttribute(CPListElement.OUTPUT) != null);
			if (ePatterns.length > 0 || iPatterns.length > 0 || hasOutputFolder) {
				fFoldersList.expandElement(cpe, 3);
			}
		}
	}

	public Control getControl(Composite parent) {
		PixelConverter converter = new PixelConverter(parent);
		Composite composite = new Composite(parent, SWT.NONE);

		LayoutUtil.doDefaultLayout(composite, new DialogField[] { fFoldersList,
				fUseFolderOutputs, fOutputLocationField }, true, SWT.DEFAULT,
				SWT.DEFAULT);
		LayoutUtil.setHorizontalGrabbing(fFoldersList.getTreeControl(null));

		int buttonBarWidth = converter.convertWidthInCharsToPixels(24);
		fFoldersList.setButtonsMinWidth(buttonBarWidth);

		fSWTControl = composite;

		// expand
		List elements = fFoldersList.getElements();
		for (int i = 0; i < elements.size(); i++) {
			CPListElement elem = (CPListElement) elements.get(i);
			IPath[] exclusionPatterns = (IPath[]) elem
					.getAttribute(CPListElement.EXCLUSION);
			IPath[] inclusionPatterns = (IPath[]) elem
					.getAttribute(CPListElement.INCLUSION);
			IPath output = (IPath) elem.getAttribute(CPListElement.OUTPUT);
			if (exclusionPatterns.length > 0 || inclusionPatterns.length > 0
					|| output != null) {
				fFoldersList.expandElement(elem, 3);
			}
		}
		return composite;
	}

	private Shell getShell() {
		if (fSWTControl != null) {
			return fSWTControl.getShell();
		}
		return JavaPlugin.getActiveWorkbenchShell();
	}

	private class SourceContainerAdapter implements ITreeListAdapter,
			IDialogFieldListener {

		private final Object[] EMPTY_ARR = new Object[0];

		// -------- IListAdapter --------
		public void customButtonPressed(TreeListDialogField field, int index) {
			sourcePageCustomButtonPressed(field, index);
		}

		public void selectionChanged(TreeListDialogField field) {
			sourcePageSelectionChanged(field);
		}

		public void doubleClicked(TreeListDialogField field) {
			sourcePageDoubleClicked(field);
		}

		public void keyPressed(TreeListDialogField field, KeyEvent event) {
			sourcePageKeyPressed(field, event);
		}

		public Object[] getChildren(TreeListDialogField field, Object element) {
			if (element instanceof CPListElement) {
				return ((CPListElement) element).getChildren(!fUseFolderOutputs
						.isSelected());
			}
			return EMPTY_ARR;
		}

		public Object getParent(TreeListDialogField field, Object element) {
			if (element instanceof CPListElementAttribute) {
				return ((CPListElementAttribute) element).getParent();
			}
			return null;
		}

		public boolean hasChildren(TreeListDialogField field, Object element) {
			return (element instanceof CPListElement);
		}

		// ---------- IDialogFieldListener --------
		public void dialogFieldChanged(DialogField field) {
			sourcePageDialogFieldChanged(field);
		}

	}

	protected void sourcePageKeyPressed(TreeListDialogField field,
			KeyEvent event) {
		if (field == fFoldersList) {
			if (event.character == SWT.DEL && event.stateMask == 0) {
				List selection = field.getSelectedElements();
				if (canRemove(selection)) {
					removeEntry();
				}
			}
		}
	}

	protected void sourcePageDoubleClicked(TreeListDialogField field) {
		if (field == fFoldersList) {
			List selection = field.getSelectedElements();
			if (canEdit(selection)) {
				editEntry();
			}
		}
	}

	protected void sourcePageCustomButtonPressed(DialogField field, int index) {
		if (field == fFoldersList) {
			if (index == IDX_ADD) {
				IProject project = fCurrJProject.getProject();
				if (project.exists() && hasFolders(project)) {
					List existingElements = fFoldersList.getElements();
					CPListElement[] existing = (CPListElement[]) existingElements
							.toArray(new CPListElement[existingElements.size()]);
					CreateMultipleSourceFoldersDialog dialog = new CreateMultipleSourceFoldersDialog(
							fCurrJProject, existing, fOutputLocationField
									.getText(), getShell());
					if (dialog.open() == Window.OK) {
						refresh(dialog.getInsertedElements(), dialog
								.getRemovedElements(), dialog
								.getModifiedElements(), dialog
								.getOutputLocation());
					}
				} else {
					CPListElement newElement = new CPListElement(fCurrJProject,
							IClasspathEntry.CPE_SOURCE);
					AddSourceFolderWizard wizard = newSourceFolderWizard(
							newElement, fFoldersList.getElements(),
							fOutputLocationField.getText(), true);
					OpenBuildPathWizardAction action = new OpenBuildPathWizardAction(
							wizard);
					action.run();
				}
			} else if (index == IDX_ADD_LINK) {
				CPListElement newElement = new CPListElement(fCurrJProject,
						IClasspathEntry.CPE_SOURCE);
				AddSourceFolderWizard wizard = newLinkedSourceFolderWizard(
						newElement, fFoldersList.getElements(),
						fOutputLocationField.getText(), true);
				OpenBuildPathWizardAction action = new OpenBuildPathWizardAction(
						wizard);
				action.run();
			} else if (index == IDX_EDIT) {
				editEntry();
			} else if (index == IDX_REMOVE) {
				removeEntry();
			}
		}
	}

	private boolean hasFolders(IContainer container) {

		try {
			IResource[] members = container.members();
			for (int i = 0; i < members.length; i++) {
				if (members[i] instanceof IContainer) {
					return true;
				}
			}
		} catch (CoreException e) {
			// ignore
		}

		List elements = fFoldersList.getElements();
		if (elements.size() > 1)
			return true;

		if (elements.size() == 0)
			return false;

		CPListElement single = (CPListElement) elements.get(0);
		if (single.getPath().equals(fCurrJProject.getPath()))
			return false;

		return true;
	}

	private void editEntry() {
		List selElements = fFoldersList.getSelectedElements();
		if (selElements.size() != 1) {
			return;
		}
		Object elem = selElements.get(0);
		if (fFoldersList.getIndexOfElement(elem) != -1) {
			editElementEntry((CPListElement) elem);
		} else if (elem instanceof CPListElementAttribute) {
			editAttributeEntry((CPListElementAttribute) elem);
		}
	}

	private void editElementEntry(CPListElement elem) {
		if (elem.getLinkTarget() != null) {
			AddSourceFolderWizard wizard = newLinkedSourceFolderWizard(elem,
					fFoldersList.getElements(), fOutputLocationField.getText(),
					false);
			OpenBuildPathWizardAction action = new OpenBuildPathWizardAction(
					wizard);
			action.run();
		} else {
			AddSourceFolderWizard wizard = newSourceFolderWizard(elem,
					fFoldersList.getElements(), fOutputLocationField.getText(),
					false);
			OpenBuildPathWizardAction action = new OpenBuildPathWizardAction(
					wizard);
			action.run();
		}
	}

	private void editAttributeEntry(CPListElementAttribute elem) {
		String key = elem.getKey();
		if (key.equals(CPListElement.OUTPUT)) {
			CPListElement selElement = elem.getParent();
			OutputLocationDialog dialog = new OutputLocationDialog(getShell(),
					selElement, fClassPathList.getElements());
			if (dialog.open() == Window.OK) {
				selElement.setAttribute(CPListElement.OUTPUT, dialog
						.getOutputLocation());
				fFoldersList.refresh();
				fClassPathList.dialogFieldChanged(); // validate
			}
		} else if (key.equals(CPListElement.EXCLUSION)
				|| key.equals(CPListElement.INCLUSION)) {
			EditFilterWizard wizard = newEditFilterWizard(elem.getParent(),
					fFoldersList.getElements(), fOutputLocationField.getText());
			OpenBuildPathWizardAction action = new OpenBuildPathWizardAction(
					wizard);
			action.run();
		} else if (key.equals(CPListElement.NATIVE_LIB_PATH)) {
			CPListElement selElement = elem.getParent();
			NativeLibrariesDialog dialog = new NativeLibrariesDialog(
					getShell(), selElement);
			if (dialog.open() == Window.OK) {
				selElement.setAttribute(CPListElement.NATIVE_LIB_PATH, dialog
						.getNativeLibraryPath());
				fFoldersList.refresh();
				fClassPathList.dialogFieldChanged(); // validate
			}
		}
	}

	protected void sourcePageSelectionChanged(DialogField field) {
		List selected = fFoldersList.getSelectedElements();
		fFoldersList.enableButton(IDX_EDIT, canEdit(selected));
		fFoldersList.enableButton(IDX_REMOVE, canRemove(selected));
		boolean noAttributes = containsOnlyTopLevelEntries(selected);
		fFoldersList.enableButton(IDX_ADD, noAttributes);
	}

	private void removeEntry() {
		List selElements = fFoldersList.getSelectedElements();
		for (int i = selElements.size() - 1; i >= 0; i--) {
			Object elem = selElements.get(i);
			if (elem instanceof CPListElementAttribute) {
				CPListElementAttribute attrib = (CPListElementAttribute) elem;
				String key = attrib.getKey();
				Object value = null;
				if (key.equals(CPListElement.EXCLUSION)
						|| key.equals(CPListElement.INCLUSION)) {
					value = new Path[0];
				}
				attrib.getParent().setAttribute(key, value);
				selElements.remove(i);
			}
		}
		if (selElements.isEmpty()) {
			fFoldersList.refresh();
			fClassPathList.dialogFieldChanged(); // validate
		} else {
			for (Iterator iter = selElements.iterator(); iter.hasNext();) {
				CPListElement element = (CPListElement) iter.next();
				if (element.getEntryKind() == IClasspathEntry.CPE_SOURCE) {
					List list = ClasspathModifier.removeFilters(element
							.getPath(), fCurrJProject, fFoldersList
							.getElements());
					for (Iterator iterator = list.iterator(); iterator
							.hasNext();) {
						CPListElement modified = (CPListElement) iterator
								.next();
						fFoldersList.refresh(modified);
						fFoldersList.expandElement(modified, 3);
					}
				}
			}
			fFoldersList.removeElements(selElements);
		}
	}

	private boolean canRemove(List selElements) {
		if (selElements.size() == 0) {
			return false;
		}
		for (int i = 0; i < selElements.size(); i++) {
			Object elem = selElements.get(i);
			if (elem instanceof CPListElementAttribute) {
				CPListElementAttribute attrib = (CPListElementAttribute) elem;
				String key = attrib.getKey();
				if (CPListElement.INCLUSION.equals(key)) {
					if (((IPath[]) attrib.getValue()).length == 0) {
						return false;
					}
				} else if (CPListElement.EXCLUSION.equals(key)) {
					if (((IPath[]) attrib.getValue()).length == 0) {
						return false;
					}
				} else if (attrib.getValue() == null) {
					return false;
				}
			} else if (elem instanceof CPListElement) {
				CPListElement curr = (CPListElement) elem;
				if (curr.getParentContainer() != null) {
					return false;
				}
			}
		}
		return true;
	}

	private boolean canEdit(List selElements) {
		if (selElements.size() != 1) {
			return false;
		}
		Object elem = selElements.get(0);
		if (elem instanceof CPListElement) {
			CPListElement cp = ((CPListElement) elem);
			if (cp.getPath().equals(cp.getJavaProject().getPath()))
				return false;

			return true;
		}
		if (elem instanceof CPListElementAttribute) {
			return true;
		}
		return false;
	}

	private void sourcePageDialogFieldChanged(DialogField field) {
		if (fCurrJProject == null) {
			// not initialized
			return;
		}

		if (field == fUseFolderOutputs) {
			if (!fUseFolderOutputs.isSelected()) {
				int nFolders = fFoldersList.getSize();
				for (int i = 0; i < nFolders; i++) {
					CPListElement cpe = (CPListElement) fFoldersList
							.getElement(i);
					cpe.setAttribute(CPListElement.OUTPUT, null);
				}
			}
			fFoldersList.refresh();
		} else if (field == fFoldersList) {
			updateClasspathList();
		}
	}

	private void updateClasspathList() {
		List srcelements = fFoldersList.getElements();

		List cpelements = fClassPathList.getElements();
		int nEntries = cpelements.size();
		// backwards, as entries will be deleted
		int lastRemovePos = nEntries;
		int afterLastSourcePos = 0;
		for (int i = nEntries - 1; i >= 0; i--) {
			CPListElement cpe = (CPListElement) cpelements.get(i);
			int kind = cpe.getEntryKind();
			if (isEntryKind(kind)) {
				if (!srcelements.remove(cpe)) {
					cpelements.remove(i);
					lastRemovePos = i;
				} else if (lastRemovePos == nEntries) {
					afterLastSourcePos = i + 1;
				}
			}
		}

		if (!srcelements.isEmpty()) {
			int insertPos = Math.min(afterLastSourcePos, lastRemovePos);
			cpelements.addAll(insertPos, srcelements);
		}

		if (lastRemovePos != nEntries || !srcelements.isEmpty()) {
			fClassPathList.setElements(cpelements);
		}
	}

	/*
	 * @see BuildPathBasePage#getSelection
	 */
	public List getSelection() {
		return fFoldersList.getSelectedElements();
	}

	/*
	 * @see BuildPathBasePage#setSelection
	 */
	public void setSelection(List selElements, boolean expand) {
		fFoldersList.selectElements(new StructuredSelection(selElements));
		if (expand) {
			for (int i = 0; i < selElements.size(); i++) {
				fFoldersList.expandElement(selElements.get(i), 1);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jdt.internal.ui.wizards.buildpaths.BuildPathBasePage#isEntryKind(int)
	 */
	public boolean isEntryKind(int kind) {
		return kind == IClasspathEntry.CPE_SOURCE;
	}

	private void refresh(List insertedElements, List removedElements,
			List modifiedElements, IPath outputLocation) {
		fFoldersList.addElements(insertedElements);
		for (Iterator iter = insertedElements.iterator(); iter.hasNext();) {
			CPListElement element = (CPListElement) iter.next();
			fFoldersList.expandElement(element, 3);
		}

		fFoldersList.removeElements(removedElements);

		for (Iterator iter = modifiedElements.iterator(); iter.hasNext();) {
			CPListElement element = (CPListElement) iter.next();
			fFoldersList.refresh(element);
			fFoldersList.expandElement(element, 3);
		}

		fFoldersList.refresh(); // does enforce the order of the entries.
		if (!insertedElements.isEmpty()) {
			fFoldersList.postSetSelection(new StructuredSelection(
					insertedElements));
		}

		fOutputLocationField
				.setText(outputLocation.makeRelative().toOSString());
	}

}
