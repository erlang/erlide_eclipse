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
package org.erlide.gunit.internal.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.IStatus;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ViewerFilter;

import org.eclipse.ui.dialogs.ISelectionStatusValidator;
import org.eclipse.ui.dialogs.SelectionStatusDialog;

/**
 * A dialog with checked table viewer.
 */
public class CheckedTableSelectionDialog extends SelectionStatusDialog {

	private CheckboxTableViewer fViewer;

	private ILabelProvider fLabelProvider;
	private IStructuredContentProvider fContentProvider;

	private ISelectionStatusValidator fValidator = null;
	private String fEmptyListMessage = WizardMessages.CheckedTableSelectionDialog_emptyListMessage;

	private IStatus fCurrStatus = new JUnitStatus();
	private List fFilters;
	private Object fInput;
	private boolean fIsEmpty;

	private int fWidth = 40;
	private int fHeight = 18;

	/**
	 * Constructs an instance of <code>ElementTreeSelectionDialog</code>.
	 * 
	 * @param labelProvider
	 *            the label provider to render the entries
	 * @param contentProvider
	 *            the content provider to evaluate the tree structure
	 */
	public CheckedTableSelectionDialog(Shell parent,
			ILabelProvider labelProvider,
			IStructuredContentProvider contentProvider) {
		super(parent);

		fLabelProvider = labelProvider;
		fContentProvider = contentProvider;

		setResult(new ArrayList(0));
		setStatusLineAboveButtons(true);
	}

	/**
	 * Sets the initial selection. Convenience method.
	 * 
	 * @param selection
	 *            the initial selection.
	 */
	public void setInitialSelection(Object selection) {
		setInitialSelections(new Object[] { selection });
	}

	/**
	 * Sets the message to be displayed if the list is empty.
	 * 
	 * @param message
	 *            the message to be displayed.
	 */
	public void setEmptyListMessage(String message) {
		fEmptyListMessage = message;
	}

	/**
	 * Adds a filter to the tree viewer.
	 * 
	 * @param filter
	 *            a filter.
	 */
	public void addFilter(ViewerFilter filter) {
		if (fFilters == null)
			fFilters = new ArrayList(4);

		fFilters.add(filter);
	}

	/**
	 * Sets an optional validator to check if the selection is valid. The
	 * validator is invoked whenever the selection changes.
	 * 
	 * @param validator
	 *            the validator to validate the selection.
	 */
	public void setValidator(ISelectionStatusValidator validator) {
		fValidator = validator;
	}

	/**
	 * Sets the tree input.
	 * 
	 * @param input
	 *            the tree input.
	 */
	public void setInput(Object input) {
		fInput = input;
	}

	/**
	 * Sets the size of the tree in unit of characters.
	 * 
	 * @param width
	 *            the width of the tree.
	 * @param height
	 *            the height of the tree.
	 */
	public void setSize(int width, int height) {
		fWidth = width;
		fHeight = height;
	}

	protected void updateOKStatus() {
		if (!fIsEmpty) {
			if (fValidator != null) {
				fCurrStatus = fValidator.validate(fViewer.getCheckedElements());
				updateStatus(fCurrStatus);
			} else if (!fCurrStatus.isOK()) {
				fCurrStatus = new JUnitStatus();
			}
		} else {
			fCurrStatus = new JUnitStatus(IStatus.ERROR, fEmptyListMessage);
		}
		updateStatus(fCurrStatus);
	}

	/*
	 * @see Window#open()
	 */
	@Override
	public int open() {
		fIsEmpty = evaluateIfTableEmpty(fInput);
		BusyIndicator.showWhile(null, new Runnable() {
			public void run() {
				access$superOpen();
			}
		});
		return getReturnCode();
	}

	private void access$superOpen() {
		super.open();
	}

	/**
	 * Handles cancel button pressed event.
	 */
	@Override
	protected void cancelPressed() {
		setResult(null);
		super.cancelPressed();
	}

	/*
	 * @see SelectionStatusDialog#computeResult()
	 */
	@Override
	protected void computeResult() {
		setResult(Arrays.asList(fViewer.getCheckedElements()));
	}

	/*
	 * @see Window#create()
	 */
	@Override
	public void create() {
		super.create();

		List initialSelections = getInitialElementSelections();
		if (initialSelections.size() > 0) {
			fViewer.setCheckedElements(initialSelections.toArray());
		}

		updateOKStatus();
	}

	/*
	 * @see Dialog#createDialogArea(Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);

		Label messageLabel = createMessageArea(composite);
		Control treeWidget = createTableViewer(composite);
		Control buttonComposite = createSelectionButtons(composite);

		GridData data = new GridData(GridData.FILL_BOTH);
		data.widthHint = convertWidthInCharsToPixels(fWidth);
		data.heightHint = convertHeightInCharsToPixels(fHeight);
		treeWidget.setLayoutData(data);

		if (fIsEmpty) {
			messageLabel.setEnabled(false);
			treeWidget.setEnabled(false);
			buttonComposite.setEnabled(false);
		}
		applyDialogFont(composite);
		return composite;
	}

	private Table createTableViewer(Composite parent) {
		fViewer = CheckboxTableViewer.newCheckList(parent, SWT.BORDER);

		fViewer.setContentProvider(fContentProvider);
		fViewer.setLabelProvider(fLabelProvider);
		fViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(CheckStateChangedEvent event) {
				updateOKStatus();
			}
		});

		if (fFilters != null) {
			for (int i = 0; i != fFilters.size(); i++)
				fViewer.addFilter((ViewerFilter) fFilters.get(i));
		}

		fViewer.setInput(fInput);
		return fViewer.getTable();
	}

	/**
	 * Add the selection and deselection buttons to the dialog.
	 * 
	 * @param composite
	 *            org.eclipse.swt.widgets.Composite
	 */
	private Composite createSelectionButtons(Composite composite) {
		Composite buttonComposite = new Composite(composite, SWT.RIGHT);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		buttonComposite.setLayout(layout);
		GridData data = new GridData(GridData.HORIZONTAL_ALIGN_END
				| GridData.GRAB_HORIZONTAL);
		data.grabExcessHorizontalSpace = true;
		composite.setData(data);

		Button selectButton = createButton(buttonComposite,
				IDialogConstants.SELECT_ALL_ID,
				WizardMessages.CheckedTableSelectionDialog_selectAll, false);

		SelectionListener listener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				fViewer
						.setCheckedElements(fContentProvider
								.getElements(fInput));
				updateOKStatus();
			}
		};
		selectButton.addSelectionListener(listener);

		Button deselectButton = createButton(buttonComposite,
				IDialogConstants.DESELECT_ALL_ID,
				WizardMessages.CheckedTableSelectionDialog_deselectAll, false);

		listener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				fViewer.setCheckedElements(new Object[0]);
				updateOKStatus();
			}
		};
		deselectButton.addSelectionListener(listener);
		return buttonComposite;
	}

	private boolean evaluateIfTableEmpty(Object input) {
		Object[] elements = fContentProvider.getElements(input);
		if (elements.length > 0) {
			if (fFilters != null) {
				for (int i = 0; i < fFilters.size(); i++) {
					ViewerFilter curr = (ViewerFilter) fFilters.get(i);
					elements = curr.filter(fViewer, input, elements);
				}
			}
		}
		return elements.length == 0;
	}
}
