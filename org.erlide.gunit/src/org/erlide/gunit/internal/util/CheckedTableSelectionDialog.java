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
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ViewerFilter;
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
import org.eclipse.ui.dialogs.ISelectionStatusValidator;
import org.eclipse.ui.dialogs.SelectionStatusDialog;

/**
 * A dialog with checked table viewer.
 */
public class CheckedTableSelectionDialog extends SelectionStatusDialog {

	private CheckboxTableViewer fViewer;

	private final ILabelProvider fLabelProvider;

	private final IStructuredContentProvider fContentProvider;

	private ISelectionStatusValidator fValidator = null;

	private String fEmptyListMessage = "empty list";

	private IStatus fCurrStatus = new GUnitStatus();

	private List<ViewerFilter> fFilters;

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
	public CheckedTableSelectionDialog(final Shell parent,
			final ILabelProvider labelProvider,
			final IStructuredContentProvider contentProvider) {
		super(parent);

		this.fLabelProvider = labelProvider;
		this.fContentProvider = contentProvider;

		setResult(new ArrayList<Object>(0));
		setStatusLineAboveButtons(true);
	}

	/**
	 * Sets the initial selection. Convenience method.
	 * 
	 * @param selection
	 *            the initial selection.
	 */
	public void setInitialSelection(final Object selection) {
		setInitialSelections(new Object[] { selection });
	}

	/**
	 * Sets the message to be displayed if the list is empty.
	 * 
	 * @param message
	 *            the message to be displayed.
	 */
	public void setEmptyListMessage(final String message) {
		this.fEmptyListMessage = message;
	}

	/**
	 * Adds a filter to the tree viewer.
	 * 
	 * @param filter
	 *            a filter.
	 */
	public void addFilter(final ViewerFilter filter) {
		if (this.fFilters == null) {
			this.fFilters = new ArrayList<ViewerFilter>(4);
		}

		this.fFilters.add(filter);
	}

	/**
	 * Sets an optional validator to check if the selection is valid. The
	 * validator is invoked whenever the selection changes.
	 * 
	 * @param validator
	 *            the validator to validate the selection.
	 */
	public void setValidator(final ISelectionStatusValidator validator) {
		this.fValidator = validator;
	}

	/**
	 * Sets the tree input.
	 * 
	 * @param input
	 *            the tree input.
	 */
	public void setInput(final Object input) {
		this.fInput = input;
	}

	/**
	 * Sets the size of the tree in unit of characters.
	 * 
	 * @param width
	 *            the width of the tree.
	 * @param height
	 *            the height of the tree.
	 */
	public void setSize(final int width, final int height) {
		this.fWidth = width;
		this.fHeight = height;
	}

	protected void updateOKStatus() {
		if (!this.fIsEmpty) {
			if (this.fValidator != null) {
				this.fCurrStatus = this.fValidator.validate(this.fViewer
						.getCheckedElements());
				updateStatus(this.fCurrStatus);
			} else if (!this.fCurrStatus.isOK()) {
				this.fCurrStatus = new GUnitStatus();
			}
		} else {
			this.fCurrStatus = new GUnitStatus(IStatus.ERROR,
					this.fEmptyListMessage);
		}
		updateStatus(this.fCurrStatus);
	}

	/*
	 * @see Window#open()
	 */
	@Override
	public int open() {
		this.fIsEmpty = evaluateIfTableEmpty(this.fInput);
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
		setResult(Arrays.asList(this.fViewer.getCheckedElements()));
	}

	/*
	 * @see Window#create()
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void create() {
		super.create();

		final List initialSelections = getInitialElementSelections();
		if (initialSelections.size() > 0) {
			this.fViewer.setCheckedElements(initialSelections.toArray());
		}

		updateOKStatus();
	}

	/*
	 * @see Dialog#createDialogArea(Composite)
	 */
	@Override
	protected Control createDialogArea(final Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);

		final Label messageLabel = createMessageArea(composite);
		final Control treeWidget = createTableViewer(composite);
		final Control buttonComposite = createSelectionButtons(composite);

		final GridData data = new GridData(GridData.FILL_BOTH);
		data.widthHint = convertWidthInCharsToPixels(this.fWidth);
		data.heightHint = convertHeightInCharsToPixels(this.fHeight);
		treeWidget.setLayoutData(data);

		if (this.fIsEmpty) {
			messageLabel.setEnabled(false);
			treeWidget.setEnabled(false);
			buttonComposite.setEnabled(false);
		}
		applyDialogFont(composite);
		return composite;
	}

	private Table createTableViewer(final Composite parent) {
		this.fViewer = CheckboxTableViewer.newCheckList(parent, SWT.BORDER);

		this.fViewer.setContentProvider(this.fContentProvider);
		this.fViewer.setLabelProvider(this.fLabelProvider);
		this.fViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(final CheckStateChangedEvent event) {
				updateOKStatus();
			}
		});

		if (this.fFilters != null) {
			for (int i = 0; i != this.fFilters.size(); i++) {
				this.fViewer.addFilter(this.fFilters.get(i));
			}
		}

		this.fViewer.setInput(this.fInput);
		return this.fViewer.getTable();
	}

	/**
	 * Add the selection and deselection buttons to the dialog.
	 * 
	 * @param composite
	 *            org.eclipse.swt.widgets.Composite
	 */
	private Composite createSelectionButtons(final Composite composite) {
		final Composite buttonComposite = new Composite(composite, SWT.RIGHT);
		final GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		buttonComposite.setLayout(layout);
		final GridData data = new GridData(GridData.HORIZONTAL_ALIGN_END
				| GridData.GRAB_HORIZONTAL);
		data.grabExcessHorizontalSpace = true;
		composite.setData(data);

		final Button selectButton = createButton(buttonComposite,
				IDialogConstants.SELECT_ALL_ID, "select all", false);

		SelectionListener listener = new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				CheckedTableSelectionDialog.this.fViewer
				.setCheckedElements(CheckedTableSelectionDialog.this.fContentProvider
						.getElements(CheckedTableSelectionDialog.this.fInput));
				updateOKStatus();
			}
		};
		selectButton.addSelectionListener(listener);

		final Button deselectButton = createButton(buttonComposite,
				IDialogConstants.DESELECT_ALL_ID, "deselect all", false);

		listener = new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				CheckedTableSelectionDialog.this.fViewer
				.setCheckedElements(new Object[0]);
				updateOKStatus();
			}
		};
		deselectButton.addSelectionListener(listener);
		return buttonComposite;
	}

	private boolean evaluateIfTableEmpty(final Object input) {
		Object[] elements = this.fContentProvider.getElements(input);
		if (elements.length > 0) {
			if (this.fFilters != null) {
				for (int i = 0; i < this.fFilters.size(); i++) {
					final ViewerFilter curr = this.fFilters.get(i);
					elements = curr.filter(this.fViewer, input, elements);
				}
			}
		}
		return elements.length == 0;
	}
}
