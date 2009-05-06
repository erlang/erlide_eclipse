/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Sebastian Davids: sdavids@gmx.de bug 37333, 26653
 *     Johan Walles: walles@mailblocks.com bug 68737
 *******************************************************************************/
package org.erlide.gunit.internal.ui;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.util.IOpenEventListener;
import org.eclipse.jface.util.OpenStrategy;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.ToolBar;
import org.erlide.gunit.internal.model.TestElement;

/**
 * A pane that shows a stack trace of a failed test.
 */
public class FailureTrace implements IMenuListener {
	private static final int MAX_LABEL_LENGTH = 256;

	static final String FRAME_PREFIX = "at "; //$NON-NLS-1$

	private final Table fTable;

	private final TestRunnerViewPart fTestRunner;

	private String fInputTrace;

	private final Clipboard fClipboard;

	private TestElement fFailure;

	private final CompareResultsAction fCompareAction;

	private final FailureTableDisplay fFailureTableDisplay;

	public FailureTrace(final Composite parent, final Clipboard clipboard,
			final TestRunnerViewPart testRunner, final ToolBar toolBar) {
		Assert.isNotNull(clipboard);

		// fill the failure trace viewer toolbar
		final ToolBarManager failureToolBarmanager = new ToolBarManager(toolBar);
		failureToolBarmanager.add(new EnableStackFilterAction(this));
		this.fCompareAction = new CompareResultsAction(this);
		this.fCompareAction.setEnabled(false);
		failureToolBarmanager.add(this.fCompareAction);
		failureToolBarmanager.update(true);

		this.fTable = new Table(parent, SWT.SINGLE | SWT.V_SCROLL
				| SWT.H_SCROLL);
		this.fTestRunner = testRunner;
		this.fClipboard = clipboard;

		final OpenStrategy handler = new OpenStrategy(this.fTable);
		handler.addOpenListener(new IOpenEventListener() {
			public void handleOpen(final SelectionEvent e) {
				if (FailureTrace.this.fTable.getSelectionIndex() == 0
						&& FailureTrace.this.fFailure.isComparisonFailure()) {
					FailureTrace.this.fCompareAction.run();
				}
				if (FailureTrace.this.fTable.getSelection().length != 0) {
					final Action a = createOpenEditorAction(getSelectedText());
					if (a != null) {
						a.run();
					}
				}
			}
		});

		initMenu();

		this.fFailureTableDisplay = new FailureTableDisplay(this.fTable);
	}

	private void initMenu() {
		final MenuManager menuMgr = new MenuManager();
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(this);
		final Menu menu = menuMgr.createContextMenu(this.fTable);
		this.fTable.setMenu(menu);
	}

	public void menuAboutToShow(final IMenuManager manager) {
		if (this.fTable.getSelectionCount() > 0) {
			final Action a = createOpenEditorAction(getSelectedText());
			if (a != null) {
				manager.add(a);
			}
			manager
			.add(new GUnitCopyAction(FailureTrace.this, this.fClipboard));
		}
		// fix for bug 68058
		if (this.fFailure != null && this.fFailure.isComparisonFailure()) {
			manager.add(this.fCompareAction);
		}
	}

	public String getTrace() {
		return this.fInputTrace;
	}

	private String getSelectedText() {
		return this.fTable.getSelection()[0].getText();
	}

	private Action createOpenEditorAction(final String traceLine) {
		try {
			String testName = traceLine;
			testName = testName.substring(testName.indexOf(FRAME_PREFIX));
			testName = testName.substring(FRAME_PREFIX.length(),
					testName.lastIndexOf('(')).trim();
			testName = testName.substring(0, testName.lastIndexOf('.'));
			final int innerSeparatorIndex = testName.indexOf('$');
			if (innerSeparatorIndex != -1) {
				testName = testName.substring(0, innerSeparatorIndex);
			}

			String lineNumber = traceLine;
			lineNumber = lineNumber.substring(lineNumber.indexOf(':') + 1,
					lineNumber.lastIndexOf(')'));
			final int line = Integer.valueOf(lineNumber).intValue();
			// fix for bug 37333
			final String cuName = traceLine.substring(traceLine.lastIndexOf('(') + 1,
					traceLine.lastIndexOf(':'));
			return new OpenEditorAtLineAction(this.fTestRunner, cuName,
					testName, line);
		} catch (final NumberFormatException e) {
		} catch (final IndexOutOfBoundsException e) {
		}
		return null;
	}

	/**
	 * Returns the composite used to present the trace
	 * 
	 * @return The composite
	 */
	Composite getComposite() {
		return this.fTable;
	}

	/**
	 * Refresh the table from the trace.
	 */
	public void refresh() {
		updateTable(this.fInputTrace);
	}

	/**
	 * Shows a TestFailure
	 * 
	 * @param test
	 *            the failed test
	 */
	public void showFailure(final TestElement test) {
		this.fFailure = test;
		String trace = ""; //$NON-NLS-1$
		updateEnablement(test);
		if (test != null) {
			trace = test.getTrace();
		}
		if (this.fInputTrace == trace) {
			return;
		}
		this.fInputTrace = trace;
		updateTable(trace);
	}

	public void updateEnablement(final TestElement test) {
		final boolean enableCompare = test != null && test.isComparisonFailure();
		this.fCompareAction.setEnabled(enableCompare);
		if (enableCompare) {
			this.fCompareAction.updateOpenDialog(test);
		}
	}

	private void updateTable(String trace) {
		if (trace == null || trace.trim().equals("")) { //$NON-NLS-1$
			clear();
			return;
		}
		trace = trace.trim();
		this.fTable.setRedraw(false);
		this.fTable.removeAll();
		new TextualTrace(trace, getFilterPatterns()).display(
				this.fFailureTableDisplay, MAX_LABEL_LENGTH);
		this.fTable.setRedraw(true);
	}

	private String[] getFilterPatterns() {
		if (GUnitPreferencePage.getFilterStack()) {
			return GUnitPreferencePage.getFilterPatterns();
		}
		return new String[0];
	}

	/**
	 * Shows other information than a stack trace.
	 * 
	 * @param text
	 *            the informational message to be shown
	 */
	public void setInformation(final String text) {
		clear();
		final TableItem tableItem = this.fFailureTableDisplay.newTableItem();
		tableItem.setText(text);
	}

	/**
	 * Clears the non-stack trace info
	 */
	public void clear() {
		this.fTable.removeAll();
		this.fInputTrace = null;
	}

	public TestElement getFailedTest() {
		return this.fFailure;
	}

	public Shell getShell() {
		return this.fTable.getShell();
	}

	public FailureTableDisplay getFailureTableDisplay() {
		return this.fFailureTableDisplay;
	}
}
