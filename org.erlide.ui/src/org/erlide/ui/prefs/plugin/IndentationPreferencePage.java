/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs.plugin;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.basiccore.StatusInfo;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

/**
 * The editor preferences
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class IndentationPreferencePage extends ErlidePreferencePage implements
		IWorkbenchPreferencePage {

	private static final String INDENT_FIELDS[] = new String[] {
			ErlEditorMessages.IndentationPrefs_0,
			ErlEditorMessages.IndentationPrefs_3,
			ErlEditorMessages.IndentationPrefs_6,
			ErlEditorMessages.IndentationPrefs_9,
			ErlEditorMessages.IndentationPrefs_12,
			ErlEditorMessages.IndentationPrefs_15,
			ErlEditorMessages.IndentationPrefs_18,
			ErlEditorMessages.IndentationPrefs_21,
			ErlEditorMessages.IndentationPrefs_24,
			ErlEditorMessages.IndentationPrefs_27,
			ErlEditorMessages.IndentationPrefs_30,
			ErlEditorMessages.IndentationPrefs_33,
			ErlEditorMessages.IndentationPrefs_50,
			ErlEditorMessages.IndentationPrefs_51,
			ErlEditorMessages.IndentationPrefs_52,
			ErlEditorMessages.IndentationPrefs_53 };

	private static final String INDENT_KEYS[] = new String[] {
			"before_binary_op", "after_binary_op", "before_arrow", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"after_arrow", "after_unary_op", "clause", "case", "try", "catch", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
			"function_parameters", "fun", "fun_body", "comma_nl", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"semicolon_nl", "dot_nl", "arrow_nl" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

	private static final String INDENT_DEFAULTS[] = new String[] { "4", "4",
			"2", "4", "4", "4", "4", "4", "4", "2", "3", "5", "0", "0", "0",
			"0" };

	private static final int N_NUMERIC_KEYS = INDENT_KEYS.length - 4;

	public IndentationPreferencePage() {
		super();
		setDescription(ErlEditorMessages.IndentationPrefs_36);
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}

	List<Text> textFields = new ArrayList<Text>();
	List<Button> buttons = new ArrayList<Button>();

	/**
	 * Tells whether the fields are initialized.
	 */
	private boolean fieldsInitialized = false;

	/*
	 * @see PreferencePage#createContents(Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		final Composite c = new Composite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		c.setLayout(layout);
		createMyControls(c);
		setToPreferences();
		return c;
	}

	private void createMyControls(Composite parent) {
		for (int i = 0; i < INDENT_FIELDS.length; ++i) {
			final String desc = INDENT_FIELDS[i];
			final Composite c = parent;
			if (i < N_NUMERIC_KEYS) {
				final Label label = new Label(c, SWT.NONE);
				label.setText(desc);
				GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
				gd.horizontalIndent = 3;
				label.setLayoutData(gd);
				final Text text = new Text(c, SWT.BORDER | SWT.SINGLE);
				gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
				gd.widthHint = convertWidthInCharsToPixels(3);
				text.setLayoutData(gd);
				textFields.add(text);
				text.addModifyListener(fNumberFieldListener);
			} else {
				final Button button = new Button(c, SWT.CHECK);
				button.setText(desc);
				buttons.add(button);
			}
		}
	}

	static final String INDENT_KEY = "indentation"; //$NON-NLS-1$

	@SuppressWarnings("boxing")
	private void setToPreferences() {
		final List<String> l = getPreferences(INDENT_KEY, INDENT_KEYS,
				INDENT_DEFAULTS);
		for (int i = 0; i < l.size(); ++i) {
			final String s = l.get(i);
			if (i < N_NUMERIC_KEYS) {
				textFields.get(i).setText(s);
			} else {
				buttons.get(i - N_NUMERIC_KEYS).setSelection(
						s != null && !s.equals("0"));
			}
		}
		fieldsInitialized = true;
	}

	@Override
	protected void putPreferences() {
		final Preferences node = getPrefsNode();
		for (int i = 0; i < INDENT_KEYS.length; ++i) {
			int n;
			if (i < N_NUMERIC_KEYS) {
				n = Integer.parseInt(textFields.get(i).getText());
			} else {
				n = buttons.get(i - N_NUMERIC_KEYS).getSelection() ? 1 : 0;
			}
			node.putInt(INDENT_KEY + "/" + INDENT_KEYS[i], n); //$NON-NLS-1$
		}
		try {
			node.flush();
		} catch (final BackingStoreException e) {
			e.printStackTrace();
		}
	}

	/*
	 * @see PreferencePage#performDefaults()
	 */
	@Override
	protected void performDefaults() {
		for (int i = 0; i < INDENT_KEYS.length; ++i) {
			if (i < N_NUMERIC_KEYS) {
				final String s = INDENT_DEFAULTS[i];
				textFields.get(i).setText(s);
			} else {
				buttons.get(i - N_NUMERIC_KEYS).setSelection(
						!INDENT_DEFAULTS[i].equals("0"));
			}
		}
		super.performDefaults();
	}

	private final ModifyListener fNumberFieldListener = new ModifyListener() {
		public void modifyText(ModifyEvent e) {
			numberFieldChanged((Text) e.widget);
		}
	};

	void numberFieldChanged(Text textControl) {
		final String number = textControl.getText();
		final IStatus status = validatePositiveNumber(number);
		updateStatus(status);
	}

	private IStatus validatePositiveNumber(String number) {
		final StatusInfo status = new StatusInfo();
		if (number.length() == 0) {
			status
					.setError(ErlEditorMessages.ErlEditorPreferencePage_empty_input);
		} else {
			try {
				final int value = Integer.parseInt(number);
				if (value < 0) {
					status
							.setError(MessageFormat
									.format(
											ErlEditorMessages.ErlEditorPreferencePage_invalid_input,
											(Object[]) new String[] { number }));
				}
			} catch (final NumberFormatException e) {
				status
						.setError(MessageFormat
								.format(
										ErlEditorMessages.ErlEditorPreferencePage_invalid_input,
										(Object[]) new String[] { number }));
			}
		}
		return status;
	}

	void updateStatus(IStatus status) {
		if (!fieldsInitialized) {
			return;
		}
		setValid(!status.matches(IStatus.ERROR));
		applyToStatusLine(this, status);
	}

	/**
	 * Applies the status to the status line of a dialog page.
	 * 
	 * @param page
	 *            the dialog page
	 * @param status
	 *            the status
	 */
	public void applyToStatusLine(DialogPage page, IStatus status) {
		String message = status.getMessage();
		switch (status.getSeverity()) {
		case IStatus.OK:
			page.setMessage(message, IMessageProvider.NONE);
			page.setErrorMessage(null);
			break;
		case IStatus.WARNING:
			page.setMessage(message, IMessageProvider.WARNING);
			page.setErrorMessage(null);
			break;
		case IStatus.INFO:
			page.setMessage(message, IMessageProvider.INFORMATION);
			page.setErrorMessage(null);
			break;
		default:
			if (message.length() == 0) {
				message = null;
			}
			page.setMessage(null);
			page.setErrorMessage(message);
			break;
		}
	}

	public static Map<String, String> getKeysAndPrefs() {
		return getKeysAndPrefs(INDENT_KEY, INDENT_KEYS, INDENT_DEFAULTS);
	}
}
