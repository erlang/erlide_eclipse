/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.basicui.prefs;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.erlide.basiccore.ErtsPreferences;
import org.erlide.basicui.ErlideBasicUIPlugin;

/**
 * The system preferences
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ErtsPreferencePage extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage {

	public static final String ID = "org.erlide.ui.preferences.erts";

	private OtpDirectoryFieldEditor home;

	private PathEditor pathA;

	private PathEditor pathZ;

	// private BooleanFieldEditor longName;
	// private StringFieldEditor extra;

	/**
	 * Initialize the system preferences
	 * 
	 */
	public ErtsPreferencePage() {
		super(FieldEditorPreferencePage.GRID);
		setTitle("Erlang runtime");
	}

	/**
	 * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
	 */
	@Override
	protected void createFieldEditors() {
		Composite fieldEditorParent = getFieldEditorParent();
		home = new OtpDirectoryFieldEditor(IPrefConstants.ERTS_OTP_HOME,
				"ERL_TOP", fieldEditorParent);
		final Label zzzLabel = this.home.getLabelControl(fieldEditorParent);
		zzzLabel.setText(ErlideBasicUIPlugin
				.getResourceString("prefs.system.home"));
		addField(home);

		pathA = new PathEditor(IPrefConstants.ERTS_PATH_A, "patha",
				"Add a library directory to code:patha()", fieldEditorParent);
		final Label pathaLabel = this.pathA.getLabelControl(fieldEditorParent);
		pathaLabel.setText(ErlideBasicUIPlugin
				.getResourceString("prefs.system.patha"));
		addField(pathA);

		pathZ = new PathEditor(IPrefConstants.ERTS_PATH_Z, "pathz",
				"Add a library directory to code:pathz()", fieldEditorParent);
		final Label pathzLabel = this.pathZ.getLabelControl(fieldEditorParent);
		pathzLabel.setText(ErlideBasicUIPlugin
				.getResourceString("prefs.system.pathz"));
		addField(pathZ);

		// longName = new BooleanFieldEditor(IPrefConstants.ERTS_USE_LONG_NAME,
		// ErlangPlugin.getResourceString("prefs.system.longname"),
		// getFieldEditorParent());
		// addField(longName);
		// longName.setEnabled(false, getFieldEditorParent());

		// extra = new StringFieldEditor(IPrefConstants.ERTS_EXTRA_ARGS,
		// ErlideBasicUIPlugin.getResourceString("prefs.system.extra"),
		// getFieldEditorParent());
		// addField(extra);

	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.IPreferencePage#performOk()
	 */
	@Override
	public boolean performOk() {
		final ErtsPreferences prefs = ErlideBasicUIPlugin.getDefault()
				.getPreferences();
		prefs.setOtpHome(home.getStringValue());

		// prefs.setPathA(pathA.getPreferenceName());
		// prefs.setPathZ(pathZ.getStringValue());

		// prefs.getErtsPrefs().setUseLongName(longName.getBooleanValue());
		// prefs.setExtraErtsArgs(extra.getStringValue());
		prefs.updatePluginPreferences();

		showRestartWarning();

		return super.performOk();
	}

	private void showRestartWarning() {
		final IWorkbench workbench = PlatformUI.getWorkbench();
		workbench.getDisplay().asyncExec(new Runnable() {

			public void run() {
				final IWorkbenchWindow window = workbench
						.getActiveWorkbenchWindow();
				if (window != null) {
					final boolean restart = MessageDialog
							.openConfirm(
									null,
									"Workbench may need restart",
									"Your changes will only be visible after restarting. Do you want to restart now?");
					if (restart) {
						ErlideBasicUIPlugin.getDefault().getWorkbench()
								.restart();
					}
				}
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.FieldEditorPreferencePage#initialize()
	 */
	@Override
	protected void initialize() {
		super.initialize();
		final ErtsPreferences prefs = ErlideBasicUIPlugin.getDefault()
				.getPreferences();
		home.setStringValue(prefs.getOtpHome());

		// pathA.setStringValue(prefs.getPathA());
		// pathZ.setStringValue(prefs.getPathZ());

		// longName.g(prefs.getErtsPrefs().getExtraErtsArgs());
		// FIXME setStringValue shouldn't be used
		// extra.setStringValue(prefs.getExtraErtsArgs());

	}

}
