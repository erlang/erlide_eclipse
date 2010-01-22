/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.properties;

import java.util.Arrays;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.RuntimeInfoListener;
import org.erlide.jinterface.backend.RuntimeVersion;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.prefs.RuntimePreferencePage;
import org.erlide.ui.properties.internal.MockupPreferenceStore;

public class OldErlProjectPropertyPage extends PropertyPage implements
		IPropertyChangeListener, RuntimeInfoListener {

	private Combo runtimeVersion;
	private Text output;
	private Text source;
	private Text include;
	private MockupPreferenceStore mockPrefs;
	Text externalIncludes;
	private Button externalIncludesBrowse;
	Text externalModules;
	private Button externalModulesBrowse;

	/**
	 * Constructor for ErlProjectPropertyPage.
	 */
	public OldErlProjectPropertyPage() {
		super();
		ErlangCore.getRuntimeInfoManager().addListener(this);
	}

	/**
	 * @see PreferencePage#createContents(Composite)
	 */
	@Override
	protected Control createContents(final Composite parent) {
		if (ErlangCore.getRuntimeInfoManager().getDefaultRuntime() == null) {
			openPreferencePage();
		}

		mockPrefs = new MockupPreferenceStore();
		mockPrefs.addPropertyChangeListener(this);

		// create the composite to hold the widgets
		final Composite composite = new Composite(parent, SWT.NONE);

		// create the desired layout for this wizard page
		final GridLayout gl = new GridLayout();
		gl.numColumns = 4;
		composite.setLayout(gl);

		final String resourceString = ErlideUIPlugin
				.getResourceString("wizards.labels.buildoutput");
		// create the widgets and their grid data objects
		final Label outLabel = new Label(composite, SWT.NONE);
		outLabel.setText(Messages.OldErlProjectPropertyPage_outLabel_text);
		final GridData gd_Label = new GridData();
		gd_Label.minimumWidth = 50;
		outLabel.setLayoutData(gd_Label);
		outLabel.setText(resourceString + ":");
		output = new Text(composite, SWT.BORDER);
		GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gd.minimumWidth = 50;
		gd.widthHint = 256;
		output.setLayoutData(gd);
		output.addListener(SWT.Modify, modifyListener);
		new Label(composite, SWT.NONE);
		new Label(composite, SWT.NONE);

		final Label l1 = new Label(composite, SWT.NONE);
		l1.setText(Messages.OldErlProjectPropertyPage_l1_text);
		final String resourceString2 = ErlideUIPlugin
				.getResourceString("wizards.labels.source");
		l1.setText(resourceString2 + ":");
		source = new Text(composite, SWT.BORDER);
		source
				.setToolTipText(Messages.OldErlProjectPropertyPage_source_toolTipText);
		gd = new GridData(SWT.FILL, SWT.TOP, false, false);
		gd.widthHint = 325;
		source.setLayoutData(gd);
		source.addListener(SWT.Modify, modifyListener);
		new Label(composite, SWT.NONE);

		final Label label = new Label(composite, SWT.NONE);
		label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		final Label includesLabel = new Label(composite, SWT.NONE);
		includesLabel
				.setText(Messages.OldErlProjectPropertyPage_includesLabel_text);
		include = new Text(composite, SWT.BORDER);
		include
				.setToolTipText(Messages.OldErlProjectPropertyPage_include_toolTipText);
		gd = new GridData(SWT.FILL, SWT.TOP, true, false);
		gd.widthHint = 313;
		include.setLayoutData(gd);
		include.addListener(SWT.Modify, modifyListener);
		new Label(composite, SWT.NONE);

		new Label(composite, SWT.NONE);

		final String resourceString4 = ErlideUIPlugin
				.getResourceString("wizards.labels.testsources");
		Label label_1 = new Label(composite, SWT.NONE);
		label_1.setText(resourceString4 + ":");

		text_1 = new Text(composite, SWT.BORDER);
		text_1.setEditable(false);
		text_1.setToolTipText("enter a list of folders");
		text_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
				1));
		new Label(composite, SWT.NONE);
		new Label(composite, SWT.NONE);

		final Label nodeNameLabel_1 = new Label(composite, SWT.NONE);
		nodeNameLabel_1
				.setText(Messages.OldErlProjectPropertyPage_nodeNameLabel_1_text);

		final Composite composite_1 = new Composite(composite, SWT.NONE);
		composite_1.setLayout(new RowLayout(SWT.HORIZONTAL));
		GridData gridData = new GridData(SWT.FILL, SWT.CENTER, false, false);
		gridData.widthHint = 338;
		composite_1.setLayoutData(gridData);

		runtimeVersion = new Combo(composite_1, SWT.READ_ONLY);
		runtimeVersion
				.setToolTipText(Messages.OldErlProjectPropertyPage_runtimeVersion_toolTipText);
		final RowData rd_runtimeVersion = new RowData();
		rd_runtimeVersion.width = 64;
		runtimeVersion.setLayoutData(rd_runtimeVersion);
		final String[] versions = BackendManager.SUPPORTED_MAIN_VERSIONS;
		runtimeVersion.setItems(versions);
		runtimeVersion.select(Arrays.binarySearch(versions,
				BackendManager.DEFAULT_VERSION));
		new Label(composite, SWT.NONE);
		new Label(composite, SWT.NONE);
		Label l = new Label(composite, SWT.NONE);
		l.setText("External modules:");

		if (ErlideUtil.isTest()) {
			externalModules = new Text(composite, SWT.BORDER);
			final GridData gd1 = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gd1.minimumWidth = 50;
			gd1.widthHint = 256;
			externalModules.setLayoutData(gd1);

			externalModules.addListener(SWT.Modify, modifyListener);

			externalModulesBrowse = new Button(composite, SWT.PUSH);
			externalModulesBrowse.setText("Browse...");
			externalModulesBrowse.addSelectionListener(new SelectionListener() {

				public void widgetDefaultSelected(final SelectionEvent e) {
				}

				public void widgetSelected(final SelectionEvent e) {
					handleBrowseSelected(externalModules,
							"Select file with external modules", "*.erlidex");
				}

			});
			new Label(composite, SWT.NONE);
			l = new Label(composite, SWT.NONE);
			l.setText("External includes:");

			externalIncludes = new Text(composite, SWT.BORDER);
			final GridData gd2 = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gd2.minimumWidth = 50;
			gd2.widthHint = 256;
			externalIncludes.setLayoutData(gd2);

			externalIncludes.addListener(SWT.Modify, modifyListener);

			externalIncludesBrowse = new Button(composite, SWT.PUSH);
			externalIncludesBrowse.setText("Browse...");
			new Label(composite, SWT.NONE);
			externalIncludesBrowse
					.addSelectionListener(new SelectionListener() {

						public void widgetDefaultSelected(final SelectionEvent e) {
						}

						public void widgetSelected(final SelectionEvent e) {
							handleBrowseSelected(externalIncludes,
									"Select file with external includes",
									"*.erlidex");
						}

					});
		}

		performDefaults();
		setValid(testPageComplete());

		return composite;
	}

	protected void handleBrowseSelected(final Text text,
			final String selectTipString, final String extension) {
		String last = text.getText();
		if (last == null) {
			last = ""; //$NON-NLS-1$
		} else {
			last = last.trim();
		}
		final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
		dialog.setText(selectTipString);
		dialog.setFileName(last);
		dialog.setFilterExtensions(new String[] { extension });
		final String result = dialog.open();
		if (result == null) {
			return;
		}
		text.setText(result);
	}

	@Override
	protected void performDefaults() {
		// Populate the owner text field with the default value
		final IAdaptable prj = getElement();
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties((IProject) prj.getAdapter(IProject.class));

		source.setText(PreferencesUtils.packList(prefs.getSourceDirs()));
		include.setText(PreferencesUtils.packList(prefs.getIncludeDirs()));
		output.setText(prefs.getOutputDir());
		RuntimeVersion rv = prefs.getRuntimeVersion();
		if (!rv.isDefined()) {
			final RuntimeInfo runtimeInfo = prefs.getRuntimeInfo();
			if (runtimeInfo != null) {
				rv = runtimeInfo.getVersion();
			}
		}
		final String[] items = runtimeVersion.getItems();
		final RuntimeVersion asMinor = rv.asMinor();
		runtimeVersion.select(Arrays.binarySearch(items, asMinor.toString()));

		if (ErlideUtil.isTest()) {
			externalModules.setText(prefs.getExternalModulesFile());
			externalIncludes.setText(prefs.getExternalIncludesFile());
		}

	}

	@Override
	public boolean performOk() {
		ErlangCore.getRuntimeInfoManager().removeListener(this);
		// store the value in the owner text field
		final IAdaptable prj = getElement();
		final IProject project = (IProject) prj.getAdapter(IProject.class);
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);

		prefs.setOutputDir(output.getText());
		prefs.setSourceDirs(PreferencesUtils.unpackList(source.getText()));
		prefs.setIncludeDirs(PreferencesUtils.unpackList(include.getText()));
		prefs.setRuntimeVersion(new RuntimeVersion(runtimeVersion.getText()));

		if (ErlideUtil.isTest()) {
			prefs.setExternalModulesFile(externalModules.getText());
			prefs.setExternalIncludesFile(externalIncludes.getText());
		}
		final IEclipsePreferences root = new ProjectScope(prefs.getProject())
				.getNode(ErlangPlugin.PLUGIN_ID);
		prefs.store(root);

		return true;
	}

	private final Listener modifyListener = new Listener() {
		public void handleEvent(final Event e) {
			setValid(testPageComplete());
		}
	};
	private Text text_1;

	protected boolean testPageComplete() {
		if (output.getText() == null || output.getText().trim().length() == 0) {
			setErrorMessage(ErlideUIPlugin
					.getResourceString("wizards.errors.outputrequired"));
			return false;
		}

		if (source.getText() == null || source.getText().trim().length() == 0) {
			setErrorMessage(ErlideUIPlugin
					.getResourceString("wizards.errors.sourcerequired"));
			return false;
		}
		setErrorMessage(null);
		setMessage(null);
		return true;
	}

	public void propertyChange(final PropertyChangeEvent event) {
		ErlLogger.debug("*+> " + event);
	}

	public void infoChanged() {
		if (runtimeVersion.isDisposed()) {
			return;
		}
		final String[] runtimes = ErlangCore.getRuntimeInfoManager()
				.getRuntimeNames().toArray(new String[] {});
		final RuntimeInfo defaultRuntime = ErlangCore.getRuntimeInfoManager()
				.getDefaultRuntime();
		if (defaultRuntime != null) {
			int db = 0;
			for (final String info : runtimes) {
				if (info.equals(defaultRuntime.getName())) {
					break;
				}
				db++;
			}
			if (db >= 0 && db < runtimes.length) {
			}
		}
	}

	private static void openPreferencePage() {
		final IPreferencePage page = new RuntimePreferencePage();
		final PreferenceManager mgr = new PreferenceManager();
		final IPreferenceNode node = new PreferenceNode("1", page);
		mgr.addToRoot(node);
		final Display display = PlatformUI.getWorkbench().getDisplay();
		display.asyncExec(new Runnable() {

			public void run() {
				final PreferenceDialog dialog = new PreferenceDialog(display
						.getActiveShell(), mgr);
				dialog.create();
				dialog.setMessage(page.getTitle());
				dialog.open();
				ErlangCore.getRuntimeInfoManager().load();
			}
		});
	}
}
