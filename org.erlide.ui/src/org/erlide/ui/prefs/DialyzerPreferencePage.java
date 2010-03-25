/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ControlEnableState;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.core.builder.DialyzerPreferences;
import org.erlide.core.builder.DialyzerUtils;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.OtpErlangObject;

import erlang.ErlideDialyze;

public class DialyzerPreferencePage extends PropertyPage implements
		IWorkbenchPreferencePage {

	DialyzerPreferences prefs;
	private IProject fProject;
	private Button fUseProjectSettings;
	private Link fChangeWorkspaceSettings;
	protected ControlEnableState fBlockEnableState;
	private Text pltEdit = null;
	private Combo fromCombo;
	private Button dialyzeCheckbox;
	private Composite prefsComposite;

	public DialyzerPreferencePage() {
		super();
		setTitle("Dialyzer options");
		setDescription("Select the options for dialyzer.");
	}

	@Override
	protected Control createContents(final Composite parent) {

		performDefaults();

		prefsComposite = new Composite(parent, SWT.NONE);
		prefsComposite.setLayout(new GridLayout());

		final Group group = new Group(prefsComposite, SWT.NONE);
		group.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false));
		group.setLayout(new GridLayout(1, false));
		createDialyzeCheckbox(group);
		createPltSelection(group);
		createPltCheck(group);
		createFromSelection(group);

		if (isProjectPreferencePage()) {
			final boolean useProjectSettings = hasProjectSpecificOptions(fProject);
			enableProjectSpecificSettings(useProjectSettings);
		}

		return prefsComposite;
	}

	private final class CheckPltOperation implements IRunnableWithProgress {

		public void run(final IProgressMonitor monitor)
				throws InvocationTargetException, InterruptedException {
			final String plt = pltEdit.getText();
			monitor.beginTask("Checking PLT file " + plt, 1);
			Backend backend;
			final BackendManager backendManager = ErlangCore
					.getBackendManager();
			try {
				if (fProject != null) {
					backend = backendManager.getBuildBackend(fProject);
				} else {
					backend = backendManager.getIdeBackend();
				}
				final OtpErlangObject result = ErlideDialyze.checkPlt(backend,
						plt);
				DialyzerUtils.checkDialyzeError(result);
				monitor.done();
			} catch (final Exception e) {
				throw new InvocationTargetException(e);
			}
		}
	}

	private void createPltCheck(final Group group) {
		final Composite comp = new Composite(group, SWT.NONE);
		comp.setLayout(new GridLayout(2, false));
		final Button b = new Button(comp, SWT.PUSH);
		b.setText("Check PLT");
		b.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final IRunnableWithProgress op = new CheckPltOperation();
				try {
					final IWorkbench wb = PlatformUI.getWorkbench();
					wb.getProgressService().run(false, true, op);
					// FIXME setting "fork" to true gives
					// "Invalid Thread Access" exception, why?
				} catch (final InvocationTargetException e1) {
					final Throwable t = e1.getCause();
					Display.getDefault().asyncExec(new Runnable() {
						public void run() {
							MessageDialog.openError(getShell(),
									"Dialyzer error", t.getMessage());
						}
					});
				} catch (final InterruptedException e1) {
					ErlLogger.error(e1);
				}
			}
		});
		final Label l = new Label(comp, SWT.NONE);
		l.setText("Warning: this can take some time");
	}

	private void createDialyzeCheckbox(final Composite group) {
		final Composite comp = new Composite(group, SWT.NONE);
		// comp.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true,
		// false));
		comp.setLayout(new GridLayout(1, false));
		dialyzeCheckbox = new Button(comp, SWT.CHECK);
		dialyzeCheckbox.setText("Run dialyzer when compiling");
		dialyzeCheckbox.setSelection(prefs.getDialyzeOnCompile());
	}

	private void createFromSelection(final Composite group) {
		final Composite comp = new Composite(group, SWT.NONE);
		// comp.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true,
		// false));
		comp.setLayout(new GridLayout(2, false));
		final Label l = new Label(comp, SWT.NONE);
		l.setText("Analyze from ");
		fromCombo = new Combo(comp, SWT.READ_ONLY);
		fromCombo.setItems(new String[] { "Source", "Binaries" });
		fromCombo.setText(fromCombo.getItem(prefs.getFromSource() ? 0 : 1));
	}

	private void createPltSelection(final Composite group) {
		final Composite comp = new Composite(group, SWT.NONE);
		// comp.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true,
		// false));
		comp.setLayout(new GridLayout(3, false));
		GridData gd = new GridData(SWT.FILL, SWT.BEGINNING, true, false);
		comp.setLayoutData(gd);
		final Label l = new Label(comp, SWT.NONE);
		l.setText("Select PLT");
		pltEdit = new Text(comp, SWT.BORDER);
		gd = new GridData(SWT.FILL, GridData.CENTER, true, false);
		pltEdit.setLayoutData(gd);
		final Button b = new Button(comp, SWT.PUSH);
		b.setText("Browse...");
		b.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final String s = pltEdit.getText();
				final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
				dialog.setText("Select PLT file");
				dialog.setFileName(s);
				dialog.setFilterExtensions(new String[] { "*.plt" });
				final String result = dialog.open();
				if (result == null) {
					return;
				}
				pltEdit.setText(result);
			}
		});
		pltEdit.setText(prefs.getPltPath());
	}

	protected boolean hasProjectSpecificOptions(final IProject project) {
		final DialyzerPreferences p = new DialyzerPreferences(project);
		return p.hasOptionsAtLowestScope();
	}

	private boolean isProjectPreferencePage() {
		return fProject != null;
	}

	@Override
	protected Label createDescriptionLabel(final Composite parent) {
		createProjectSpecificSettingsCheckBoxAndLink(parent);
		return super.createDescriptionLabel(parent);
	}

	protected void enableProjectSpecificSettings(
			final boolean useProjectSpecificSettings) {
		fUseProjectSettings.setSelection(useProjectSpecificSettings);
		enablePreferenceContent(useProjectSpecificSettings);
		fChangeWorkspaceSettings.setEnabled(!useProjectSpecificSettings);
		// doStatusChanged();
	}

	private void enablePreferenceContent(
			final boolean useProjectSpecificSettings) {
		if (useProjectSpecificSettings) {
			if (fBlockEnableState != null) {
				fBlockEnableState.restore();
				fBlockEnableState = null;
			}
		} else {
			if (fBlockEnableState == null) {
				fBlockEnableState = ControlEnableState.disable(prefsComposite);
			}
		}
	}

	private void createProjectSpecificSettingsCheckBoxAndLink(
			final Composite parent) {
		if (isProjectPreferencePage()) {
			final Composite composite = new Composite(parent, SWT.NONE);
			composite.setFont(parent.getFont());
			final GridLayout layout = new GridLayout(2, false);
			layout.marginHeight = 0;
			layout.marginWidth = 0;
			composite.setLayout(layout);
			composite.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
					false));

			// final IDialogFieldListener listener = new IDialogFieldListener()
			// {
			// public void dialogFieldChanged(final DialogField field) {
			// final boolean enabled = ((SelectionButtonDialogField) field)
			// .isSelected();
			// enableProjectSpecificSettings(enabled);
			//
			// if (enabled && getData() != null) {
			// applyData(getData());
			// }
			// }
			// };

			fUseProjectSettings = new Button(composite, SWT.CHECK);
			fUseProjectSettings.setText("Enable project specific settings");
			fUseProjectSettings.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(final SelectionEvent e) {
					final boolean sel = fUseProjectSettings.getSelection();
					enableProjectSpecificSettings(sel);
					super.widgetSelected(e);
				}
			});
			// fUseProjectSettings.setDialogFieldListener(listener);
			// fUseProjectSettings
			// .setLabelText(PreferencesMessages.PropertyAndPreferencePage_useprojectsettings_label);
			// LayoutUtil.setHorizontalGrabbing(fUseProjectSettings
			// .getSelectionButton(null));

			if (true) { // if (offerLink()) {
				fChangeWorkspaceSettings = createLink(composite,
						"Configure Workspace settings...");
				fChangeWorkspaceSettings.setLayoutData(new GridData(SWT.END,
						SWT.CENTER, false, false));
			}
			// else {
			// LayoutUtil.setHorizontalSpan(fUseProjectSettings
			// .getSelectionButton(null), 2);
			// }

			final Label horizontalLine = new Label(composite, SWT.SEPARATOR
					| SWT.HORIZONTAL);
			horizontalLine.setLayoutData(new GridData(GridData.FILL,
					GridData.FILL, true, false, 2, 1));
			horizontalLine.setFont(composite.getFont());
		} else { // if (supportsProjectSpecificOptions() && offerLink()) {
			fChangeWorkspaceSettings = createLink(parent,
					"Configure project specific settings..");
			fChangeWorkspaceSettings.setLayoutData(new GridData(SWT.END,
					SWT.CENTER, true, false));
		}

	}

	private Link createLink(final Composite composite, final String text) {
		final Link link = new Link(composite, SWT.NONE);
		link.setFont(composite.getFont());
		link.setText("<A>" + text + "</A>"); //$NON-NLS-1$//$NON-NLS-2$
		link.addSelectionListener(new SelectionListener() {
			public void widgetSelected(final SelectionEvent e) {
				doLinkActivated((Link) e.widget);
			}

			public void widgetDefaultSelected(final SelectionEvent e) {
				doLinkActivated((Link) e.widget);
			}
		});
		return link;
	}

	void doLinkActivated(final Link widget) {
		if (isProjectPreferencePage()) {
			openWorkspacePreferences(null);
		} else {
			final List<IProject> erlProjects = new ArrayList<IProject>();
			final Set<IProject> projectsWithSpecifics = new HashSet<IProject>();
			final IErlModel model = ErlangCore.getModel();
			try {
				for (final IErlProject ep : model.getErlangProjects()) {
					final IProject p = ep.getProject();
					if (hasProjectSpecificOptions(p)) {
						projectsWithSpecifics.add(p);
					}
					erlProjects.add(p);
				}
			} catch (final ErlModelException e) {
			}
			final ProjectSelectionDialog dialog = new ProjectSelectionDialog(
					getShell(), erlProjects, projectsWithSpecifics);
			if (dialog.open() == Window.OK) {
				final IProject res = (IProject) dialog.getFirstResult();
				openProjectProperties(res);
			}
		}
	}

	private void openProjectProperties(final IProject project) {
		final String id = getPropertyPageID();
		if (id != null) {
			PreferencesUtil.createPropertyDialogOn(getShell(), project, id,
					new String[] { id }, null).open();
		}
	}

	protected final void openWorkspacePreferences(final Object data) {
		final String id = getPreferencePageID();
		PreferencesUtil.createPreferenceDialogOn(getShell(), id,
				new String[] { id }, data).open();
	}

	protected String getPreferencePageID() {
		return "org.erlide.ui.preferences.dialyzer";
	}

	protected String getPropertyPageID() {
		return "org.erlide.ui.properties.dialyzerPreferencePage";
	}

	boolean optionsAreOk() {
		final File f = new File(pltEdit.getText());
		return f.exists();
	}

	@Override
	public boolean performOk() {
		try {
			prefs.setPltPath(pltEdit.getText());
			prefs.setFromSource(fromCombo.getSelectionIndex() == 0);
			prefs.setDialyzeOnCompile(dialyzeCheckbox.getSelection());
			if (fUseProjectSettings != null
					&& !fUseProjectSettings.getSelection()
					&& isProjectPreferencePage()) {
				prefs.removeAllProjectSpecificSettings();
			} else {
				prefs.store();
			}
		} catch (final BackingStoreException e) {
			ErlLogger.warn(e);
		}
		return super.performOk();
	}

	@Override
	protected void performDefaults() {
		if (fProject == null) {
			prefs = new DialyzerPreferences();
		} else {
			prefs = new DialyzerPreferences(fProject);
		}
		try {
			prefs.load();
			if (pltEdit != null) {
				pltEdit.setText(prefs.getPltPath());
			}
			if (fromCombo != null) {
				fromCombo.setText(fromCombo.getItem(prefs.getFromSource() ? 0
						: 1));
			}
			if (dialyzeCheckbox != null) {
				dialyzeCheckbox.setSelection(prefs.getDialyzeOnCompile());
			}
		} catch (final BackingStoreException e) {
			ErlLogger.warn(e);
		}
		super.performDefaults();
	}

	@Override
	public void setElement(final IAdaptable element) {
		fProject = (IProject) element.getAdapter(IResource.class);
		super.setElement(element);
	}

	public void init(final IWorkbench workbench) {
		performDefaults();
	}
}
