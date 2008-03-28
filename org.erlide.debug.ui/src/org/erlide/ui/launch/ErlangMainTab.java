/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlProject;
import org.erlide.runtime.backend.IErlangLaunchConfigurationAttributes;

public class ErlangMainTab extends AbstractLaunchConfigurationTab {

	private Text fProjText;

	private Button fProjButton;

	private Text moduleText;

	private Text funcText;

	private Text otpPathText;

	private Button otpPathBrowseButton;

	public void createControl(Composite parent) {
		// final Font font = parent.getFont();

		final Composite comp = new Composite(parent, SWT.NONE);
		setControl(comp);
		final GridLayout topLayout = new GridLayout();
		comp.setLayout(topLayout);

		Label l = new Label(comp, SWT.NONE);
		l.setText("*** Only for internal erlide testing! ***");

		createProjectEditor(comp);
		createStartFunctionEditor(comp);
		createOtpPathEditor(comp);
	}

	private void createOtpPathEditor(Composite parent) {
		final Font font = parent.getFont();
		Group group = new Group(parent, SWT.NONE);
		group.setText("OTP Home");
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		group.setLayoutData(gd);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		group.setLayout(layout);
		group.setFont(font);

		// final Label otpPathLabel = new Label(group, SWT.NONE);
		// otpPathLabel.setText("Otp Home");
		// otpPathLabel.setFont(font);

		otpPathText = new Text(group, SWT.SINGLE | SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		otpPathText.setLayoutData(gd);
		otpPathText.setFont(font);
		otpPathText.addModifyListener(fBasicModifyListener);

		otpPathBrowseButton = new Button(group, SWT.PUSH);
		otpPathBrowseButton.setText("Browse...");
		otpPathBrowseButton.setFont(font);
		otpPathBrowseButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				handleotpPathBrowseButtonSelected();
			}

		});

	}

	private void createStartFunctionEditor(final Composite parent) {
		final Font font = parent.getFont();
		Group group = new Group(parent, SWT.NONE);
		group.setText("Start function");
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		group.setLayoutData(gd);
		GridLayout layout = new GridLayout();
		layout.numColumns = 4;
		group.setLayout(layout);
		group.setFont(font);

		final Label moduleLabel = new Label(group, SWT.NONE);
		moduleLabel.setText("module");
		moduleLabel.setFont(font);

		moduleText = new Text(group, SWT.SINGLE | SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		moduleText.setLayoutData(gd);
		moduleText.setFont(font);
		moduleText.addModifyListener(fBasicModifyListener);

		final Label funcLabel = new Label(group, SWT.NONE);
		funcLabel.setText("function");
		funcLabel.setFont(font);

		funcText = new Text(group, SWT.SINGLE | SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		funcText.setLayoutData(gd);
		funcText.setFont(font);
		funcText.addModifyListener(fBasicModifyListener);
	}

	void handleotpPathBrowseButtonSelected() {

		String last = otpPathText.getText().trim();
		// if (last.length() == 0) {
		// last =
		// DebugUIPlugin.getDefault().getDialogSettings().get(LAST_PATH_SETTING);
		// }
		if (last == null) {
			last = ""; //$NON-NLS-1$
		}
		DirectoryDialog dialog = new DirectoryDialog(getShell(), SWT.SINGLE);
		dialog.setText("Select otp home");
		dialog.setMessage("Select otp home <msg>");
		dialog.setFilterPath(last);
		String result = dialog.open();
		if (result == null) {
			return;
		}
		otpPathText.setText(result);
	}

	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_ENODE_MODULE,
				IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE);
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_ENODE_FUNCTION,
				IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION);
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_OTP_HOME,
				IErlangLaunchConfigurationAttributes.DEFAULT_OTP_HOME);
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_PROJECT_NAME, "");
	}

	public void initializeFrom(ILaunchConfiguration configuration) {
		try {
			moduleText.setText(configuration.getAttribute(
					IErlangLaunchConfigurationAttributes.ATTR_ENODE_MODULE,
					IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE));
		} catch (final CoreException e) {
			moduleText
					.setText(IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE);
		}
		try {
			funcText
					.setText(configuration
							.getAttribute(
									IErlangLaunchConfigurationAttributes.ATTR_ENODE_FUNCTION,
									IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION));
		} catch (final CoreException e) {
			funcText
					.setText(IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION);
		}

		try {
			otpPathText.setText(configuration.getAttribute(
					IErlangLaunchConfigurationAttributes.ATTR_OTP_HOME,
					IErlangLaunchConfigurationAttributes.DEFAULT_OTP_HOME));
		} catch (CoreException e) {
			otpPathText
					.setText(IErlangLaunchConfigurationAttributes.DEFAULT_OTP_HOME);
		}
		try {
			fProjText
					.setText(configuration
							.getAttribute(
									IErlangLaunchConfigurationAttributes.ATTR_PROJECT_NAME,
									""));
		} catch (CoreException e) {
			fProjText.setText("");
		}
	}

	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_ENODE_MODULE,
				moduleText.getText());
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_ENODE_FUNCTION,
				funcText.getText());
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_OTP_HOME, otpPathText
						.getText());
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_PROJECT_NAME,
				fProjText.getText());
	}

	public String getName() {
		return "Erlang node";
	}

	@Override
	public boolean isValid(ILaunchConfiguration launchConfig) {
		return true;
	}

	private final ModifyListener fBasicModifyListener = new ModifyListener() {

		@SuppressWarnings("synthetic-access")
		public void modifyText(ModifyEvent evt) {
			updateLaunchConfigurationDialog();
		}
	};

	/**
	 * Creates the widgets for specifying a main type.
	 * 
	 * @param parent
	 *            the parent composite
	 */
	protected void createProjectEditor(Composite parent) {
		Font font = parent.getFont();
		Group group = new Group(parent, SWT.NONE);
		group.setText("Project");
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		group.setLayoutData(gd);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		group.setLayout(layout);
		group.setFont(font);
		fProjText = new Text(group, SWT.SINGLE | SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		fProjText.setLayoutData(gd);
		fProjText.setFont(font);
		fProjText.addModifyListener(fBasicModifyListener);
		fProjButton = createPushButton(group, "Browse...", null);
		fProjButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				handleProjectButtonSelected();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}
		});
	}

	protected void handleProjectButtonSelected() {
		// TODO Auto-generated method stub
		IErlProject project = chooseErlProject();
		if (project == null) {
			return;
		}
		String projectName = project.getName();
		fProjText.setText(projectName);
	}

	private IErlProject chooseErlProject() {
		ILabelProvider labelProvider = new ErlProjectNameLabelProvider();
		// JavaElementLabelProvider(JavaElementLabelProvider.SHOW_DEFAULT);
		ElementListSelectionDialog dialog = new ElementListSelectionDialog(
				getShell(), labelProvider);
		dialog.setTitle("Select Project");
		dialog.setMessage("SelectProject");
		IErlModelManager mm = ErlangCore.getModelManager();
		IErlModel m = mm.getErlangModel();
		IErlElement[] projects;
		try {
			projects = m.getChildren().toArray(new IErlElement[0]);
		} catch (ErlModelException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
		dialog.setElements(projects);
		IErlProject erlProject = getErlProject();
		if (erlProject != null) {
			dialog.setInitialSelections(new Object[] { erlProject });
		}
		if (dialog.open() == Window.OK) {
			return (IErlProject) dialog.getFirstResult();
		}
		return null;
	}

	private IErlProject getErlProject() {
		String projectName = fProjText.getText().trim();
		if (projectName.length() == 0) {
			return null;
		}
		IErlModel m = ErlangCore.getModelManager().getErlangModel();
		return m.getErlangProject(projectName);
	}

	public static class ErlProjectNameLabelProvider implements ILabelProvider {

		public Image getImage(Object element) {
			return null;
		}

		public String getText(Object element) {
			return ((IErlProject) element).getName();
		}

		public void addListener(ILabelProviderListener listener) {
		}

		public void dispose() {
		}

		public boolean isLabelProperty(Object element, String property) {
			return false;
		}

		public void removeListener(ILabelProviderListener listener) {
		}

	}
}
