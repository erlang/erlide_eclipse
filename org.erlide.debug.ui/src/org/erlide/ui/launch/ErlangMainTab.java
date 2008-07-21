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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.erlide.basicui.util.SWTUtil;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlProject;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.IErlangLaunchConfigurationAttributes;

public class ErlangMainTab extends AbstractLaunchConfigurationTab {

	private Text fProjText;

	private Button fProjButton;

	private Text moduleText;

	private Text funcText;

	private Text otpPathText;

	private Button otpPathBrowseButton;

	private Button startedNodeCheckbox;

	private Text otpNodeName;

	private CheckboxTableViewer projectsTable;

	public void createControl(final Composite parent) {
		// final Font font = parent.getFont();

		final Composite comp = new Composite(parent, SWT.NONE);
		setControl(comp);
		final GridLayout topLayout = new GridLayout();
		comp.setLayout(topLayout);

		// final Label l = new Label(comp, SWT.NONE);
		// l.setText("*** Only for internal erlide testing! ***");

		createProjectEditor(comp);
		createErlangEditor(comp);
		createOtherProjectsEditor(comp);
	}

	private void createErlangEditor(final Composite comp) {
		// final Font font = comp.getFont();
		final Group group = SWTUtil.createGroup(comp, "Erlang", 1,
				GridData.FILL_HORIZONTAL);
		startedNodeCheckbox = new Button(group, SWT.CHECK);
		startedNodeCheckbox.setText("Connect to started node");
		startedNodeCheckbox.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final boolean startNew = !startedNodeCheckbox.getSelection();
				otpPathText.setEnabled(startNew);
				otpPathBrowseButton.setEnabled(startNew);
				moduleText.setEnabled(startNew);
				funcText.setEnabled(startNew);
				updateLaunchConfigurationDialog();
			}
		});
		createOtpPathEditor(group);
		createStartFunctionEditor(group);
		createNodeNameEditor(group);
	}

	private void createNodeNameEditor(final Group parent) {
		// final Font font = parent.getFont();
		final Label otpPathLabel = new Label(parent, SWT.NONE);
		otpPathLabel.setText("Node name");
		// otpPathLabel.setFont(font);
		final Group group = SWTUtil.createGroup(parent, null, 2,
				GridData.FILL_HORIZONTAL);

		// group.setFont(font);

		otpNodeName = new Text(group, SWT.SINGLE | SWT.BORDER);
		final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		otpNodeName.setLayoutData(gd);
		// otpPathText.setFont(font);
		otpNodeName.addModifyListener(fBasicModifyListener);
	}

	private void createOtpPathEditor(final Composite parent) {
		// final Font font = parent.getFont();
		final Label otpPathLabel = new Label(parent, SWT.NONE);
		otpPathLabel.setText("OTP Home");
		// otpPathLabel.setFont(font);
		final Group group = SWTUtil.createGroup(parent, null, 3,
				GridData.FILL_HORIZONTAL);
		// group.setFont(font);

		otpPathText = new Text(group, SWT.SINGLE | SWT.BORDER);
		final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		otpPathText.setLayoutData(gd);
		// otpPathText.setFont(font);
		otpPathText.addModifyListener(fBasicModifyListener);

		otpPathBrowseButton = new Button(group, SWT.PUSH);
		otpPathBrowseButton.setText("Browse...");
		// otpPathBrowseButton.setFont(font);
		otpPathBrowseButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent evt) {
				handleotpPathBrowseButtonSelected();
			}

		});

	}

	private void createStartFunctionEditor(final Composite parent) {
		// final Font font = parent.getFont();
		final Group group = SWTUtil.createGroup(parent, "Start function", 4,
				GridData.FILL_HORIZONTAL);

		final Label moduleLabel = new Label(group, SWT.NONE);
		moduleLabel.setText("module");
		// moduleLabel.setFont(font);

		moduleText = new Text(group, SWT.SINGLE | SWT.BORDER);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		moduleText.setLayoutData(gd);
		// moduleText.setFont(font);
		moduleText.addModifyListener(fBasicModifyListener);

		final Label funcLabel = new Label(group, SWT.NONE);
		funcLabel.setText("function");
		// funcLabel.setFont(font);

		funcText = new Text(group, SWT.SINGLE | SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		funcText.setLayoutData(gd);
		// funcText.setFont(font);
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
		final DirectoryDialog dialog = new DirectoryDialog(getShell(),
				SWT.SINGLE);
		dialog.setText("Select otp home");
		dialog.setMessage("Select otp home <msg>");
		dialog.setFilterPath(last);
		final String result = dialog.open();
		if (result == null) {
			return;
		}
		otpPathText.setText(result);
	}

	/**
	 * Creates the projects table control
	 * 
	 * @param parent
	 *            the parent composite to add this one to
	 * @since 3.2
	 */
	private void createOtherProjectsEditor(final Composite parent) {
		// final Composite comp = new Composite(parent, SWT.NONE);
		// setControl(comp);
		// final GridLayout topLayout = new GridLayout();
		// comp.setLayout(topLayout);

		final Group projComp = SWTUtil.createGroup(parent, "Other projects", 1,
				GridData.FILL_BOTH);
		projectsTable = CheckboxTableViewer.newCheckList(projComp, SWT.CHECK
				| SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION);
		final Control table = projectsTable.getControl();
		final GridData gd = new GridData(GridData.FILL_BOTH);
		table.setLayoutData(gd);
		table.setFont(parent.getFont());
		projectsTable.setContentProvider(new OtherProjectsContentProvider());
		projectsTable.setLabelProvider(new OtherProjectsLabelProvider());
		projectsTable.addCheckStateListener(new ICheckStateListener() {
			@SuppressWarnings("synthetic-access")
			public void checkStateChanged(final CheckStateChangedEvent event) {
				updateLaunchConfigurationDialog();
			}
		});
	}

	/**
	 * Content provider for the projects table
	 */
	class OtherProjectsContentProvider implements IStructuredContentProvider {

		public Object[] getElements(final Object inputElement) {
			final IWorkspaceRoot root = ResourcesPlugin.getWorkspace()
					.getRoot();
			return root.getProjects();
		}

		public void dispose() {
		}

		public void inputChanged(final Viewer viewer, final Object oldInput,
				final Object newInput) {
		}

	}

	/**
	 * Provides the labels for the projects table
	 * 
	 */
	class OtherProjectsLabelProvider implements ITableLabelProvider {

		// private final Map fImages = new HashMap();

		public Image getColumnImage(final Object element, final int columnIndex) {
			// Image image = (Image)fImages.get(element);
			// if (image == null) {
			// final ImageDescriptor descriptor =
			// ((LaunchGroupExtension)element).getImageDescriptor();
			// if (descriptor != null) {
			// image = descriptor.createImage();
			// fImages.put(element, image);
			// }
			// }
			// return image;
			return null;
		}

		public String getColumnText(final Object element, final int columnIndex) {
			if (element instanceof IProject) {
				final IProject project = (IProject) element;
				return project.getName();
			}
			return "";
		}

		public void addListener(final ILabelProviderListener listener) {
		}

		public void dispose() {
			// final Iterator images = fImages.values().iterator();
			// while (images.hasNext()) {
			// final Image image = (Image)images.next();
			// image.dispose();
			// }
		}

		public boolean isLabelProperty(final Object element,
				final String property) {
			return false;
		}

		public void removeListener(final ILabelProviderListener listener) {
		}
	}

	public void setDefaults(final ILaunchConfigurationWorkingCopy configuration) {
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
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_OTHER_PROJECTS, "");
	}

	public void initializeFrom(final ILaunchConfiguration configuration) {
		projectsTable.setInput(configuration);
		initializeOtherProjectsFrom(configuration);
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
		} catch (final CoreException e) {
			otpPathText
					.setText(IErlangLaunchConfigurationAttributes.DEFAULT_OTP_HOME);
		}
		try {
			fProjText
					.setText(configuration
							.getAttribute(
									IErlangLaunchConfigurationAttributes.ATTR_PROJECT_NAME,
									""));
		} catch (final CoreException e) {
			fProjText.setText("");
		}
		try {
			otpNodeName.setText(configuration.getAttribute(
					IErlangLaunchConfigurationAttributes.ATTR_NODE_NAME, ""));
		} catch (final CoreException e) {
			otpNodeName.setText("");
		}
		try {
			startedNodeCheckbox
					.setSelection(configuration
							.getAttribute(
									IErlangLaunchConfigurationAttributes.ATTR_START_NODE,
									true));
		} catch (final CoreException e) {
			startedNodeCheckbox.setSelection(true);
		}
	}

	/**
	 * @param configuration
	 */
	private void initializeOtherProjectsFrom(
			final ILaunchConfiguration configuration) {
		String otherProjects = "";
		try {
			otherProjects = configuration.getAttribute(
					IErlangLaunchConfigurationAttributes.ATTR_OTHER_PROJECTS,
					"");
		} catch (final CoreException e) {
		}
		final IProject[] projects = BackendUtil.getProjects(otherProjects);
		projectsTable.setCheckedElements(projects);
	}

	public void performApply(final ILaunchConfigurationWorkingCopy configuration) {
		performApplyOtherProjects(configuration);
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
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_NODE_NAME,
				otpNodeName.getText());
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_START_NODE,
				!startedNodeCheckbox.getSelection());
	}

	private void performApplyOtherProjects(
			final ILaunchConfigurationWorkingCopy configuration) {
		final Object[] sel = projectsTable.getCheckedElements();
		final StringBuilder projectNames = new StringBuilder();
		for (final Object o : sel) {
			final IProject p = (IProject) o;
			projectNames.append(p.getName()).append(";");
		}
		if (projectNames.length() > 0) {
			projectNames.setLength(projectNames.length() - 1);
		}
		configuration.setAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_OTHER_PROJECTS,
				projectNames.toString());
	}

	public String getName() {
		return "Erlang node";
	}

	@Override
	public boolean isValid(final ILaunchConfiguration launchConfig) {
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
	protected void createProjectEditor(final Composite parent) {
		// final Font font = parent.getFont();
		final Group group = SWTUtil.createGroup(parent, "Project", 2,
				GridData.FILL_HORIZONTAL);
		// group.setFont(font);
		fProjText = new Text(group, SWT.SINGLE | SWT.BORDER);
		final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		fProjText.setLayoutData(gd);
		// fProjText.setFont(font);
		fProjText.addModifyListener(fBasicModifyListener);
		fProjButton = createPushButton(group, "Browse...", null);
		fProjButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(final SelectionEvent e) {
				handleProjectButtonSelected();
			}

			public void widgetDefaultSelected(final SelectionEvent e) {
				// do nothing
			}
		});
	}

	protected void handleProjectButtonSelected() {
		// TODO Auto-generated method stub
		final IErlProject project = chooseErlProject();
		if (project == null) {
			return;
		}
		final String projectName = project.getName();
		fProjText.setText(projectName);
	}

	private IErlProject chooseErlProject() {
		final ILabelProvider labelProvider = new ErlProjectNameLabelProvider();
		// JavaElementLabelProvider(JavaElementLabelProvider.SHOW_DEFAULT);
		final ElementListSelectionDialog dialog = new ElementListSelectionDialog(
				getShell(), labelProvider);
		dialog.setTitle("Select Project");
		dialog.setMessage("SelectProject");
		final IErlModelManager mm = ErlangCore.getModelManager();
		final IErlModel m = mm.getErlangModel();
		IErlElement[] projects;
		try {
			projects = m.getChildren().toArray(new IErlElement[0]);
		} catch (final ErlModelException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
		dialog.setElements(projects);
		final IErlProject erlProject = getErlProject();
		if (erlProject != null) {
			dialog.setInitialSelections(new Object[] { erlProject });
		}
		if (dialog.open() == Window.OK) {
			return (IErlProject) dialog.getFirstResult();
		}
		return null;
	}

	private IErlProject getErlProject() {
		final String projectName = fProjText.getText().trim();
		if (projectName.length() == 0) {
			return null;
		}
		final IErlModel m = ErlangCore.getModelManager().getErlangModel();
		return m.getErlangProject(projectName);
	}

	public static class ErlProjectNameLabelProvider implements ILabelProvider {

		public Image getImage(final Object element) {
			return null;
		}

		public String getText(final Object element) {
			return ((IErlProject) element).getName();
		}

		public void addListener(final ILabelProviderListener listener) {
		}

		public void dispose() {
		}

		public boolean isLabelProperty(final Object element,
				final String property) {
			return false;
		}

		public void removeListener(final ILabelProviderListener listener) {
		}

	}
}
