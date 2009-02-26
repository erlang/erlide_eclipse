/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Sebastian Davids: sdavids@gmx.de bug: 26293, 27889 
 *     David Saff (saff@mit.edu) - bug 102632: [JUnit] Support for JUnit 4.
 *******************************************************************************/
package org.erlide.gunit.launcher;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;

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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;

import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.dialogs.SelectionDialog;

import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;

import org.erlide.gunit.internal.Messages;
import org.erlide.gunit.internal.launcher.ITestKind;
import org.erlide.gunit.internal.launcher.GUnitLaunchConfigurationConstants;
import org.erlide.gunit.internal.launcher.TestKind;
import org.erlide.gunit.internal.launcher.TestKindRegistry;
import org.erlide.gunit.internal.launcher.TestSelectionDialog;
import org.erlide.gunit.internal.ui.IGUnitHelpContextIds;
import org.erlide.gunit.internal.ui.JUnitMessages;
import org.erlide.gunit.internal.ui.GUnitPlugin;
import org.erlide.gunit.internal.util.LayoutUtil;
import org.erlide.gunit.internal.util.TestSearchEngine;

/**
 * The launch configuration tab for JUnit.
 * <p>
 * This class may be instantiated but is not intended to be subclassed.
 * </p>
 * 
 * @since 3.3
 */
public class JUnitLaunchConfigurationTab extends AbstractLaunchConfigurationTab {

	// Project UI widgets
	private Label fProjLabel;
	private Text fProjText;
	private Button fProjButton;
	private Button fKeepRunning;

	// Test class UI widgets
	private Text fTestText;
	private Button fSearchButton;
	private final Image fTestIcon = createImage("obj16/test.gif"); //$NON-NLS-1$
	private String fOriginalTestMethodName;
	private Label fTestMethodLabel;
	private Text fContainerText;
	private IJavaElement fContainerElement;
	private final ILabelProvider fJavaElementLabelProvider = new JavaElementLabelProvider();

	private Button fContainerSearchButton;
	private Button fTestContainerRadioButton;
	private Button fTestRadioButton;
	private Label fTestLabel;

	private ComboViewer fTestLoaderViewer;

	/**
	 * Creates a JUnit launch configuration tab.
	 */
	public JUnitLaunchConfigurationTab() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchConfigurationTab#createControl(org.eclipse
	 * .swt.widgets.Composite)
	 */
	public void createControl(Composite parent) {
		Composite comp = new Composite(parent, SWT.NONE);
		setControl(comp);

		GridLayout topLayout = new GridLayout();
		topLayout.numColumns = 3;
		comp.setLayout(topLayout);

		createSingleTestSection(comp);
		createTestContainerSelectionGroup(comp);

		createSpacer(comp);

		createTestLoaderGroup(comp);

		createSpacer(comp);

		createKeepAliveGroup(comp);
		Dialog.applyDialogFont(comp);
		PlatformUI
				.getWorkbench()
				.getHelpSystem()
				.setHelp(
						getControl(),
						IGUnitHelpContextIds.LAUNCH_CONFIGURATION_DIALOG_JUNIT_MAIN_TAB);
		validatePage();
	}

	private void createTestLoaderGroup(Composite comp) {
		Label loaderLabel = new Label(comp, SWT.NONE);
		loaderLabel
				.setText(JUnitMessages.JUnitLaunchConfigurationTab_Test_Loader);
		GridData gd = new GridData();
		gd.horizontalIndent = 0;
		loaderLabel.setLayoutData(gd);

		fTestLoaderViewer = new ComboViewer(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
		fTestLoaderViewer.getCombo().setLayoutData(
				new GridData(GridData.FILL_HORIZONTAL));

		ArrayList/* <TestKind> */items = TestKindRegistry.getDefault()
				.getAllKinds();
		fTestLoaderViewer.setContentProvider(new ArrayContentProvider());
		fTestLoaderViewer.setLabelProvider(new LabelProvider() {
			public String getText(Object element) {
				return ((TestKind) element).getDisplayName();
			}
		});
		fTestLoaderViewer.setInput(items);
		fTestLoaderViewer
				.addSelectionChangedListener(new ISelectionChangedListener() {
					public void selectionChanged(SelectionChangedEvent event) {
						validatePage();
						updateLaunchConfigurationDialog();
					}
				});
	}

	private void createSpacer(Composite comp) {
		Label label = new Label(comp, SWT.NONE);
		GridData gd = new GridData();
		gd.horizontalSpan = 3;
		label.setLayoutData(gd);
	}

	private void createSingleTestSection(Composite comp) {
		fTestRadioButton = new Button(comp, SWT.RADIO);
		fTestRadioButton
				.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_oneTest);
		GridData gd = new GridData();
		gd.horizontalSpan = 3;
		fTestRadioButton.setLayoutData(gd);
		fTestRadioButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				if (fTestRadioButton.getSelection())
					testModeChanged();
			}
		});

		fProjLabel = new Label(comp, SWT.NONE);
		fProjLabel
				.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_project);
		gd = new GridData();
		gd.horizontalIndent = 25;
		fProjLabel.setLayoutData(gd);

		fProjText = new Text(comp, SWT.SINGLE | SWT.BORDER);
		fProjText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fProjText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				validatePage();
				updateLaunchConfigurationDialog();
				fSearchButton.setEnabled(fTestRadioButton.getSelection()
						&& fProjText.getText().length() > 0);
			}
		});

		fProjButton = new Button(comp, SWT.PUSH);
		fProjButton
				.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_browse);
		fProjButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent evt) {
				handleProjectButtonSelected();
			}
		});
		setButtonGridData(fProjButton);

		fTestLabel = new Label(comp, SWT.NONE);
		gd = new GridData();
		gd.horizontalIndent = 25;
		fTestLabel.setLayoutData(gd);
		fTestLabel
				.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_test);

		fTestText = new Text(comp, SWT.SINGLE | SWT.BORDER);
		fTestText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fTestText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				validatePage();
				updateLaunchConfigurationDialog();
			}
		});

		fSearchButton = new Button(comp, SWT.PUSH);
		fSearchButton.setEnabled(fProjText.getText().length() > 0);
		fSearchButton
				.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_search);
		fSearchButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent evt) {
				handleSearchButtonSelected();
			}
		});
		setButtonGridData(fSearchButton);

		new Label(comp, SWT.NONE);

		fTestMethodLabel = new Label(comp, SWT.NONE);
		fTestMethodLabel.setText(""); //$NON-NLS-1$
		gd = new GridData();
		gd.horizontalSpan = 2;
		fTestMethodLabel.setLayoutData(gd);

	}

	private void createTestContainerSelectionGroup(Composite comp) {
		fTestContainerRadioButton = new Button(comp, SWT.RADIO);
		fTestContainerRadioButton
				.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_containerTest);
		GridData gd = new GridData();
		gd.horizontalSpan = 3;
		fTestContainerRadioButton.setLayoutData(gd);
		fTestContainerRadioButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				if (fTestContainerRadioButton.getSelection())
					testModeChanged();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});

		fContainerText = new Text(comp, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalIndent = 25;
		gd.horizontalSpan = 2;
		fContainerText.setLayoutData(gd);
		fContainerText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		fContainerSearchButton = new Button(comp, SWT.PUSH);
		fContainerSearchButton
				.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_search);
		fContainerSearchButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent evt) {
				handleContainerSearchButtonSelected();
			}
		});
		setButtonGridData(fContainerSearchButton);
	}

	private void handleContainerSearchButtonSelected() {
		IJavaElement javaElement = chooseContainer(fContainerElement);
		if (javaElement != null)
			setContainerElement(javaElement);
	}

	private void setContainerElement(IJavaElement javaElement) {
		fContainerElement = javaElement;
		fContainerText.setText(getPresentationName(javaElement));
		validatePage();
		updateLaunchConfigurationDialog();
	}

	private void createKeepAliveGroup(Composite comp) {
		GridData gd;
		fKeepRunning = new Button(comp, SWT.CHECK);
		fKeepRunning.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				updateLaunchConfigurationDialog();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		fKeepRunning
				.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_keeprunning);
		gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.horizontalSpan = 2;
		fKeepRunning.setLayoutData(gd);
	}

	private static Image createImage(String path) {
		return GUnitPlugin.getImageDescriptor(path).createImage();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchConfigurationTab#initializeFrom(org.eclipse
	 * .debug.core.ILaunchConfiguration)
	 */
	public void initializeFrom(ILaunchConfiguration config) {
		updateProjectFromConfig(config);
		String containerHandle = ""; //$NON-NLS-1$
		try {
			containerHandle = config.getAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
		} catch (CoreException ce) {
		}

		if (containerHandle.length() > 0)
			updateTestContainerFromConfig(config);
		else
			updateTestTypeFromConfig(config);
		updateKeepRunning(config);
		updateTestLoaderFromConfig(config);
	}

	private void updateTestLoaderFromConfig(ILaunchConfiguration config) {
		ITestKind testKind = GUnitLaunchConfigurationConstants
				.getTestRunnerKind(config);
		if (testKind.isNull())
			testKind = TestKindRegistry.getDefault().getKind(
					TestKindRegistry.JUNIT3_TEST_KIND_ID);
		fTestLoaderViewer.setSelection(new StructuredSelection(testKind));
	}

	private TestKind getSelectedTestKind() {
		IStructuredSelection selection = (IStructuredSelection) fTestLoaderViewer
				.getSelection();
		return (TestKind) selection.getFirstElement();
	}

	private void updateKeepRunning(ILaunchConfiguration config) {
		boolean running = false;
		try {
			running = config.getAttribute(
					GUnitLaunchConfigurationConstants.ATTR_KEEPRUNNING, false);
		} catch (CoreException ce) {
		}
		fKeepRunning.setSelection(running);
	}

	private void updateProjectFromConfig(ILaunchConfiguration config) {
		String projectName = ""; //$NON-NLS-1$
		try {
			projectName = config.getAttribute(
					IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""); //$NON-NLS-1$
		} catch (CoreException ce) {
		}
		fProjText.setText(projectName);
	}

	private void updateTestTypeFromConfig(ILaunchConfiguration config) {
		String testTypeName = ""; //$NON-NLS-1$
		fOriginalTestMethodName = ""; //$NON-NLS-1$
		try {
			testTypeName = config.getAttribute(
					IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, ""); //$NON-NLS-1$
			fOriginalTestMethodName = config
					.getAttribute(
							GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME,
							""); //$NON-NLS-1$
		} catch (CoreException ce) {
		}
		fTestRadioButton.setSelection(true);
		setEnableSingleTestGroup(true);
		setEnableContainerTestGroup(false);
		fTestContainerRadioButton.setSelection(false);
		fTestText.setText(testTypeName);
		fContainerText.setText(""); //$NON-NLS-1$
		setTestMethodLabel(fOriginalTestMethodName);
	}

	private void setTestMethodLabel(String testMethodName) {
		if (!"".equals(testMethodName)) { //$NON-NLS-1$
			fTestMethodLabel
					.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_method
							+ fOriginalTestMethodName);
		} else {
			fTestMethodLabel.setText(""); //$NON-NLS-1$
		}
	}

	private void updateTestContainerFromConfig(ILaunchConfiguration config) {
		String containerHandle = ""; //$NON-NLS-1$
		IJavaElement containerElement = null;
		try {
			containerHandle = config.getAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
			if (containerHandle.length() > 0) {
				containerElement = JavaCore.create(containerHandle);
			}
		} catch (CoreException ce) {
		}
		if (containerElement != null)
			fContainerElement = containerElement;
		fTestContainerRadioButton.setSelection(true);
		setEnableSingleTestGroup(false);
		setEnableContainerTestGroup(true);
		fTestRadioButton.setSelection(false);
		if (fContainerElement != null)
			fContainerText.setText(getPresentationName(fContainerElement));
		fTestText.setText(""); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchConfigurationTab#performApply(org.eclipse
	 * .debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void performApply(ILaunchConfigurationWorkingCopy config) {
		if (fTestContainerRadioButton.getSelection()
				&& fContainerElement != null) {
			config.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
					fContainerElement.getJavaProject().getElementName());
			config.setAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER,
					fContainerElement.getHandleIdentifier());
			config.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, ""); //$NON-NLS-1$
			// workaround for bug 65399
			config
					.setAttribute(
							GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME,
							""); //$NON-NLS-1$
		} else {
			config.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
					fProjText.getText());
			config.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
					fTestText.getText());
			config.setAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
			config.setAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME,
					fOriginalTestMethodName);
		}
		config.setAttribute(GUnitLaunchConfigurationConstants.ATTR_KEEPRUNNING,
				fKeepRunning.getSelection());
		try {
			mapResources(config);
		} catch (CoreException e) {
			GUnitPlugin.log(e.getStatus());
		}
		IStructuredSelection testKindSelection = (IStructuredSelection) fTestLoaderViewer
				.getSelection();
		if (!testKindSelection.isEmpty()) {
			TestKind testKind = (TestKind) testKindSelection.getFirstElement();
			config.setAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_RUNNER_KIND,
					testKind.getId());
		}
	}

	private void mapResources(ILaunchConfigurationWorkingCopy config)
			throws CoreException {
		JUnitMigrationDelegate.mapResources(config);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#dispose()
	 */
	public void dispose() {
		super.dispose();
		fTestIcon.dispose();
		fJavaElementLabelProvider.dispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#getImage()
	 */
	public Image getImage() {
		return fTestIcon;
	}

	/*
	 * Show a dialog that lists all main types
	 */
	private void handleSearchButtonSelected() {
		Shell shell = getShell();

		IJavaProject javaProject = getJavaProject();

		IType[] types = new IType[0];
		boolean[] radioSetting = new boolean[2];
		try {
			// fix for 66922 Wrong radio behaviour when switching
			// remember the selected radio button
			radioSetting[0] = fTestRadioButton.getSelection();
			radioSetting[1] = fTestContainerRadioButton.getSelection();

			types = TestSearchEngine.findTests(getLaunchConfigurationDialog(),
					javaProject, getSelectedTestKind());
		} catch (InterruptedException e) {
			setErrorMessage(e.getMessage());
			return;
		} catch (InvocationTargetException e) {
			GUnitPlugin.log(e.getTargetException());
			return;
		} finally {
			fTestRadioButton.setSelection(radioSetting[0]);
			fTestContainerRadioButton.setSelection(radioSetting[1]);
		}

		SelectionDialog dialog = new TestSelectionDialog(shell, types);
		dialog
				.setTitle(JUnitMessages.JUnitLaunchConfigurationTab_testdialog_title);
		dialog
				.setMessage(JUnitMessages.JUnitLaunchConfigurationTab_testdialog_message);
		if (dialog.open() == Window.CANCEL) {
			return;
		}

		Object[] results = dialog.getResult();
		if ((results == null) || (results.length < 1)) {
			return;
		}
		IType type = (IType) results[0];

		if (type != null) {
			fTestText.setText(type.getFullyQualifiedName('.'));
			javaProject = type.getJavaProject();
			fProjText.setText(javaProject.getElementName());
		}
	}

	/*
	 * Show a dialog that lets the user select a project. This in turn provides
	 * context for the main type, allowing the user to key a main type name, or
	 * constraining the search for main types to the specified project.
	 */
	private void handleProjectButtonSelected() {
		IJavaProject project = chooseJavaProject();
		if (project == null) {
			return;
		}

		String projectName = project.getElementName();
		fProjText.setText(projectName);
	}

	/*
	 * Realize a Java Project selection dialog and return the first selected
	 * project, or null if there was none.
	 */
	private IJavaProject chooseJavaProject() {
		IJavaProject[] projects;
		try {
			projects = JavaCore.create(getWorkspaceRoot()).getJavaProjects();
		} catch (JavaModelException e) {
			GUnitPlugin.log(e.getStatus());
			projects = new IJavaProject[0];
		}

		ILabelProvider labelProvider = new JavaElementLabelProvider(
				JavaElementLabelProvider.SHOW_DEFAULT);
		ElementListSelectionDialog dialog = new ElementListSelectionDialog(
				getShell(), labelProvider);
		dialog
				.setTitle(JUnitMessages.JUnitLaunchConfigurationTab_projectdialog_title);
		dialog
				.setMessage(JUnitMessages.JUnitLaunchConfigurationTab_projectdialog_message);
		dialog.setElements(projects);

		IJavaProject javaProject = getJavaProject();
		if (javaProject != null) {
			dialog.setInitialSelections(new Object[] { javaProject });
		}
		if (dialog.open() == Window.OK) {
			return (IJavaProject) dialog.getFirstResult();
		}
		return null;
	}

	/*
	 * Return the IJavaProject corresponding to the project name in the project
	 * name text field, or null if the text does not match a project name.
	 */
	private IJavaProject getJavaProject() {
		String projectName = fProjText.getText().trim();
		if (projectName.length() < 1) {
			return null;
		}
		return getJavaModel().getErlProject(projectName);
	}

	/*
	 * Convenience method to get the workspace root.
	 */
	private IWorkspaceRoot getWorkspaceRoot() {
		return ResourcesPlugin.getWorkspace().getRoot();
	}

	/*
	 * Convenience method to get access to the java model.
	 */
	private IJavaModel getJavaModel() {
		return JavaCore.create(getWorkspaceRoot());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.AbstractLaunchConfigurationTab#isValid(org.eclipse
	 * .debug.core.ILaunchConfiguration)
	 */
	public boolean isValid(ILaunchConfiguration config) {
		return getErrorMessage() == null;
	}

	private void testModeChanged() {
		boolean isSingleTestMode = fTestRadioButton.getSelection();
		setEnableSingleTestGroup(isSingleTestMode);
		setEnableContainerTestGroup(!isSingleTestMode);
		if (!isSingleTestMode && fContainerText.getText().length() == 0) {
			String projText = fProjText.getText();
			if (Path.EMPTY.isValidSegment(projText)) {
				IJavaProject javaProject = getJavaModel().getErlProject(
						projText);
				if (javaProject != null && javaProject.exists())
					setContainerElement(javaProject);
			}
		}
		validatePage();
		updateLaunchConfigurationDialog();
	}

	private void validatePage() {
		setErrorMessage(null);
		setMessage(null);

		if (fTestContainerRadioButton.getSelection()) {
			if (fContainerElement == null) {
				setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_noContainer);
				return;
			}
			validateJavaProject(fContainerElement.getJavaProject());
			return;
		}

		String projectName = fProjText.getText().trim();
		if (projectName.length() == 0) {
			setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_projectnotdefined);
			return;
		}

		IStatus status = ResourcesPlugin.getWorkspace().validatePath(
				IPath.SEPARATOR + projectName, IResource.PROJECT);
		if (!status.isOK() || !Path.ROOT.isValidSegment(projectName)) {
			setErrorMessage(Messages
					.format(
							JUnitMessages.JUnitLaunchConfigurationTab_error_invalidProjectName,
							projectName));
			return;
		}

		IProject project = getWorkspaceRoot().getProject(projectName);
		if (!project.exists()) {
			setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_projectnotexists);
			return;
		}
		IJavaProject javaProject = JavaCore.create(project);
		validateJavaProject(javaProject);

		try {
			if (!project.hasNature(JavaCore.NATURE_ID)) {
				setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_notJavaProject);
				return;
			}
			String className = fTestText.getText().trim();
			if (className.length() == 0) {
				setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_testnotdefined);
				return;
			}
			if (javaProject.findType(className) == null) {
				setErrorMessage(Messages
						.format(
								JUnitMessages.JUnitLaunchConfigurationTab_error_test_class_not_found,
								new String[] { className, projectName }));
				return;
			}

		} catch (CoreException e) {
			GUnitPlugin.log(e);
		}

	}

	private void validateJavaProject(IJavaProject javaProject) {
		if (!TestSearchEngine.hasTestCaseType(javaProject)) {
			setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_testcasenotonpath);
			return;
		}
	}

	private void setEnableContainerTestGroup(boolean enabled) {
		fContainerSearchButton.setEnabled(enabled);
		fContainerText.setEnabled(enabled);
	}

	private void setEnableSingleTestGroup(boolean enabled) {
		fProjLabel.setEnabled(enabled);
		fProjText.setEnabled(enabled);
		fProjButton.setEnabled(enabled);
		fTestLabel.setEnabled(enabled);
		fTestText.setEnabled(enabled);
		fSearchButton.setEnabled(enabled && fProjText.getText().length() > 0);
		fTestMethodLabel.setEnabled(enabled);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchConfigurationTab#setDefaults(org.eclipse.
	 * debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		IJavaElement javaElement = getContext();
		if (javaElement != null) {
			initializeJavaProject(javaElement, config);
		} else {
			// We set empty attributes for project & main type so that when one
			// config is
			// compared to another, the existence of empty attributes doesn't
			// cause an
			// incorrect result (the performApply() method can result in empty
			// values
			// for these attributes being set on a config if there is nothing in
			// the
			// corresponding text boxes)
			config.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""); //$NON-NLS-1$
			config.setAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
		}
		initializeTestAttributes(javaElement, config);
	}

	private void initializeTestAttributes(IJavaElement javaElement,
			ILaunchConfigurationWorkingCopy config) {
		if (javaElement != null
				&& javaElement.getElementType() < IJavaElement.COMPILATION_UNIT)
			initializeTestContainer(javaElement, config);
		else
			initializeTestType(javaElement, config);
	}

	private void initializeTestContainer(IJavaElement javaElement,
			ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(
				GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER,
				javaElement.getHandleIdentifier());
		initializeName(config, javaElement.getElementName());
	}

	private void initializeName(ILaunchConfigurationWorkingCopy config,
			String name) {
		if (name == null) {
			name = ""; //$NON-NLS-1$
		}
		if (name.length() > 0) {
			int index = name.lastIndexOf('.');
			if (index > 0) {
				name = name.substring(index + 1);
			}
			name = getLaunchConfigurationDialog().generateName(name);
			config.rename(name);
		}
	}

	/*
	 * Set the main type & name attributes on the working copy based on the
	 * IJavaElement
	 */
	private void initializeTestType(IJavaElement javaElement,
			ILaunchConfigurationWorkingCopy config) {
		String name = ""; //$NON-NLS-1$
		String testKindId = null;
		try {
			// we only do a search for compilation units or class files or
			// or source references
			if (javaElement instanceof ISourceReference) {
				ITestKind testKind = TestKindRegistry
						.getContainerTestKind(javaElement);
				testKindId = testKind.getId();

				IType[] types = TestSearchEngine.findTests(
						getLaunchConfigurationDialog(), javaElement, testKind);
				if ((types == null) || (types.length < 1)) {
					return;
				}
				// Simply grab the first main type found in the searched element
				name = JavaModelUtil.getFullyQualifiedName(types[0]);

			}
		} catch (InterruptedException ie) {

		} catch (InvocationTargetException ite) {
		}
		config.setAttribute(
				IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, name);
		if (testKindId != null)
			config.setAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_RUNNER_KIND,
					testKindId);
		initializeName(config, name);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#getName()
	 */
	public String getName() {
		return JUnitMessages.JUnitLaunchConfigurationTab_tab_label;
	}

	private IJavaElement chooseContainer(IJavaElement initElement) {
		Class[] acceptedClasses = new Class[] { IPackageFragmentRoot.class,
				IJavaProject.class, IPackageFragment.class };
		TypedElementSelectionValidator validator = new TypedElementSelectionValidator(
				acceptedClasses, false) {
			public boolean isSelectedValid(Object element) {
				return true;
			}
		};

		acceptedClasses = new Class[] { IJavaModel.class,
				IPackageFragmentRoot.class, IJavaProject.class,
				IPackageFragment.class };
		ViewerFilter filter = new TypedViewerFilter(acceptedClasses) {
			public boolean select(Viewer viewer, Object parent, Object element) {
				if (element instanceof IPackageFragmentRoot
						&& ((IPackageFragmentRoot) element).isArchive())
					return false;
				return super.select(viewer, parent, element);
			}
		};

		StandardJavaElementContentProvider provider = new StandardJavaElementContentProvider();
		ILabelProvider labelProvider = new JavaElementLabelProvider(
				JavaElementLabelProvider.SHOW_DEFAULT);
		ElementTreeSelectionDialog dialog = new ElementTreeSelectionDialog(
				getShell(), labelProvider, provider);
		dialog.setValidator(validator);
		dialog.setComparator(new JavaElementComparator());
		dialog
				.setTitle(JUnitMessages.JUnitLaunchConfigurationTab_folderdialog_title);
		dialog
				.setMessage(JUnitMessages.JUnitLaunchConfigurationTab_folderdialog_message);
		dialog.addFilter(filter);
		dialog.setInput(JavaCore.create(getWorkspaceRoot()));
		dialog.setInitialSelection(initElement);
		dialog.setAllowMultiple(false);

		if (dialog.open() == Window.OK) {
			Object element = dialog.getFirstResult();
			return (IJavaElement) element;
		}
		return null;
	}

	private String getPresentationName(IJavaElement element) {
		return fJavaElementLabelProvider.getText(element);
	}

	/*
	 * Returns the current Java element context from which to initialize default
	 * settings, or <code>null</code> if none.
	 * 
	 * @return Java element context.
	 */
	private IJavaElement getContext() {
		IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
			return null;
		}
		IWorkbenchPage page = activeWorkbenchWindow.getActivePage();
		if (page != null) {
			ISelection selection = page.getSelection();
			if (selection instanceof IStructuredSelection) {
				IStructuredSelection ss = (IStructuredSelection) selection;
				if (!ss.isEmpty()) {
					Object obj = ss.getFirstElement();
					if (obj instanceof IJavaElement) {
						return (IJavaElement) obj;
					}
					if (obj instanceof IResource) {
						IJavaElement je = JavaCore.create((IResource) obj);
						if (je == null) {
							IProject pro = ((IResource) obj).getProject();
							je = JavaCore.create(pro);
						}
						if (je != null) {
							return je;
						}
					}
				}
			}
			IEditorPart part = page.getActiveEditor();
			if (part != null) {
				IEditorInput input = part.getEditorInput();
				return (IJavaElement) input.getAdapter(IJavaElement.class);
			}
		}
		return null;
	}

	private void initializeJavaProject(IJavaElement javaElement,
			ILaunchConfigurationWorkingCopy config) {
		IJavaProject javaProject = javaElement.getJavaProject();
		String name = null;
		if (javaProject != null && javaProject.exists()) {
			name = javaProject.getElementName();
		}
		config.setAttribute(
				IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, name);
	}

	private void setButtonGridData(Button button) {
		GridData gridData = new GridData();
		button.setLayoutData(gridData);
		LayoutUtil.setButtonDimensionHint(button);
	}
}
