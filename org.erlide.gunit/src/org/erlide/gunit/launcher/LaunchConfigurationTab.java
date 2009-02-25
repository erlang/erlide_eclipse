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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;

/**
 * The launch configuration tab for JUnit.
 * <p>
 * This class may be instantiated but is not intended to be subclassed.
 * </p>
 * 
 * @since 3.3
 */
@SuppressWarnings("unused")
public class LaunchConfigurationTab extends AbstractLaunchConfigurationTab {

	// Project UI widgets
	private Label fProjLabel;

	private Text fProjText;

	private Button fProjButton;

	private Label fSuiteLabel;

	private Text fSuiteText;

	Button fSuiteButton;

	// private Button fKeepRunning;

	// Test class UI widgets
	private Label fTestLabel;

	private Text fTestText;

	private Button fTestButton;

	private IFile fSelectedSuite;

	private String fSelectedProjectName;

	// private String fOriginalTestMethodName;
	// private Label fTestMethodLabel;
	// private Text fContainerText;
	// private IJavaElement fContainerElement;
	// private final ILabelProvider fJavaElementLabelProvider= new
	// JavaElementLabelProvider();

	// private Button fContainerSearchButton;
	// private Button fTestContainerRadioButton;
	// private Button fTestRadioButton;

	// private ComboViewer fTestLoaderViewer;

	/**
	 * Creates a JUnit launch configuration tab.
	 */
	public LaunchConfigurationTab() {
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
		// createTestContainerSelectionGroup(comp);

		// createSpacer(comp);

		// createTestLoaderGroup(comp);

		// createSpacer(comp);

		// createKeepAliveGroup(comp);
		// Dialog.applyDialogFont(comp);
		// PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(),
		// IJUnitHelpContextIds.LAUNCH_CONFIGURATION_DIALOG_JUNIT_MAIN_TAB);
		validatePage();
	}

	// private void createTestLoaderGroup(Composite comp) {
	// Label loaderLabel= new Label(comp, SWT.NONE);
	// loaderLabel.setText("Hej1");
	// GridData gd= new GridData();
	// gd.horizontalIndent= 0;
	// loaderLabel.setLayoutData(gd);
	//
	// fTestLoaderViewer= new ComboViewer(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
	// fTestLoaderViewer.getCombo().setLayoutData(new
	// GridData(GridData.FILL_HORIZONTAL));
	//
	// ArrayList/*<TestKind>*/ items=
	// TestKindRegistry.getDefault().getAllKinds();
	// fTestLoaderViewer.setContentProvider(new ArrayContentProvider());
	// fTestLoaderViewer.setLabelProvider(new LabelProvider() {
	// public String getText(Object element) {
	// return ((TestKind) element).getDisplayName();
	// }
	// });
	// fTestLoaderViewer.setInput(items);
	// fTestLoaderViewer.addSelectionChangedListener(new
	// ISelectionChangedListener() {
	// public void selectionChanged(SelectionChangedEvent event) {
	// validatePage();
	// updateLaunchConfigurationDialog();
	// }
	// });
	// }

	private void createSpacer(Composite comp) {
		Label label = new Label(comp, SWT.NONE);
		GridData gd = new GridData();
		gd.horizontalSpan = 3;
		label.setLayoutData(gd);
	}

	private void createSingleTestSection(Composite comp) {
		fProjLabel = new Label(comp, SWT.NONE);
		fProjLabel.setText("Project:");

		fProjText = new Text(comp, SWT.SINGLE | SWT.BORDER);
		fProjText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fProjText.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			public void modifyText(ModifyEvent evt) {
				validatePage();
				updateLaunchConfigurationDialog();
				fSuiteButton.setEnabled(getProject() != null);
			}
		});

		fProjButton = new Button(comp, SWT.PUSH);
		fProjButton.setText("Browse...");
		fProjButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				handleProjectButtonSelected();
			}
		});

		fSuiteLabel = new Label(comp, SWT.NONE);
		fSuiteLabel.setText("Test Suite:");

		fSuiteText = new Text(comp, SWT.SINGLE | SWT.BORDER);
		fSuiteText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fSuiteText.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			public void modifyText(ModifyEvent evt) {
				validatePage();
				updateLaunchConfigurationDialog();
				// fTestButton.setEnabled(fTestRadioButton.getSelection() &&
				// fProjText.getText().length() > 0);
			}
		});

		fSuiteButton = new Button(comp, SWT.PUSH);
		fSuiteButton.setText("Browse...");
		fSuiteButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				handleSuiteButtonSelected();
			}
		});

		fTestLabel = new Label(comp, SWT.NONE);
		fTestLabel.setText("Test Case:");

		fTestText = new Text(comp, SWT.SINGLE | SWT.BORDER);
		fTestText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fTestText.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			public void modifyText(ModifyEvent evt) {
				validatePage();
				updateLaunchConfigurationDialog();
				// fTestButton.setEnabled(fTestRadioButton.getSelection() &&
				// fProjText.getText().length() > 0);
			}
		});

		fTestButton = new Button(comp, SWT.PUSH);
		fTestButton.setEnabled(fProjText.getText().length() > 0);
		fTestButton.setText("Browse...");
		fTestButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				handleTestButtonSelected();
			}
		});

		fTestLabel.setEnabled(false);
		fTestText.setEnabled(false);
		fTestButton.setEnabled(false);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchConfigurationTab#initializeFrom(org.eclipse
	 * .debug.core.ILaunchConfiguration)
	 */
	public void initializeFrom(ILaunchConfiguration config) {
		// updateProjectFromConfig(config);
		//		String containerHandle= ""; //$NON-NLS-1$
		// try {
		//			containerHandle = config.getAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
		// } catch (CoreException ce) {
		// }
		//
		// if (containerHandle.length() > 0)
		// updateTestContainerFromConfig(config);
		// else
		// updateTestTypeFromConfig(config);
		// updateKeepRunning(config);
		// updateTestLoaderFromConfig(config);
	}

	private void updateTestLoaderFromConfig(ILaunchConfiguration config) {
		// ITestKind testKind=
		// JUnitLaunchConfigurationConstants.getTestRunnerKind(config);
		// if (testKind.isNull())
		// testKind=
		// TestKindRegistry.getDefault().getKind(TestKindRegistry.JUNIT3_TEST_KIND_ID);
		// fTestLoaderViewer.setSelection(new StructuredSelection(testKind));
	}

	// private TestKind getSelectedTestKind() {
	// // IStructuredSelection selection= (IStructuredSelection)
	// fTestLoaderViewer.getSelection();
	// // return (TestKind) selection.getFirstElement();
	// }

	// private void updateKeepRunning(ILaunchConfiguration config) {
	// boolean running= false;
	// try {
	// running=
	// config.getAttribute(JUnitLaunchConfigurationConstants.ATTR_KEEPRUNNING,
	// false);
	// } catch (CoreException ce) {
	// }
	// fKeepRunning.setSelection(running);
	// }

	private void updateProjectFromConfig(ILaunchConfiguration config) {
		//		String projectName= ""; //$NON-NLS-1$
		// try {
		//			projectName = config.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""); //$NON-NLS-1$
		// } catch (CoreException ce) {
		// }
		// fProjText.setText(projectName);
	}

	private void updateTestTypeFromConfig(ILaunchConfiguration config) {
		//		String testTypeName= ""; //$NON-NLS-1$
		//		fOriginalTestMethodName= ""; //$NON-NLS-1$
		// try {
		//			testTypeName = config.getAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, ""); //$NON-NLS-1$
		//			fOriginalTestMethodName = config.getAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME, ""); //$NON-NLS-1$
		// } catch (CoreException ce) {
		// }
		// fTestRadioButton.setSelection(true);
		// setEnableSingleTestGroup(true);
		// setEnableContainerTestGroup(false);
		// fTestContainerRadioButton.setSelection(false);
		// fTestText.setText(testTypeName);
		//		fContainerText.setText(""); //$NON-NLS-1$
		// setTestMethodLabel(fOriginalTestMethodName);
	}

	// private void setTestMethodLabel(String testMethodName) {
	//		if (!"".equals(testMethodName)) { //$NON-NLS-1$
	// fTestMethodLabel.setText(JUnitMessages.JUnitLaunchConfigurationTab_label_method+fOriginalTestMethodName);
	// } else {
	//			fTestMethodLabel.setText(""); //$NON-NLS-1$
	// }
	// }

	private void updateTestContainerFromConfig(ILaunchConfiguration config) {
		//		String containerHandle= ""; //$NON-NLS-1$
		// IJavaElement containerElement = null;
		// try {
		//			containerHandle = config.getAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
		// if (containerHandle.length() > 0) {
		// containerElement= JavaCore.create(containerHandle);
		// }
		// } catch (CoreException ce) {
		// }
		// if (containerElement != null)
		// fContainerElement = containerElement;
		// fTestContainerRadioButton.setSelection(true);
		// setEnableSingleTestGroup(false);
		// setEnableContainerTestGroup(true);
		// fTestRadioButton.setSelection(false);
		// if (fContainerElement != null)
		// fContainerText.setText(getPresentationName(fContainerElement));
		//		fTestText.setText(""); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchConfigurationTab#performApply(org.eclipse
	 * .debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void performApply(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(LaunchConfigurationConstants.ATTR_PROJECT_NAME,
				fSelectedProjectName);
		config.setAttribute(LaunchConfigurationConstants.ATTR_TEST_SUITE_NAME,
				fSuiteText.getText());

		// if (fTestContainerRadioButton.getSelection() && fContainerElement !=
		// null) {
		// config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
		// fContainerElement.getJavaProject().getElementName());
		// config.setAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER,
		// fContainerElement.getHandleIdentifier());
		//			config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, ""); //$NON-NLS-1$
		// //workaround for bug 65399
		//			config.setAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME, ""); //$NON-NLS-1$
		// } else {
		// config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
		// fProjText.getText());
		// config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
		// fTestText.getText());
		//			config.setAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
		// config.setAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME,
		// fOriginalTestMethodName);
		// }
		// config.setAttribute(JUnitLaunchConfigurationConstants.ATTR_KEEPRUNNING,
		// fKeepRunning.getSelection());
		// try {
		// mapResources(config);
		// } catch (CoreException e) {
		// JUnitPlugin.log(e.getStatus());
		// }
		// IStructuredSelection testKindSelection= (IStructuredSelection)
		// fTestLoaderViewer.getSelection();
		// if (! testKindSelection.isEmpty()) {
		// TestKind testKind= (TestKind) testKindSelection.getFirstElement();
		// config.setAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_RUNNER_KIND,
		// testKind.getId());
		// }
	}

	private void mapResources(ILaunchConfigurationWorkingCopy config)
			throws CoreException {
		// JUnitMigrationDelegate.mapResources(config);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#dispose()
	 */
	@Override
	public void dispose() {
		super.dispose();
		// fJavaElementLabelProvider.dispose();
	}

	/*
	 * Show a dialog that lists all main types
	 */
	void handleTestButtonSelected() {
		// Shell shell = getShell();
		//
		// IJavaProject javaProject = getJavaProject();
		//
		// IType[] types= new IType[0];
		// boolean[] radioSetting= new boolean[2];
		// try {
		// // fix for 66922 Wrong radio behaviour when switching
		// // remember the selected radio button
		// radioSetting[0]= fTestRadioButton.getSelection();
		// radioSetting[1]= fTestContainerRadioButton.getSelection();
		//
		// types= TestSearchEngine.findTests(getLaunchConfigurationDialog(),
		// javaProject, getSelectedTestKind());
		// } catch (InterruptedException e) {
		// setErrorMessage(e.getMessage());
		// return;
		// } catch (InvocationTargetException e) {
		// JUnitPlugin.log(e.getTargetException());
		// return;
		// } finally {
		// fTestRadioButton.setSelection(radioSetting[0]);
		// fTestContainerRadioButton.setSelection(radioSetting[1]);
		// }
		//
		// SelectionDialog dialog = new TestSelectionDialog(shell, types);
		// dialog.setTitle(JUnitMessages.JUnitLaunchConfigurationTab_testdialog_title);
		// dialog.setMessage(JUnitMessages.JUnitLaunchConfigurationTab_testdialog_message);
		// if (dialog.open() == Window.CANCEL) {
		// return;
		// }
		//
		// Object[] results = dialog.getResult();
		// if ((results == null) || (results.length < 1)) {
		// return;
		// }
		// IType type = (IType)results[0];
		//
		// if (type != null) {
		// fTestText.setText(type.getFullyQualifiedName('.'));
		// javaProject = type.getJavaProject();
		// fProjText.setText(javaProject.getElementName());
		// }
	}

	/*
	 * Show a dialog that lets the user select a project. This in turn provides
	 * context for the main type, allowing the user to key a main type name, or
	 * constraining the search for main types to the specified project.
	 */
	void handleProjectButtonSelected() {
		IProject project = chooseProject();
		if (project == null) {
			return;
		}

		String projectName = "Error";
		try {
			projectName = project.getDescription().getName();
		} catch (CoreException e) {
			e.printStackTrace();
		}
		fProjText.setText(projectName);
	}

	private IProject chooseProject() {
		IProject[] projects;
		projects = getWorkspaceRoot().getProjects();

		List<IProject> openProjects = new ArrayList<IProject>();

		for (int i = 0; i < projects.length; i++) {
			IProject project = projects[i];
			if (project.isOpen()) {
				openProjects.add(project);
			}
		}

		projects = openProjects.toArray(new IProject[0]);

		ILabelProvider labelProvider = new LabelProvider() {

			@Override
			public String getText(Object element) {
				if (element instanceof IProject) {
					try {
						return ((IProject) element).getDescription().getName();
					} catch (CoreException e) {
						e.printStackTrace();
					}
				}
				return element.toString();
			}
		};
		ElementListSelectionDialog dialog = new ElementListSelectionDialog(
				getShell(), labelProvider);
		dialog.setTitle("Project Selection");
		dialog.setMessage("");
		dialog.setElements(projects);

		IProject project = getProject();
		if (project != null) {
			dialog.setInitialSelections(new Object[] { project });
		}
		if (dialog.open() == Window.OK) {
			return (IProject) dialog.getFirstResult();
		}
		return null;
	}

	/*
	 * Return the IProject corresponding to the project name in the project name
	 * text field, or null if the text does not match a project name.
	 */
	IProject getProject() {
		String projectName = fProjText.getText().trim();
		if (projectName.length() < 1) {
			return null;
		}
		IProject project = getWorkspaceRoot().getProject(projectName);
		if (project.exists()) {
			return project;
		} else {
			return null;
		}
	}

	void handleSuiteButtonSelected() {
		IFile suite = chooseSuite();
		if (suite == null) {
			return;
		}
		fSelectedSuite = suite;
		String suiteName = suite.getName();
		suiteName = suiteName.substring(0, suiteName.length() - 4); // FIXME
		// Remove
		// ".erl"
		// from
		// suite
		// name in
		// _one_
		// place
		// only...
		fSuiteText.setText(suiteName);
	}

	private IFile chooseSuite() {
		IFile[] suites = getSuites();

		ILabelProvider labelProvider = new LabelProvider() {

			@Override
			public String getText(Object element) {
				if (element instanceof IFile) {
					String suiteName = ((IFile) element).getName();
					suiteName = suiteName.substring(0, suiteName.length() - 4); // FIXME
					// Remove
					// ".erl"
					// from
					// suite
					// name
					// in
					// _one_
					// place
					// only...
					return suiteName;
				}
				return element.toString();
			}
		};
		ElementListSelectionDialog dialog = new ElementListSelectionDialog(
				getShell(), labelProvider);
		dialog.setTitle("Test Suite Selection");
		dialog.setMessage("");
		dialog.setElements(suites);

		// IProject project = getProject();
		// if (project != null) {
		// dialog.setInitialSelections(new Object[] { project });
		// }
		if (dialog.open() == Window.OK) {
			return (IFile) dialog.getFirstResult();
		}
		return null;

	}

	private IFile[] getSuites() {
		List<IFile> suites = getSuites(getProject());
		return suites.toArray(new IFile[suites.size()]);
	}

	private List<IFile> getSuites(IResource resource) {
		List<IFile> suiteArrayList = new ArrayList<IFile>();

		if (resource instanceof IContainer) {
			IResource[] resources = new IResource[0];
			try {
				resources = ((IContainer) resource).members();
			} catch (CoreException e) {
				e.printStackTrace();
			}
			for (int i = 0; i < resources.length; i++) {
				suiteArrayList.addAll(getSuites(resources[i]));
			}
		} else if (resource instanceof IFile) {
			if (((IFile) resource).getName().endsWith("_SUITE.erl")) {
				suiteArrayList.add((IFile) resource);
			}
		}

		return suiteArrayList;
	}

	// private IFile getSuite() {
	// String suiteName = fSuiteText.getText().trim();
	// if (suiteName.length() < 1) {
	// return null;
	// }
	// IProject project = getWorkspaceRoot()..getProject(projectName);
	// if (project.exists()) {
	// return project;
	// }
	// else {
	// return null;
	// }
	// }
	/*
	 * Convenience method to get the workspace root.
	 */
	private IWorkspaceRoot getWorkspaceRoot() {
		return ResourcesPlugin.getWorkspace().getRoot();
	}

	/*
	 * Convenience method to get access to the java model.
	 */
	// private IJavaModel getJavaModel() {
	// return JavaCore.create(getWorkspaceRoot());
	// }
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.AbstractLaunchConfigurationTab#isValid(org.eclipse
	 * .debug.core.ILaunchConfiguration)
	 */
	@Override
	public boolean isValid(ILaunchConfiguration config) {
		return getErrorMessage() == null;
	}

	void validatePage() {
		setErrorMessage(null);
		setMessage(null);

		IProject project = getProject();
		if (project != null) {
			try {
				fSelectedProjectName = project.getDescription().getName();
			} catch (CoreException e) {
				fSelectedProjectName = null;
				setErrorMessage("Invalid project");
				return;
			}
		} else {
			fSelectedProjectName = null;
			setErrorMessage("Invalid project");
			return;
		}
		//
		// if (fTestContainerRadioButton.getSelection()) {
		// if (fContainerElement == null) {
		// setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_noContainer);
		// return;
		// }
		// validateJavaProject(fContainerElement.getJavaProject());
		// return;
		// }
		//
		// String projectName= fProjText.getText().trim();
		// if (projectName.length() == 0) {
		// setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_projectnotdefined);
		// return;
		// }
		//
		// IStatus status=
		// ResourcesPlugin.getWorkspace().validatePath(IPath.SEPARATOR +
		// projectName, IResource.PROJECT);
		// if (!status.isOK() || !Path.ROOT.isValidSegment(projectName)) {
		// setErrorMessage(Messages.format(JUnitMessages.JUnitLaunchConfigurationTab_error_invalidProjectName,
		// projectName));
		// return;
		// }
		//
		// IProject project= getWorkspaceRoot().getProject(projectName);
		// if (!project.exists()) {
		// setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_projectnotexists);
		// return;
		// }
		// IJavaProject javaProject= JavaCore.create(project);
		// validateJavaProject(javaProject);
		//
		// try {
		// if (!project.hasNature(JavaCore.NATURE_ID)) {
		// setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_notJavaProject);
		// return;
		// }
		// String className= fTestText.getText().trim();
		// if (className.length() == 0) {
		// setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_testnotdefined);
		// return;
		// }
		// if (javaProject.findType(className) == null) {
		// setErrorMessage(Messages.format(JUnitMessages.JUnitLaunchConfigurationTab_error_test_class_not_found,
		// new String[] { className, projectName }));
		// return;
		// }
		//
		//
		// } catch (CoreException e) {
		// JUnitPlugin.log(e);
		// }

	}

	// private void validateJavaProject(IJavaProject javaProject) {
	// // if (! TestSearchEngine.hasTestCaseType(javaProject)) {
	// //
	// setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_testcasenotonpath);
	// // return;
	// // }
	// }

	// private void setEnableContainerTestGroup(boolean enabled) {
	// // fContainerSearchButton.setEnabled(enabled);
	// // fContainerText.setEnabled(enabled);
	// }

	// private void setEnableSingleTestGroup(boolean enabled) {
	// fProjLabel.setEnabled(enabled);
	// fProjText.setEnabled(enabled);
	// fProjButton.setEnabled(enabled);
	// fTestLabel.setEnabled(enabled);
	// fTestText.setEnabled(enabled);
	// fTestButton.setEnabled(enabled && fProjText.getText().length() > 0);
	// // fTestMethodLabel.setEnabled(enabled);
	// }

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchConfigurationTab#setDefaults(org.eclipse.
	 * debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		// IJavaElement javaElement = getContext();
		// if (javaElement != null) {
		// initializeJavaProject(javaElement, config);
		// } else {
		// // We set empty attributes for project & main type so that when one
		// config is
		// // compared to another, the existence of empty attributes doesn't
		// cause an
		// // incorrect result (the performApply() method can result in empty
		// values
		// // for these attributes being set on a config if there is nothing in
		// the
		// // corresponding text boxes)
		//			config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""); //$NON-NLS-1$
		//			config.setAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
		// }
		// initializeTestAttributes(javaElement, config);
	}

	// private void initializeTestAttributes(IJavaElement javaElement,
	// ILaunchConfigurationWorkingCopy config) {
	// if (javaElement != null && javaElement.getElementType() <
	// IJavaElement.COMPILATION_UNIT)
	// initializeTestContainer(javaElement, config);
	// else
	// initializeTestType(javaElement, config);
	// }

	// private void initializeTestContainer(IJavaElement javaElement,
	// ILaunchConfigurationWorkingCopy config) {
	// config.setAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER,
	// javaElement.getHandleIdentifier());
	// initializeName(config, javaElement.getElementName());
	// }

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
	// private void initializeTestType(IJavaElement javaElement,
	// ILaunchConfigurationWorkingCopy config) {
	//		String name= ""; //$NON-NLS-1$
	// String testKindId= null;
	// try {
	// // we only do a search for compilation units or class files or
	// // or source references
	// if (javaElement instanceof ISourceReference) {
	// ITestKind testKind= TestKindRegistry.getContainerTestKind(javaElement);
	// testKindId= testKind.getId();
	//
	// IType[] types =
	// TestSearchEngine.findTests(getLaunchConfigurationDialog(), javaElement,
	// testKind);
	// if ((types == null) || (types.length < 1)) {
	// return;
	// }
	// // Simply grab the first main type found in the searched element
	// name= JavaModelUtil.getFullyQualifiedName(types[0]);
	//
	// }
	// } catch (InterruptedException ie) {
	//
	// } catch (InvocationTargetException ite) {
	// }
	// config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
	// name);
	// if (testKindId != null)
	// config.setAttribute(JUnitLaunchConfigurationConstants.ATTR_TEST_RUNNER_KIND,
	// testKindId);
	// initializeName(config, name);
	// }
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#getName()
	 */
	public String getName() {
		return "Test";
	}

	// private IJavaElement chooseContainer(IJavaElement initElement) {
	// Class[] acceptedClasses= new Class[] { IPackageFragmentRoot.class,
	// IJavaProject.class, IPackageFragment.class };
	// TypedElementSelectionValidator validator= new
	// TypedElementSelectionValidator(acceptedClasses, false) {
	// public boolean isSelectedValid(Object element) {
	// return true;
	// }
	// };
	//
	// acceptedClasses= new Class[] { IJavaModel.class,
	// IPackageFragmentRoot.class, IJavaProject.class, IPackageFragment.class };
	// ViewerFilter filter= new TypedViewerFilter(acceptedClasses) {
	// public boolean select(Viewer viewer, Object parent, Object element) {
	// if (element instanceof IPackageFragmentRoot &&
	// ((IPackageFragmentRoot)element).isArchive())
	// return false;
	// return super.select(viewer, parent, element);
	// }
	// };
	//
	// StandardJavaElementContentProvider provider= new
	// StandardJavaElementContentProvider();
	// ILabelProvider labelProvider= new
	// JavaElementLabelProvider(JavaElementLabelProvider.SHOW_DEFAULT);
	// ElementTreeSelectionDialog dialog= new
	// ElementTreeSelectionDialog(getShell(), labelProvider, provider);
	// dialog.setValidator(validator);
	// dialog.setComparator(new JavaElementComparator());
	// dialog.setTitle(JUnitMessages.JUnitLaunchConfigurationTab_folderdialog_title);
	// dialog.setMessage(JUnitMessages.JUnitLaunchConfigurationTab_folderdialog_message);
	// dialog.addFilter(filter);
	// dialog.setInput(JavaCore.create(getWorkspaceRoot()));
	// dialog.setInitialSelection(initElement);
	// dialog.setAllowMultiple(false);
	//
	// if (dialog.open() == Window.OK) {
	// Object element= dialog.getFirstResult();
	// return (IJavaElement)element;
	// }
	// return null;
	// }

	// private String getPresentationName(IJavaElement element) {
	// return fJavaElementLabelProvider.getText(element);
	// }

	/*
	 * Returns the current Java element context from which to initialize default
	 * settings, or <code>null</code> if none.
	 * 
	 * @return Java element context.
	 */
	// private IJavaElement getContext() {
	// IWorkbenchWindow activeWorkbenchWindow=
	// PlatformUI.getWorkbench().getActiveWorkbenchWindow();
	// if (activeWorkbenchWindow == null) {
	// return null;
	// }
	// IWorkbenchPage page = activeWorkbenchWindow.getActivePage();
	// if (page != null) {
	// ISelection selection = page.getSelection();
	// if (selection instanceof IStructuredSelection) {
	// IStructuredSelection ss = (IStructuredSelection)selection;
	// if (!ss.isEmpty()) {
	// Object obj = ss.getFirstElement();
	// if (obj instanceof IJavaElement) {
	// return (IJavaElement)obj;
	// }
	// if (obj instanceof IResource) {
	// IJavaElement je = JavaCore.create((IResource)obj);
	// if (je == null) {
	// IProject pro = ((IResource)obj).getProject();
	// je = JavaCore.create(pro);
	// }
	// if (je != null) {
	// return je;
	// }
	// }
	// }
	// }
	// IEditorPart part = page.getActiveEditor();
	// if (part != null) {
	// IEditorInput input = part.getEditorInput();
	// return (IJavaElement) input.getAdapter(IJavaElement.class);
	// }
	// }
	// return null;
	// }
	// private void initializeJavaProject(IJavaElement javaElement,
	// ILaunchConfigurationWorkingCopy config) {
	// IJavaProject javaProject = javaElement.getJavaProject();
	// String name = null;
	// if (javaProject != null && javaProject.exists()) {
	// name = javaProject.getElementName();
	// }
	// config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
	// name);
	// }
	// private void setButtonGridData(Button button) {
	// GridData gridData= new GridData();
	// button.setLayoutData(gridData);
	// LayoutUtil.setButtonDimensionHint(button);
	// }
}
