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
package org.erlide.gunit.internal.launcher;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
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
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlProject;
import org.erlide.gunit.internal.Messages;
import org.erlide.gunit.internal.ui.GUnitMessages;
import org.erlide.gunit.internal.ui.GUnitPlugin;
import org.erlide.gunit.internal.ui.IGUnitHelpContextIds;
import org.erlide.gunit.internal.util.LayoutUtil;
import org.erlide.ui.views.outline.ErlangLabelProvider;

/**
 * The launch configuration tab for JUnit.
 * <p>
 * This class may be instantiated but is not intended to be subclassed.
 * </p>
 * 
 * @since 3.3
 */
public class GUnitLaunchConfigurationTab extends AbstractLaunchConfigurationTab {

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

	private IErlElement fContainerElement;

	private final ILabelProvider fErlElementLabelProvider = new ErlangLabelProvider();

	private Button fContainerSearchButton;

	private Button fTestContainerRadioButton;

	private Button fTestRadioButton;

	private Label fTestLabel;

	private ComboViewer fTestLoaderViewer;

	/**
	 * Creates a JUnit launch configuration tab.
	 */
	public GUnitLaunchConfigurationTab() {
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
				.setText(GUnitMessages.JUnitLaunchConfigurationTab_Test_Loader);
		GridData gd = new GridData();
		gd.horizontalIndent = 0;
		loaderLabel.setLayoutData(gd);

		this.fTestLoaderViewer = new ComboViewer(comp, SWT.DROP_DOWN
				| SWT.READ_ONLY);
		this.fTestLoaderViewer.getCombo().setLayoutData(
				new GridData(GridData.FILL_HORIZONTAL));

		List<TestKind> items = TestKindRegistry.getDefault().getAllKinds();
		this.fTestLoaderViewer.setContentProvider(new ArrayContentProvider());
		this.fTestLoaderViewer.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {
				return ((TestKind) element).getDisplayName();
			}
		});
		this.fTestLoaderViewer.setInput(items);
		this.fTestLoaderViewer
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
		this.fTestRadioButton = new Button(comp, SWT.RADIO);
		this.fTestRadioButton
				.setText(GUnitMessages.JUnitLaunchConfigurationTab_label_oneTest);
		GridData gd = new GridData();
		gd.horizontalSpan = 3;
		this.fTestRadioButton.setLayoutData(gd);
		this.fTestRadioButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (GUnitLaunchConfigurationTab.this.fTestRadioButton
						.getSelection()) {
					testModeChanged();
				}
			}
		});

		this.fProjLabel = new Label(comp, SWT.NONE);
		this.fProjLabel
				.setText(GUnitMessages.JUnitLaunchConfigurationTab_label_project);
		gd = new GridData();
		gd.horizontalIndent = 25;
		this.fProjLabel.setLayoutData(gd);

		this.fProjText = new Text(comp, SWT.SINGLE | SWT.BORDER);
		this.fProjText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		this.fProjText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				validatePage();
				updateLaunchConfigurationDialog();
				GUnitLaunchConfigurationTab.this.fSearchButton
						.setEnabled(GUnitLaunchConfigurationTab.this.fTestRadioButton
								.getSelection()
								&& GUnitLaunchConfigurationTab.this.fProjText
										.getText().length() > 0);
			}
		});

		this.fProjButton = new Button(comp, SWT.PUSH);
		this.fProjButton
				.setText(GUnitMessages.JUnitLaunchConfigurationTab_label_browse);
		this.fProjButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				handleProjectButtonSelected();
			}
		});
		setButtonGridData(this.fProjButton);

		this.fTestLabel = new Label(comp, SWT.NONE);
		gd = new GridData();
		gd.horizontalIndent = 25;
		this.fTestLabel.setLayoutData(gd);
		this.fTestLabel
				.setText(GUnitMessages.JUnitLaunchConfigurationTab_label_test);

		this.fTestText = new Text(comp, SWT.SINGLE | SWT.BORDER);
		this.fTestText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		this.fTestText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				validatePage();
				updateLaunchConfigurationDialog();
			}
		});

		this.fSearchButton = new Button(comp, SWT.PUSH);
		this.fSearchButton.setEnabled(this.fProjText.getText().length() > 0);
		this.fSearchButton
				.setText(GUnitMessages.JUnitLaunchConfigurationTab_label_search);
		this.fSearchButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				handleSearchButtonSelected();
			}
		});
		setButtonGridData(this.fSearchButton);

		new Label(comp, SWT.NONE);

		this.fTestMethodLabel = new Label(comp, SWT.NONE);
		this.fTestMethodLabel.setText(""); //$NON-NLS-1$
		gd = new GridData();
		gd.horizontalSpan = 2;
		this.fTestMethodLabel.setLayoutData(gd);

	}

	private void createTestContainerSelectionGroup(Composite comp) {
		this.fTestContainerRadioButton = new Button(comp, SWT.RADIO);
		this.fTestContainerRadioButton
				.setText(GUnitMessages.JUnitLaunchConfigurationTab_label_containerTest);
		GridData gd = new GridData();
		gd.horizontalSpan = 3;
		this.fTestContainerRadioButton.setLayoutData(gd);
		this.fTestContainerRadioButton
				.addSelectionListener(new SelectionListener() {
					public void widgetSelected(SelectionEvent e) {
						if (GUnitLaunchConfigurationTab.this.fTestContainerRadioButton
								.getSelection()) {
							testModeChanged();
						}
					}

					public void widgetDefaultSelected(SelectionEvent e) {
					}
				});

		this.fContainerText = new Text(comp, SWT.SINGLE | SWT.BORDER
				| SWT.READ_ONLY);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalIndent = 25;
		gd.horizontalSpan = 2;
		this.fContainerText.setLayoutData(gd);
		this.fContainerText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		this.fContainerSearchButton = new Button(comp, SWT.PUSH);
		this.fContainerSearchButton
				.setText(GUnitMessages.JUnitLaunchConfigurationTab_label_search);
		this.fContainerSearchButton
				.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent evt) {
						handleContainerSearchButtonSelected();
					}
				});
		setButtonGridData(this.fContainerSearchButton);
	}

	private void handleContainerSearchButtonSelected() {
		IErlElement javaElement = chooseContainer(this.fContainerElement);
		if (javaElement != null) {
			setContainerElement(javaElement);
		}
	}

	private void setContainerElement(IErlElement javaElement) {
		this.fContainerElement = javaElement;
		this.fContainerText.setText(getPresentationName(javaElement));
		validatePage();
		updateLaunchConfigurationDialog();
	}

	private void createKeepAliveGroup(Composite comp) {
		GridData gd;
		this.fKeepRunning = new Button(comp, SWT.CHECK);
		this.fKeepRunning.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				updateLaunchConfigurationDialog();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		this.fKeepRunning
				.setText(GUnitMessages.JUnitLaunchConfigurationTab_label_keeprunning);
		gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.horizontalSpan = 2;
		this.fKeepRunning.setLayoutData(gd);
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

		if (containerHandle.length() > 0) {
			updateTestContainerFromConfig(config);
		} else {
			updateTestTypeFromConfig(config);
		}
		updateKeepRunning(config);
		updateTestLoaderFromConfig(config);
	}

	private void updateTestLoaderFromConfig(ILaunchConfiguration config) {
		// ITestKind testKind = GUnitLaunchConfigurationConstants
		// .getTestRunnerKind(config);
		// if (testKind.isNull()) {
		// testKind = TestKindRegistry.getDefault().getKind(
		// TestKindRegistry.JUNIT3_TEST_KIND_ID);
		// }
		// fTestLoaderViewer.setSelection(new StructuredSelection(testKind));
	}

	private TestKind getSelectedTestKind() {
		IStructuredSelection selection = (IStructuredSelection) this.fTestLoaderViewer
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
		this.fKeepRunning.setSelection(running);
	}

	private void updateProjectFromConfig(ILaunchConfiguration config) {
		//		String projectName = ""; //$NON-NLS-1$
		// try {
		// projectName = config.getAttribute(
		//					IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""); //$NON-NLS-1$
		// } catch (CoreException ce) {
		// }
		// fProjText.setText(projectName);
	}

	private void updateTestTypeFromConfig(ILaunchConfiguration config) {
		//		String testTypeName = ""; //$NON-NLS-1$
		//		fOriginalTestMethodName = ""; //$NON-NLS-1$
		// try {
		// testTypeName = config.getAttribute(
		//					IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, ""); //$NON-NLS-1$
		// fOriginalTestMethodName = config
		// .getAttribute(
		// GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME,
		//							""); //$NON-NLS-1$
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

	private void setTestMethodLabel(String testMethodName) {
		if (!"".equals(testMethodName)) { //$NON-NLS-1$
			this.fTestMethodLabel
					.setText(GUnitMessages.JUnitLaunchConfigurationTab_label_method
							+ this.fOriginalTestMethodName);
		} else {
			this.fTestMethodLabel.setText(""); //$NON-NLS-1$
		}
	}

	private void updateTestContainerFromConfig(ILaunchConfiguration config) {
		String containerHandle = ""; //$NON-NLS-1$
		// IErlElement containerElement = null;
		// try {
		// containerHandle = config.getAttribute(
		//					GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
		// if (containerHandle.length() > 0) {
		// containerElement = JavaCore.create(containerHandle);
		// }
		// } catch (CoreException ce) {
		// }
		// if (containerElement != null) {
		// fContainerElement = containerElement;
		// }
		// fTestContainerRadioButton.setSelection(true);
		// setEnableSingleTestGroup(false);
		// setEnableContainerTestGroup(true);
		// fTestRadioButton.setSelection(false);
		// if (fContainerElement != null) {
		// fContainerText.setText(getPresentationName(fContainerElement));
		// }
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
		// if (fTestContainerRadioButton.getSelection()
		// && fContainerElement != null) {
		// config.setAttribute(
		// IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
		// fContainerElement.getJavaProject().getElementName());
		// config.setAttribute(
		// GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER,
		// fContainerElement.getHandleIdentifier());
		// config.setAttribute(
		//					IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, ""); //$NON-NLS-1$
		// // workaround for bug 65399
		// config
		// .setAttribute(
		// GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME,
		//							""); //$NON-NLS-1$
		// } else {
		// config.setAttribute(
		// IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
		// fProjText.getText());
		// config.setAttribute(
		// IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
		// fTestText.getText());
		// config.setAttribute(
		//					GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
		// config.setAttribute(
		// GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME,
		// fOriginalTestMethodName);
		// }
		// config.setAttribute(GUnitLaunchConfigurationConstants.ATTR_KEEPRUNNING,
		// fKeepRunning.getSelection());
		// try {
		// mapResources(config);
		// } catch (CoreException e) {
		// GUnitPlugin.log(e.getStatus());
		// }
		// IStructuredSelection testKindSelection = (IStructuredSelection)
		// fTestLoaderViewer
		// .getSelection();
		// if (!testKindSelection.isEmpty()) {
		// TestKind testKind = (TestKind) testKindSelection.getFirstElement();
		// config.setAttribute(
		// GUnitLaunchConfigurationConstants.ATTR_TEST_RUNNER_KIND,
		// testKind.getId());
		// }
	}

	private void mapResources(ILaunchConfigurationWorkingCopy config)
			throws CoreException {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#dispose()
	 */
	@Override
	public void dispose() {
		super.dispose();
		this.fTestIcon.dispose();
		this.fErlElementLabelProvider.dispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#getImage()
	 */
	@Override
	public Image getImage() {
		return this.fTestIcon;
	}

	/*
	 * Show a dialog that lists all main types
	 */
	private void handleSearchButtonSelected() {
		// Shell shell = getShell();
		//
		// IErlProject javaProject = getJavaProject();
		//
		// IErlModule[] types = new IErlModule[0];
		// boolean[] radioSetting = new boolean[2];
		// try {
		// // fix for 66922 Wrong radio behaviour when switching
		// // remember the selected radio button
		// radioSetting[0] = fTestRadioButton.getSelection();
		// radioSetting[1] = fTestContainerRadioButton.getSelection();
		//
		// types = TestSearchEngine.findTests(getLaunchConfigurationDialog(),
		// javaProject, getSelectedTestKind());
		// } catch (InterruptedException e) {
		// setErrorMessage(e.getMessage());
		// return;
		// } catch (InvocationTargetException e) {
		// GUnitPlugin.log(e.getTargetException());
		// return;
		// } finally {
		// fTestRadioButton.setSelection(radioSetting[0]);
		// fTestContainerRadioButton.setSelection(radioSetting[1]);
		// }
		//
		// SelectionDialog dialog = new TestSelectionDialog(shell, types);
		// dialog
		// .setTitle(JUnitMessages.JUnitLaunchConfigurationTab_testdialog_title);
		// dialog
		// .setMessage(JUnitMessages.JUnitLaunchConfigurationTab_testdialog_message);
		// if (dialog.open() == Window.CANCEL) {
		// return;
		// }
		//
		// Object[] results = dialog.getResult();
		// if ((results == null) || (results.length < 1)) {
		// return;
		// }
		// IErlModule type = (IErlModule) results[0];
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
	private void handleProjectButtonSelected() {
		IErlProject project = chooseErlangProject();
		if (project == null) {
			return;
		}

		String projectName = project.getName();
		this.fProjText.setText(projectName);
	}

	/*
	 * Realize a Java Project selection dialog and return the first selected
	 * project, or null if there was none.
	 */
	private IErlProject chooseErlangProject() {
		IErlProject[] projects;
		// try {
		// projects = ErlangCore.create(getWorkspaceRoot()).getJavaProjects();
		// } catch (JavaModelException e) {
		// GUnitPlugin.log(e.getStatus());
		// projects = new IErlProject[0];
		// }
		//
		// ILabelProvider labelProvider = new JavaElementLabelProvider(
		// JavaElementLabelProvider.SHOW_DEFAULT);
		// ElementListSelectionDialog dialog = new ElementListSelectionDialog(
		// getShell(), labelProvider);
		// dialog
		// .setTitle(JUnitMessages.JUnitLaunchConfigurationTab_projectdialog_title);
		// dialog
		// .setMessage(JUnitMessages.JUnitLaunchConfigurationTab_projectdialog_message);
		// dialog.setElements(projects);
		//
		// IErlProject javaProject = getJavaProject();
		// if (javaProject != null) {
		// dialog.setInitialSelections(new Object[] { javaProject });
		// }
		// if (dialog.open() == Window.OK) {
		// return (IErlProject) dialog.getFirstResult();
		// }
		return null;
	}

	/*
	 * Return the IErlProject corresponding to the project name in the project
	 * name text field, or null if the text does not match a project name.
	 */
	private IErlProject getErlangProject() {
		String projectName = this.fProjText.getText().trim();
		if (projectName.length() < 1) {
			return null;
		}
		return getErlModel().getErlangProject(projectName);
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
	private IErlModel getErlModel() {
		return ErlangCore.getModel();
	}

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

	private void testModeChanged() {
		boolean isSingleTestMode = this.fTestRadioButton.getSelection();
		setEnableSingleTestGroup(isSingleTestMode);
		setEnableContainerTestGroup(!isSingleTestMode);
		if (!isSingleTestMode && this.fContainerText.getText().length() == 0) {
			String projText = this.fProjText.getText();
			if (Path.EMPTY.isValidSegment(projText)) {
				IErlProject javaProject = getErlModel().getErlangProject(
						projText);
				if (javaProject != null && javaProject.exists()) {
					setContainerElement(javaProject);
				}
			}
		}
		validatePage();
		updateLaunchConfigurationDialog();
	}

	private void validatePage() {
		setErrorMessage(null);
		setMessage(null);

		if (this.fTestContainerRadioButton.getSelection()) {
			if (this.fContainerElement == null) {
				setErrorMessage(GUnitMessages.JUnitLaunchConfigurationTab_error_noContainer);
				return;
			}
			validateErlangProject(this.fContainerElement.getErlProject());
			return;
		}

		String projectName = this.fProjText.getText().trim();
		if (projectName.length() == 0) {
			setErrorMessage(GUnitMessages.JUnitLaunchConfigurationTab_error_projectnotdefined);
			return;
		}

		IStatus status = ResourcesPlugin.getWorkspace().validatePath(
				IPath.SEPARATOR + projectName, IResource.PROJECT);
		if (!status.isOK() || !Path.ROOT.isValidSegment(projectName)) {
			setErrorMessage(Messages
					.format(
							GUnitMessages.JUnitLaunchConfigurationTab_error_invalidProjectName,
							projectName));
			return;
		}

		IProject project = getWorkspaceRoot().getProject(projectName);
		if (!project.exists()) {
			setErrorMessage(GUnitMessages.JUnitLaunchConfigurationTab_error_projectnotexists);
			return;
		}
		IErlProject javaProject = ErlangCore.getModel().findProject(project);
		validateErlangProject(javaProject);

		try {
			if (!project.hasNature(ErlangPlugin.NATURE_ID)) {
				setErrorMessage(GUnitMessages.JUnitLaunchConfigurationTab_error_notJavaProject);
				return;
			}
			String className = this.fTestText.getText().trim();
			if (className.length() == 0) {
				setErrorMessage(GUnitMessages.JUnitLaunchConfigurationTab_error_testnotdefined);
				return;
			}
			// if (javaProject.findType(className) == null) {
			// setErrorMessage(Messages
			// .format(
			// JUnitMessages.JUnitLaunchConfigurationTab_error_test_class_not_found,
			// new String[] { className, projectName }));
			// return;
			// }

		} catch (CoreException e) {
			GUnitPlugin.log(e);
		}

	}

	private void validateErlangProject(IErlProject javaProject) {
		// if (!TestSearchEngine.hasTestCaseType(javaProject)) {
		// setErrorMessage(JUnitMessages.JUnitLaunchConfigurationTab_error_testcasenotonpath);
		// return;
		// }
	}

	private void setEnableContainerTestGroup(boolean enabled) {
		this.fContainerSearchButton.setEnabled(enabled);
		this.fContainerText.setEnabled(enabled);
	}

	private void setEnableSingleTestGroup(boolean enabled) {
		this.fProjLabel.setEnabled(enabled);
		this.fProjText.setEnabled(enabled);
		this.fProjButton.setEnabled(enabled);
		this.fTestLabel.setEnabled(enabled);
		this.fTestText.setEnabled(enabled);
		this.fSearchButton.setEnabled(enabled
				&& this.fProjText.getText().length() > 0);
		this.fTestMethodLabel.setEnabled(enabled);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchConfigurationTab#setDefaults(org.eclipse.
	 * debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		IErlElement javaElement = getContext();
		if (javaElement != null) {
			initializeErlangProject(javaElement, config);
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
			// config.setAttribute(
			//					IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""); //$NON-NLS-1$
			config.setAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
		}
		initializeTestAttributes(javaElement, config);
	}

	private void initializeTestAttributes(IErlElement javaElement,
			ILaunchConfigurationWorkingCopy config) {
		// if (javaElement != null
		// && javaElement.getElementType() < IErlElement.COMPILATION_UNIT)
		// initializeTestContainer(javaElement, config);
		// else
		// initializeTestType(javaElement, config);
	}

	private void initializeTestContainer(IErlElement javaElement,
			ILaunchConfigurationWorkingCopy config) {
		// config.setAttribute(
		// GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER,
		// javaElement.getHandleIdentifier());
		initializeName(config, javaElement.getName());
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
	 * IErlElement
	 */
	private void initializeTestType(IErlElement javaElement,
			ILaunchConfigurationWorkingCopy config) {
		//		String name = ""; //$NON-NLS-1$
		// String testKindId = null;
		// try {
		// // we only do a search for compilation units or class files or
		// // or source references
		// if (javaElement instanceof ISourceReference) {
		// ITestKind testKind = TestKindRegistry
		// .getContainerTestKind(javaElement);
		// testKindId = testKind.getId();
		//
		// IErlModule[] types = TestSearchEngine.findTests(
		// getLaunchConfigurationDialog(), javaElement, testKind);
		// if ((types == null) || (types.length < 1)) {
		// return;
		// }
		// // Simply grab the first main type found in the searched element
		// name = JavaModelUtil.getFullyQualifiedName(types[0]);
		//
		// }
		// } catch (InterruptedException ie) {
		//
		// } catch (InvocationTargetException ite) {
		// }
		// config.setAttribute(
		// IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, name);
		// if (testKindId != null) {
		// config.setAttribute(
		// GUnitLaunchConfigurationConstants.ATTR_TEST_RUNNER_KIND,
		// testKindId);
		// }
		// initializeName(config, name);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#getName()
	 */
	public String getName() {
		return GUnitMessages.JUnitLaunchConfigurationTab_tab_label;
	}

	private IErlElement chooseContainer(IErlElement initElement) {
		// Class[] acceptedClasses = new Class[] { IErlProject.class, };
		// TypedElementSelectionValidator validator = new
		// TypedElementSelectionValidator(
		// acceptedClasses, false) {
		// public boolean isSelectedValid(Object element) {
		// return true;
		// }
		// };
		//
		// acceptedClasses = new Class[] { IErlModel.class, IErlProject.class,
		// };
		// ViewerFilter filter = new TypedViewerFilter(acceptedClasses) {
		// public boolean select(Viewer viewer, Object parent, Object element) {
		// if (element instanceof IPackageFragmentRoot
		// && ((IPackageFragmentRoot) element).isArchive()) {
		// return false;
		// }
		// return super.select(viewer, parent, element);
		// }
		// };
		//
		// StandardJavaElementContentProvider provider = new
		// StandardJavaElementContentProvider();
		// ILabelProvider labelProvider = new JavaElementLabelProvider(
		// JavaElementLabelProvider.SHOW_DEFAULT);
		// ElementTreeSelectionDialog dialog = new ElementTreeSelectionDialog(
		// getShell(), labelProvider, provider);
		// dialog.setValidator(validator);
		// dialog.setComparator(new JavaElementComparator());
		// dialog
		// .setTitle(JUnitMessages.JUnitLaunchConfigurationTab_folderdialog_title);
		// dialog
		// .setMessage(JUnitMessages.JUnitLaunchConfigurationTab_folderdialog_message);
		// dialog.addFilter(filter);
		// dialog.setInput(JavaCore.create(getWorkspaceRoot()));
		// dialog.setInitialSelection(initElement);
		// dialog.setAllowMultiple(false);
		//
		// if (dialog.open() == Window.OK) {
		// Object element = dialog.getFirstResult();
		// return (IErlElement) element;
		// }
		return null;
	}

	private String getPresentationName(IErlElement element) {
		return this.fErlElementLabelProvider.getText(element);
	}

	/*
	 * Returns the current Java element context from which to initialize default
	 * settings, or <code>null</code> if none.
	 * 
	 * @return Java element context.
	 */
	private IErlElement getContext() {
		// IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench()
		// .getActiveWorkbenchWindow();
		// if (activeWorkbenchWindow == null) {
		// return null;
		// }
		// IWorkbenchPage page = activeWorkbenchWindow.getActivePage();
		// if (page != null) {
		// ISelection selection = page.getSelection();
		// if (selection instanceof IStructuredSelection) {
		// IStructuredSelection ss = (IStructuredSelection) selection;
		// if (!ss.isEmpty()) {
		// Object obj = ss.getFirstElement();
		// if (obj instanceof IErlElement) {
		// return (IErlElement) obj;
		// }
		// if (obj instanceof IResource) {
		// IErlElement je = JavaCore.create((IResource) obj);
		// if (je == null) {
		// IProject pro = ((IResource) obj).getProject();
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
		// return (IErlElement) input.getAdapter(IErlElement.class);
		// }
		// }
		return null;
	}

	private void initializeErlangProject(IErlElement javaElement,
			ILaunchConfigurationWorkingCopy config) {
		IErlProject javaProject = javaElement.getErlProject();
		String name = null;
		if (javaProject != null && javaProject.exists()) {
			name = javaProject.getName();
		}
		// config.setAttribute(
		// IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, name);
	}

	private void setButtonGridData(Button button) {
		GridData gridData = new GridData();
		button.setLayoutData(gridData);
		LayoutUtil.setButtonDimensionHint(button);
	}
}
