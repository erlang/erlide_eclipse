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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.IErlLaunchAttributes;
import org.erlide.runtime.debug.IErlDebugConstants;
import org.erlide.ui.util.SWTUtil;

public class ErlangMainTab extends AbstractLaunchConfigurationTab {

	private CheckboxTableViewer projectsTable;
	private Text moduleText;
	private Text funcText;
	private Text argsText;
	private Button attachOnFirstCallCheck;
	private Button attachOnBreakpointCheck;
	private Button attachOnExitCheck;
	private Button distributedDebugCheck;

	public void createControl(final Composite parent) {
		final Composite comp = new Composite(parent, SWT.NONE);
		setControl(comp);
		final GridLayout topLayout = new GridLayout();
		comp.setLayout(topLayout);

		createProjectsGroup(comp);

		createStartGroup(comp);

		createDebugFlagsGroup(comp);

		List<IErlProject> projects;
		try {
			projects = ErlangCore.getModel().getErlangProjects();
			final List<String> ps = new ArrayList<String>();
			for (final IErlProject p : projects) {
				ps.add(p.getName());
			}
		} catch (final ErlModelException e) {
			e.printStackTrace();
		}
	}

	private void createDebugFlagsGroup(final Composite comp) {
		final Group debugGroup = SWTUtil.createGroup(comp, "Debug", 1,
				GridData.FILL_HORIZONTAL);
		distributedDebugCheck = createCheckButton(debugGroup,
				"Debug all connected nodes");
		distributedDebugCheck.addSelectionListener(fBasicSelectionListener);
		final Group attachGroup = SWTUtil.createGroup(debugGroup,
				"Auto Attach", 1, GridData.FILL_BOTH);
		attachOnFirstCallCheck = createCheckButton(attachGroup, "First &call");
		attachOnFirstCallCheck.addSelectionListener(fBasicSelectionListener);
		attachOnBreakpointCheck = createCheckButton(attachGroup, "&Breakpoint");
		attachOnBreakpointCheck.addSelectionListener(fBasicSelectionListener);
		attachOnExitCheck = createCheckButton(attachGroup, "E&xit");
		attachOnExitCheck.addSelectionListener(fBasicSelectionListener);
	}

	/**
	 * @param comp
	 */
	private void createProjectsGroup(final Composite comp) {
		final Group projectsGroup = SWTUtil.createGroup(comp, "Projects", 2,
				GridData.FILL_HORIZONTAL);
		projectsTable = CheckboxTableViewer.newCheckList(projectsGroup,
				SWT.HIDE_SELECTION | SWT.BORDER);
		projectsTable.setLabelProvider(new ProjectsLabelProvider());
		projectsTable.setContentProvider(new ProjectsContentProvider());
		final Table table_1 = projectsTable.getTable();
		final GridData gd_table_1 = new GridData(SWT.LEFT, SWT.FILL, true, true);
		gd_table_1.widthHint = 287;
		gd_table_1.heightHint = 126;
		gd_table_1.minimumHeight = 108;
		gd_table_1.minimumWidth = 256;
		table_1.setLayoutData(gd_table_1);
		projectsTable.addCheckStateListener(new ICheckStateListener() {
			@SuppressWarnings("synthetic-access")
			public void checkStateChanged(final CheckStateChangedEvent event) {
				updateLaunchConfigurationDialog();
			}
		});
	}

	/**
	 * @param comp
	 */
	private void createStartGroup(final Composite comp) {
		final Group startGroup = new Group(comp, SWT.NONE);
		startGroup.setText("Start");
		final GridData gd_startGroup = new GridData(SWT.FILL, SWT.CENTER,
				false, false);
		startGroup.setLayoutData(gd_startGroup);
		final GridLayout gridLayout_1 = new GridLayout();
		gridLayout_1.numColumns = 4;
		startGroup.setLayout(gridLayout_1);

		moduleText = textWithLabel(startGroup, "Module", 114,
				fBasicModifyListener);
		funcText = textWithLabel(startGroup, "Function", 107,
				fBasicModifyListener);
		argsText = textWithLabel(startGroup, "Arguments", 3,
				fBasicModifyListener);

		new Label(startGroup, SWT.NONE);

		final Label infoLabel = new Label(startGroup, SWT.NONE);
		final GridData gd_infoLabel = new GridData(SWT.LEFT, SWT.CENTER, false,
				false, 3, 1);
		infoLabel.setLayoutData(gd_infoLabel);
		infoLabel
				.setText("The arguments will be sent as one single argument, a string.");
	}

	/**
	 * @param startGroup
	 * @param labelText
	 *            TODO
	 * @param textWidthHint
	 *            TODO
	 * @param modifyListener
	 *            TODO
	 */
	private Text textWithLabel(final Group startGroup, final String labelText,
			final int textWidthHint, final ModifyListener modifyListener) {
		final Label label = new Label(startGroup, SWT.NONE);
		label.setLayoutData(new GridData());
		label.setText(labelText);

		final Text text = new Text(startGroup, SWT.SINGLE | SWT.BORDER);
		final GridData gd;
		if (textWidthHint < 10) {
			gd = new GridData(SWT.FILL, SWT.CENTER, false, false,
					textWidthHint, 1);
		} else {
			gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
			gd.widthHint = textWidthHint;
		}
		text.setLayoutData(gd);
		text.addModifyListener(modifyListener);

		return text;
	}

	/**
	 * Content provider for the projects table
	 */
	static class ProjectsContentProvider implements IStructuredContentProvider {

		public Object[] getElements(final Object inputElement) {
			final java.util.List<String> ps = new ArrayList<String>();

			IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
					.getProjects();
			for (final IProject p : projects) {
				if (p.isAccessible()) {
					IProjectNature n = null;
					try {
						n = p.getNature(ErlangPlugin.NATURE_ID);
					} catch (CoreException e) {
					}
					if (n == null) {
						ErlLogger.debug(
								"project %s doesn't have an erlang nature", p
										.getName());
					} else {
						ps.add(p.getName());
					}
				}
			}
			return ps.toArray(new String[0]);
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
	static class ProjectsLabelProvider implements ITableLabelProvider {
		public Image getColumnImage(final Object element, final int columnIndex) {
			return null;
		}

		public String getColumnText(final Object element, final int columnIndex) {
			if (element instanceof String) {
				return (String) element;
			}
			return "?" + element;
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

	public void setDefaults(final ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(IErlLaunchAttributes.PROJECTS, "");
		config.setAttribute(IErlLaunchAttributes.MODULE, "");
		config.setAttribute(IErlLaunchAttributes.FUNCTION, "");
		config.setAttribute(IErlLaunchAttributes.ARGUMENTS, "");
		config.setAttribute(IErlLaunchAttributes.DEBUG_FLAGS,
				IErlDebugConstants.DEFAULT_DEBUG_FLAGS);
	}

	public void initializeFrom(final ILaunchConfiguration config) {
		projectsTable.setInput(config);
		String projs;
		try {
			projs = config.getAttribute(IErlLaunchAttributes.PROJECTS, "");
		} catch (final CoreException e1) {
			projs = "";
		}
		final String[] projects = projs.split(";");
		projectsTable.setAllChecked(false);
		for (final String p : projects) {
			projectsTable.setChecked(p, true);
		}
		final int itemCount = projectsTable.getTable().getItemCount();
		if (itemCount == 1) {
			projectsTable.setChecked(projectsTable.getTable().getItem(0), true);
		}

		try {
			final String attribute = config.getAttribute(
					IErlLaunchAttributes.MODULE, "");
			moduleText.setText(attribute);
		} catch (final CoreException e) {
			moduleText.setText("");
		}
		try {
			final String attribute = config.getAttribute(
					IErlLaunchAttributes.FUNCTION, "");
			funcText.setText(attribute);
		} catch (final CoreException e) {
			funcText.setText("");
		}
		try {
			final String attribute = config.getAttribute(
					IErlLaunchAttributes.ARGUMENTS, "");
			argsText.setText(attribute);
		} catch (final CoreException e) {
			argsText.setText("");
		}
		int debugFlags;
		try {
			debugFlags = config.getAttribute(IErlLaunchAttributes.DEBUG_FLAGS,
					IErlDebugConstants.DEFAULT_DEBUG_FLAGS);
		} catch (final CoreException e) {
			debugFlags = IErlDebugConstants.DEFAULT_DEBUG_FLAGS;
		}
		setFlagCheckboxes(debugFlags);

		updateLaunchConfigurationDialog();
	}

	private void setFlagCheckboxes(final int debugFlags) {
		attachOnFirstCallCheck
				.setSelection((debugFlags & IErlDebugConstants.ATTACH_ON_FIRST_CALL_FLAG) != 0);
		attachOnBreakpointCheck
				.setSelection((debugFlags & IErlDebugConstants.ATTACH_ON_BREAKPOINT_FLAG) != 0);
		attachOnExitCheck
				.setSelection((debugFlags & IErlDebugConstants.ATTACH_ON_EXIT_FLAG) != 0);
		distributedDebugCheck
				.setSelection((debugFlags & IErlDebugConstants.DISTRIBUTED_DEBUG_FLAG) != 0);
	}

	private int getFlagChechboxes() {
		int result = attachOnFirstCallCheck.getSelection() ? IErlDebugConstants.ATTACH_ON_FIRST_CALL_FLAG
				: 0;
		result += attachOnBreakpointCheck.getSelection() ? IErlDebugConstants.ATTACH_ON_BREAKPOINT_FLAG
				: 0;
		result += attachOnExitCheck.getSelection() ? IErlDebugConstants.ATTACH_ON_EXIT_FLAG
				: 0;
		result += distributedDebugCheck.getSelection() ? IErlDebugConstants.DISTRIBUTED_DEBUG_FLAG
				: 0;
		return result;
	}

	public void performApply(final ILaunchConfigurationWorkingCopy config) {
		final Object[] sel = projectsTable.getCheckedElements();
		final StringBuilder projectNames = new StringBuilder();
		for (final Object o : sel) {
			final String p = (String) o;
			projectNames.append(p).append(";");
		}
		if (projectNames.length() > 0) {
			projectNames.setLength(projectNames.length() - 1);
		}
		config.setAttribute(IErlLaunchAttributes.PROJECTS, projectNames
				.toString());

		config.setAttribute(IErlLaunchAttributes.MODULE, moduleText.getText());
		config.setAttribute(IErlLaunchAttributes.FUNCTION, funcText.getText());
		config.setAttribute(IErlLaunchAttributes.ARGUMENTS, argsText.getText());
		config.setAttribute(IErlLaunchAttributes.DEBUG_FLAGS,
				getFlagChechboxes());
	}

	public String getName() {
		return "Main";
	}

	@Override
	public boolean isValid(final ILaunchConfiguration launchConfig) {
		if (projectsTable.getCheckedElements().length == 0) {
			return false;
		}
		return true;
	}

	private final ModifyListener fBasicModifyListener = new ModifyListener() {
		@SuppressWarnings("synthetic-access")
		public void modifyText(ModifyEvent evt) {
			updateLaunchConfigurationDialog();
		}
	};

	private final SelectionListener fBasicSelectionListener = new SelectionListener() {
		@SuppressWarnings("synthetic-access")
		public void widgetDefaultSelected(SelectionEvent e) {
			updateLaunchConfigurationDialog();
		}

		@SuppressWarnings("synthetic-access")
		public void widgetSelected(SelectionEvent e) {
			updateLaunchConfigurationDialog();
		}
	};

}
