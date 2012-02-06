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
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IWorkspaceRoot;
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
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlProject;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.ui.util.SWTUtil;

public class ErlangMainTab extends AbstractLaunchConfigurationTab {

    private CheckboxTableViewer projectsTable;
    private Text moduleText;
    private Text funcText;
    private Text argsText;

    /**
     * @wbp.parser.entryPoint
     */
    @Override
    public void createControl(final Composite parent) {
        final Composite comp = new Composite(parent, SWT.NONE);
        setControl(comp);
        final GridLayout topLayout = new GridLayout();
        comp.setLayout(topLayout);

        createProjectsGroup(comp);
        createStartGroup(comp);

        Collection<IErlProject> projects;
        try {
            projects = ErlModelManager.getErlangModel().getErlangProjects();
            final List<String> ps = new ArrayList<String>();
            for (final IErlProject p : projects) {
                ps.add(p.getName());
            }
        } catch (final ErlModelException e) {
            ErlLogger.warn(e);
        }
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
            @Override
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
                .setText("Start function takes no arguments or a single string.\n"
                        + "It's similar to using '-s mod fun args' on the command line.\n"
                        + "Use it for system initialization/startup.");
    }

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

        @Override
        public Object[] getElements(final Object inputElement) {
            final java.util.List<String> ps = new ArrayList<String>();

            final IProject[] projects = ResourcesPlugin.getWorkspace()
                    .getRoot().getProjects();
            for (final IProject p : projects) {
                if (p.isAccessible()) {
                    IProjectNature n = null;
                    try {
                        n = p.getNature(ErlangCore.NATURE_ID);
                        if (n != null) {
                            ps.add(p.getName());
                        }
                    } catch (final CoreException e) {
                    }
                }
            }
            return ps.toArray(new String[0]);
        }

        @Override
        public void dispose() {
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
        }

    }

    /**
     * Provides the labels for the projects table
     * 
     */
    static class ProjectsLabelProvider implements ITableLabelProvider {
        @Override
        public Image getColumnImage(final Object element, final int columnIndex) {
            return null;
        }

        @Override
        public String getColumnText(final Object element, final int columnIndex) {
            if (element instanceof String) {
                return (String) element;
            }
            return "?" + element;
        }

        @Override
        public void addListener(final ILabelProviderListener listener) {
        }

        @Override
        public void dispose() {
        }

        @Override
        public boolean isLabelProperty(final Object element,
                final String property) {
            return false;
        }

        @Override
        public void removeListener(final ILabelProviderListener listener) {
        }
    }

    @Override
    public void setDefaults(final ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(ErlLaunchAttributes.PROJECTS, "");
        config.setAttribute(ErlLaunchAttributes.MODULE, "");
        config.setAttribute(ErlLaunchAttributes.FUNCTION, "");
        config.setAttribute(ErlLaunchAttributes.ARGUMENTS, "");
        config.setAttribute(ErlLaunchAttributes.DEBUG_FLAGS,
                ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
    }

    @Override
    public void initializeFrom(final ILaunchConfiguration config) {
        projectsTable.setInput(config);
        String projs;
        try {
            projs = config.getAttribute(ErlLaunchAttributes.PROJECTS, "");
        } catch (final CoreException e1) {
            projs = "";
        }
        final String[] projectNames = projs.split(";");
        projectsTable.setAllChecked(false);
        for (final String p : projectNames) {
            projectsTable.setChecked(p, true);
        }
        // final Set<IProject> projects = new HashSet<IProject>();
        // ErlangLaunchConfigurationDelegate.addBreakpointProjectsAndModules(
        // projects, new HashSet<String>());
        // for (final IProject p : projects) {
        // projectsTable.setChecked(p.getName(), true);
        // }
        final int itemCount = projectsTable.getTable().getItemCount();
        if (itemCount == 1) {
            projectsTable.setChecked(projectsTable.getTable().getItem(0), true);
        }

        try {
            final String attribute = config.getAttribute(
                    ErlLaunchAttributes.MODULE, "");
            moduleText.setText(attribute);
        } catch (final CoreException e) {
            moduleText.setText("");
        }
        try {
            final String attribute = config.getAttribute(
                    ErlLaunchAttributes.FUNCTION, "");
            funcText.setText(attribute);
        } catch (final CoreException e) {
            funcText.setText("");
        }
        try {
            final String attribute = config.getAttribute(
                    ErlLaunchAttributes.ARGUMENTS, "");
            argsText.setText(attribute);
        } catch (final CoreException e) {
            argsText.setText("");
        }

        updateLaunchConfigurationDialog();
    }

    @Override
    public void performApply(final ILaunchConfigurationWorkingCopy config) {
        final List<IProject> projects = getSelectedProjects();
        final StringBuilder projectNames = new StringBuilder();
        for (final IProject p : projects) {
            projectNames.append(p.getName()).append(';');
        }
        if (projectNames.length() > 0) {
            projectNames.setLength(projectNames.length() - 1);
        }
        config.setAttribute(ErlLaunchAttributes.PROJECTS,
                projectNames.toString());

        config.setAttribute(ErlLaunchAttributes.MODULE, moduleText.getText());
        config.setAttribute(ErlLaunchAttributes.FUNCTION, funcText.getText());
        config.setAttribute(ErlLaunchAttributes.ARGUMENTS, argsText.getText());
    }

    public List<IProject> getSelectedProjects() {
        final Object[] sel = projectsTable.getCheckedElements();
        final List<IProject> result = new ArrayList<IProject>();
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        for (final Object o : sel) {
            final String p = (String) o;
            result.add(root.getProject(p));
        }
        return result;
    }

    @Override
    public String getName() {
        return "Erlang";
    }

    @Override
    public boolean isValid(final ILaunchConfiguration launchConfig) {
        if (projectsTable.getCheckedElements().length == 0) {
            return false;
        }
        return true;
    }

    private final ModifyListener fBasicModifyListener = new ModifyListener() {
        @Override
        @SuppressWarnings("synthetic-access")
        public void modifyText(final ModifyEvent evt) {
            updateLaunchConfigurationDialog();
        }
    };

}
