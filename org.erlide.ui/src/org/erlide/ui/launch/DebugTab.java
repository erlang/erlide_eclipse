/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *     Jakob C
 *******************************************************************************/
package org.erlide.ui.launch;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.launch.ErlangLaunchDelegate;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.ui.util.SWTUtil;
import org.erlide.utils.CommonUtils;

/**
 * A tab in the Launch Config with erlang debugger parameters: the debug flags
 * for attaching and distruibuted debugging a checkbox tree of modules to
 * interpret upon launching. The checkbox tree classes are reused by
 * InterpretedModulesView
 * 
 */
public class DebugTab extends AbstractLaunchConfigurationTab {

    CheckboxTreeViewer checkboxTreeViewer;
    private Button attachOnFirstCallCheck;
    private Button attachOnBreakpointCheck;
    private Button attachOnExitCheck;
    private Button distributedDebugCheck;
    private List<IErlModule> interpretedModules;

    public static class TreeLabelProvider extends LabelProvider {
        public TreeLabelProvider() {
            super();
        }

        @Override
        public String getText(final Object element) {
            if (element instanceof DebugTreeItem) {
                final IErlElement item = ((DebugTreeItem) element).item;
                if (item == null) {
                    ErlLogger.warn("Null item in DebugTreeItem %s",
                            element.toString());
                    return "---";
                }
                return item.getName();
            }
            return "!" + super.getText(element);
        }

        @Override
        public Image getImage(final Object element) {
            return null;
        }
    }

    public static class TreeContentProvider implements
            IStructuredContentProvider, ITreeContentProvider {
        private DebugTreeItem root;

        public TreeContentProvider() {
            super();
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
            try {
                setRoot(new DebugTreeItem(null, null));
                if (newInput instanceof ILaunchConfiguration) {
                    final ILaunchConfiguration input = (ILaunchConfiguration) newInput;
                    final String projs = input.getAttribute(
                            ErlLaunchAttributes.PROJECTS, "").trim();
                    if (projs.length() == 0) {
                        return;
                    }
                    final String[] projNames = projs.split(";");
                    if (projNames == null) {
                        return;
                    }
                    final IErlModel model = ErlModelManager.getErlangModel();
                    for (final String projName : projNames) {
                        final IErlElement prj = model.getChildNamed(projName);
                        getRoot().addAllErlangModules(prj);
                    }
                }
            } catch (final CoreException e1) {
            }
        }

        @Override
        public void dispose() {
        }

        @Override
        public Object[] getElements(final Object inputElement) {
            return getChildren(getRoot());
        }

        @Override
        public Object[] getChildren(final Object parentElement) {
            final DebugTreeItem dti = (DebugTreeItem) parentElement;
            return dti.children.toArray();
        }

        @Override
        public Object getParent(final Object element) {
            final DebugTreeItem dti = (DebugTreeItem) element;
            return dti.getParent();
        }

        @Override
        public boolean hasChildren(final Object element) {
            return getChildren(element).length > 0;
        }

        public DebugTreeItem getRoot() {
            return root;
        }

        public void setRoot(final DebugTreeItem root) {
            this.root = root;
        }
    }

    /**
     * @wbp.parser.entryPoint
     */
    @Override
    public void createControl(final Composite parent) {
        interpretedModules = new ArrayList<IErlModule>();

        final Composite comp = new Composite(parent, SWT.NONE);
        setControl(comp);
        final GridLayout topLayout = new GridLayout();
        comp.setLayout(topLayout);

        distributedDebugCheck = createCheckButton(comp,
                "Debug all connected nodes");

        final Group attachGroup = SWTUtil.createGroup(comp, "Auto Attach", 1,
                GridData.FILL_BOTH);
        attachGroup.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false,
                false));
        attachOnFirstCallCheck = createCheckButton(attachGroup, "First &call");
        attachOnBreakpointCheck = createCheckButton(attachGroup, "&Breakpoint");
        attachOnExitCheck = createCheckButton(attachGroup, "E&xit");

        distributedDebugCheck.addSelectionListener(fBasicSelectionListener);
        attachOnFirstCallCheck.addSelectionListener(fBasicSelectionListener);
        attachOnBreakpointCheck.addSelectionListener(fBasicSelectionListener);
        attachOnExitCheck.addSelectionListener(fBasicSelectionListener);

        final Group interpretedModulesGroup = new Group(comp, SWT.NONE);
        interpretedModulesGroup.setLayoutData(new GridData(SWT.FILL,
                SWT.CENTER, false, false, 1, 1));
        interpretedModulesGroup.setText("Interpreted modules");
        interpretedModulesGroup.setLayout(new GridLayout());

        final Label anyModuleHavingLabel = new Label(interpretedModulesGroup,
                SWT.WRAP);
        anyModuleHavingLabel.setLayoutData(new GridData(279, SWT.DEFAULT));
        anyModuleHavingLabel
                .setText("Any module having breakpoints enabled will be dynamically added to the list.\n\nThis widget is disabled for now, it takes 100%CPU for large projects. If you need to use \"attach on first call\" or \"attach on exit\", please mark the modules by setting a dummy breakpoint in them. Sorry for the inconvenience!");

        checkboxTreeViewer = new CheckboxTreeViewer(interpretedModulesGroup,
                SWT.BORDER);
        checkboxTreeViewer.addCheckStateListener(new ICheckStateListener() {
            @Override
            @SuppressWarnings("synthetic-access")
            public void checkStateChanged(final CheckStateChangedEvent event) {
                final DebugTreeItem dti = (DebugTreeItem) event.getElement();
                checkboxTreeViewer.setGrayed(dti, false);
                final boolean checked = event.getChecked();
                setSubtreeChecked(dti, checked);
                // checkUpwards(checkboxTreeViewer, dti, checked, false);
                updateLaunchConfigurationDialog();
            }

        });
        checkboxTreeViewer.setLabelProvider(new TreeLabelProvider());
        checkboxTreeViewer.setContentProvider(new TreeContentProvider());
        final Tree tree = checkboxTreeViewer.getTree();
        tree.setEnabled(false);
        final GridData gd_tree = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd_tree.minimumWidth = 250;
        gd_tree.minimumHeight = 120;
        gd_tree.widthHint = 256;
        gd_tree.heightHint = 220;
        tree.setLayoutData(gd_tree);
    }

    protected void setSubtreeChecked(final DebugTreeItem dti,
            final boolean checked) {
        final List<DebugTreeItem> children = dti.getChildren();
        if (children == null || children.size() == 0) {
            interpretOrDeinterpret(dti, checked);
            return;
        }
        for (final DebugTreeItem i : children) {
            checkboxTreeViewer.setChecked(i, checked);
            setSubtreeChecked(i, checked);
        }
    }

    private void interpretOrDeinterpret(final DebugTreeItem dti,
            final boolean checked) {
        final IErlModule m = (IErlModule) dti.getItem();
        if (checked) {
            interpretedModules.add(m);
        } else {
            interpretedModules.remove(m);
        }
    }

    public static void checkUpwards(final CheckboxTreeViewer ctv,
            final DebugTreeItem dti, final boolean checked, final boolean grayed) {
        for (DebugTreeItem parent = dti.getParent(); parent != null; parent = parent
                .getParent()) {
            ctv.setChecked(parent, checked);
        }
    }

    @Override
    public void setDefaults(final ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                new ArrayList<String>());
        config.setAttribute(ErlLaunchAttributes.DEBUG_FLAGS,
                ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
    }

    /**
     * Find modules from string list add to IFile-list
     * 
     * @param interpret
     *            the list of strings from prefs (projectName:fileName;... or
     *            moduleName;...)
     * @param interpretedModules
     *            collection that the IFile-s are added to
     */
    public static void addModules(final Collection<String> interpret,
            final Collection<IErlModule> interpretedModules) {
        final IErlModel model = ErlModelManager.getErlangModel();
        for (final String i : interpret) {
            final String[] pm = i.split(":");
            IErlModule module = null;
            if (pm.length > 1) {
                final IErlProject p = (IErlProject) model.getChildNamed(pm[0]);
                if (p != null) {
                    final String mName = pm[1];
                    try {
                        final boolean isErlangFile = CommonUtils
                                .isErlangFileContentFileName(mName);
                        final String s = isErlangFile ? mName : mName + ".erl";
                        module = p.getModule(s);
                    } catch (final ErlModelException e) {
                        ErlLogger.warn(e);
                    }
                }
            } else {
                try {
                    module = model.findModule(i);
                } catch (final ErlModelException e) {
                }
            }
            if (module != null) {
                if (!interpretedModules.contains(module)) {
                    interpretedModules.add(module);
                }
            }
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public void initializeFrom(final ILaunchConfiguration config) {
        List<String> interpret;
        String prjs;
        try {
            interpret = config.getAttribute(
                    ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                    new ArrayList<String>());
            prjs = config.getAttribute(ErlLaunchAttributes.PROJECTS, "").trim();
        } catch (final CoreException e1) {
            interpret = new ArrayList<String>();
            prjs = "";
        }
        final String[] projectNames = prjs.length() == 0 ? new String[] {}
                : prjs.split(";");
        final Set<IProject> projects = new HashSet<IProject>();
        for (final String s : projectNames) {
            final IProject project = ResourcesPlugin.getWorkspace().getRoot()
                    .getProject(s);
            if (project == null) {
                continue;
            }
            projects.add(project);
        }

        interpret = ErlangLaunchDelegate.addBreakpointProjectsAndModules(
                projects, interpret);
        interpretedModules = new ArrayList<IErlModule>();

        addModules(interpret, interpretedModules);

        int debugFlags;
        try {
            debugFlags = config.getAttribute(ErlLaunchAttributes.DEBUG_FLAGS,
                    ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
        } catch (final CoreException e) {
            debugFlags = ErlDebugConstants.DEFAULT_DEBUG_FLAGS;
        }
        setFlagCheckboxes(debugFlags);

        if (checkboxTreeViewer != null) {
            checkboxTreeViewer.setInput(config);
            final DebugTreeItem root = ((TreeContentProvider) checkboxTreeViewer
                    .getContentProvider()).getRoot();
            root.setChecked(checkboxTreeViewer, interpretedModules);
            checkboxTreeViewer.expandAll();
        }
    }

    @Override
    public void performApply(final ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(ErlLaunchAttributes.DEBUG_FLAGS,
                getFlagCheckboxes());
        final List<String> r = new ArrayList<String>();
        for (final IErlModule m : interpretedModules) {
            r.add(m.getProject().getName() + ":" + m.getName());
        }
        config.setAttribute(ErlLaunchAttributes.DEBUG_INTERPRET_MODULES, r);
    }

    @Override
    public String getName() {
        return "Debug";
    }

    @Override
    public boolean isValid(final ILaunchConfiguration config) {
        return true;
    }

    /**
     * check or uncheck the four flag checkboxes
     * 
     * @param debugFlags
     *            flags
     */
    private void setFlagCheckboxes(final int debugFlags) {
        if (attachOnFirstCallCheck == null) {
            // I don't know why these are null sometimes...
            return;
        }
        int flag = debugFlags & ErlDebugConstants.ATTACH_ON_FIRST_CALL;
        attachOnFirstCallCheck.setSelection(flag != 0);
        flag = debugFlags & ErlDebugConstants.ATTACH_ON_BREAKPOINT;
        attachOnBreakpointCheck.setSelection(flag != 0);
        flag = debugFlags & ErlDebugConstants.ATTACH_ON_EXIT;
        attachOnExitCheck.setSelection(flag != 0);
        flag = debugFlags & ErlDebugConstants.DISTRIBUTED_DEBUG;
        distributedDebugCheck.setSelection(flag != 0);
    }

    /**
     * get flag settings by reading checkboxes
     * 
     * @return flags as int
     */
    private int getFlagCheckboxes() {
        int result = 0;
        if (attachOnFirstCallCheck.getSelection()) {
            result |= ErlDebugConstants.ATTACH_ON_FIRST_CALL;
        }
        if (attachOnBreakpointCheck.getSelection()) {
            result |= ErlDebugConstants.ATTACH_ON_BREAKPOINT;
        }
        if (attachOnExitCheck.getSelection()) {
            result |= ErlDebugConstants.ATTACH_ON_EXIT;
        }
        if (distributedDebugCheck.getSelection()) {
            result |= ErlDebugConstants.DISTRIBUTED_DEBUG;
        }
        return result;
    }

    private final SelectionListener fBasicSelectionListener = new SelectionListener() {
        @Override
        @SuppressWarnings("synthetic-access")
        public void widgetDefaultSelected(final SelectionEvent e) {
            updateLaunchConfigurationDialog();
        }

        @Override
        @SuppressWarnings("synthetic-access")
        public void widgetSelected(final SelectionEvent e) {
            updateLaunchConfigurationDialog();
        }
    };

}
