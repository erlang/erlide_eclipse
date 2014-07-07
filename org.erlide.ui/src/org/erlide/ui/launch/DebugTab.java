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
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.ErlRuntimeAttributes;
import org.erlide.debug.ui.utils.ModuleItemLabelProvider;
import org.erlide.debug.ui.views.InterpretedModuleListContentProvider;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.runtime.api.ErlDebugFlags;
import org.erlide.ui.dialogs.AddInterpretedModulesSelectionDialog;
import org.erlide.ui.util.SWTUtil;

import com.google.common.collect.Lists;

/**
 * A tab in the Launch Config with erlang debugger parameters: the debug flags
 * for attaching and distruibuted debugging a checkbox tree of modules to
 * interpret upon launching. The checkbox tree classes are reused by
 * InterpretedModulesView
 *
 */
public class DebugTab extends AbstractLaunchConfigurationTab {

    ListViewer listViewer;
    private Button attachOnFirstCallCheck;
    private Button attachOnBreakpointCheck;
    private Button attachOnExitCheck;
    private Button distributedDebugCheck;
    private Button addButton;
    private Button removeButton;
    private InterpretedModuleListContentProvider contentProvider;

    public static class ListLabelProvider extends LabelProvider {

        @Override
        public String getText(final Object element) {
            if (element instanceof IErlElement) {
                final IErlElement erlElement = (IErlElement) element;
                return erlElement.getName();
            }
            return "!" + super.getText(element);
        }

        @Override
        public Image getImage(final Object element) {
            return null;
        }
    }

    /**
     * @wbp.parser.entryPoint
     */
    @Override
    public void createControl(final Composite parent) {
        final Composite comp = new Composite(parent, SWT.NONE);
        setControl(comp);
        final GridLayout topLayout = new GridLayout();
        comp.setLayout(topLayout);

        distributedDebugCheck = createCheckButton(comp, "Debug all connected nodes");

        final Group attachGroup = SWTUtil.createGroup(comp, "Auto Attach", 1,
                GridData.FILL_BOTH);
        attachGroup.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
        attachOnFirstCallCheck = createCheckButton(attachGroup, "First &call");
        attachOnBreakpointCheck = createCheckButton(attachGroup, "&Breakpoint");
        attachOnExitCheck = createCheckButton(attachGroup, "E&xit");

        distributedDebugCheck.addSelectionListener(fBasicSelectionListener);
        attachOnFirstCallCheck.addSelectionListener(fBasicSelectionListener);
        attachOnBreakpointCheck.addSelectionListener(fBasicSelectionListener);
        attachOnExitCheck.addSelectionListener(fBasicSelectionListener);

        final Group interpretedModulesGroup = new Group(comp, SWT.NONE);
        final GridData gd_interpretedModulesGroup = new GridData(SWT.FILL, SWT.CENTER,
                false, false, 2, 1);
        gd_interpretedModulesGroup.widthHint = 387;
        interpretedModulesGroup.setLayoutData(gd_interpretedModulesGroup);
        interpretedModulesGroup.setText("Interpreted modules");
        interpretedModulesGroup.setLayout(new GridLayout(2, false));

        // final Label anyModuleHavingLabel = new Label(interpretedModulesGroup,
        // SWT.WRAP);
        // anyModuleHavingLabel.setLayoutData(new GridData(279, SWT.DEFAULT));
        // anyModuleHavingLabel
        // .setText("Any module having breakpoints enabled will be dynamically added to the list.\n\nThis widget is disabled for now, it takes 100%CPU for large projects. If you need to use \"attach on first call\" or \"attach on exit\", please mark the modules by setting a dummy breakpoint in them. Sorry for the inconvenience!");

        listViewer = new ListViewer(interpretedModulesGroup, SWT.BORDER | SWT.MULTI);
        listViewer.addSelectionChangedListener(new ISelectionChangedListener() {

            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
                final ISelection selection = event.getSelection();
                removeButton.setEnabled(!selection.isEmpty());
            }
        });
        // checkboxTreeViewer.addCheckStateListener(new ICheckStateListener() {
        // @Override
        // @SuppressWarnings("synthetic-access")
        // public void checkStateChanged(final CheckStateChangedEvent event) {
        // final DebugTreeItem dti = (DebugTreeItem) event.getElement();
        // checkboxTreeViewer.setGrayed(dti, false);
        // final boolean checked = event.getChecked();
        // setSubtreeChecked(dti, checked);
        // // checkUpwards(checkboxTreeViewer, dti, checked, false);
        // updateLaunchConfigurationDialog();
        // }
        //
        // });
        listViewer.setLabelProvider(new ModuleItemLabelProvider());
        contentProvider = new InterpretedModuleListContentProvider();
        listViewer.setContentProvider(contentProvider);
        final Control control = listViewer.getControl();
        final GridData gd_list = new GridData(SWT.FILL, SWT.FILL, true, true, 1, 3);
        gd_list.minimumWidth = 250;
        gd_list.minimumHeight = 120;
        gd_list.widthHint = 256;
        gd_list.heightHint = 220;
        control.setLayoutData(gd_list);
        addButton = createPushButton(interpretedModulesGroup, "Add...", null);
        addButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                final List<IErlModule> result = getModulesFromAddModulesDialog(getShell());
                contentProvider.addModules(result);
                listViewer.refresh();
                updateLaunchConfigurationDialog();
            }
        });
        removeButton = createPushButton(interpretedModulesGroup, "Remove", null);
        removeButton.setEnabled(false);
        removeButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                final IStructuredSelection selection = (IStructuredSelection) listViewer
                        .getSelection();
                for (final Object o : selection.toArray()) {
                    contentProvider.removeModule((IErlModule) o);
                }
                listViewer.refresh();
                updateLaunchConfigurationDialog();
            }
        });
    }

    public static List<IErlModule> getModulesFromAddModulesDialog(final Shell shell) {
        final List<IErlModule> result = Lists.newArrayList();
        final AddInterpretedModulesSelectionDialog dialog = new AddInterpretedModulesSelectionDialog(
                shell);
        final int resultCode = dialog.open();
        if (resultCode != IDialogConstants.OK_ID) {
            return result;
        }
        final Object[] dialogResult = dialog.getResult();
        if (dialogResult == null || dialogResult.length == 0) {
            return result;
        }
        final IErlModel model = ErlangEngine.getInstance().getModel();
        for (final Object o : dialogResult) {
            if (o instanceof IFile) {
                final IFile file = (IFile) o;
                result.add(model.findModule(file));
            }
        }
        return result;
    }

    // protected void setSubtreeChecked(final DebugTreeItem dti,
    // final boolean checked) {
    // final List<DebugTreeItem> children = dti.getChildren();
    // if (children == null || children.size() == 0) {
    // interpretOrDeinterpret(dti, checked);
    // return;
    // }
    // for (final DebugTreeItem i : children) {
    // checkboxTreeViewer.setChecked(i, checked);
    // setSubtreeChecked(i, checked);
    // }
    // }

    // public static void checkUpwards(final CheckboxTreeViewer ctv,
    // final DebugTreeItem dti, final boolean checked, final boolean grayed) {
    // for (DebugTreeItem parent = dti.getParent(); parent != null; parent =
    // parent
    // .getParent()) {
    // ctv.setChecked(parent, checked);
    // }
    // }

    @Override
    public void setDefaults(final ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(ErlRuntimeAttributes.DEBUG_INTERPRET_MODULES,
                new ArrayList<String>());
        config.setAttribute(ErlRuntimeAttributes.DEBUG_FLAGS,
                ErlDebugFlags.getFlag(ErlDebugFlags.DEFAULT_DEBUG_FLAGS));
    }

    @Override
    public void initializeFrom(final ILaunchConfiguration config) {
        if (listViewer != null) {
            listViewer.setInput(config);
        }

        final Set<String> interpret = BackendData.addBreakpointProjectsAndModules(null,
                new ArrayList<String>());
        contentProvider.addModules(interpret);

        EnumSet<ErlDebugFlags> debugFlags;
        try {
            final int attribute = config.getAttribute(ErlRuntimeAttributes.DEBUG_FLAGS,
                    ErlDebugFlags.getFlag(ErlDebugFlags.DEFAULT_DEBUG_FLAGS));
            debugFlags = ErlDebugFlags.makeSet(attribute);
        } catch (final CoreException e) {
            debugFlags = ErlDebugFlags.DEFAULT_DEBUG_FLAGS;
        }
        setFlagCheckboxes(debugFlags);
    }

    @Override
    public void performApply(final ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(ErlRuntimeAttributes.DEBUG_FLAGS,
                ErlDebugFlags.getFlag(getFlagCheckboxes()));
        final List<String> r = new ArrayList<String>();
        for (final Object o : contentProvider.getElements(null)) {
            final IErlModule module = (IErlModule) o;
            r.add(ErlangEngine.getInstance().getModelUtilService().getProject(module)
                    .getName()
                    + ":" + module.getName());
        }
        config.setAttribute(ErlRuntimeAttributes.DEBUG_INTERPRET_MODULES, r);
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
    private void setFlagCheckboxes(final EnumSet<ErlDebugFlags> debugFlags) {
        if (attachOnFirstCallCheck == null) {
            // I don't know why these are null sometimes...
            return;
        }
        attachOnFirstCallCheck.setSelection(debugFlags
                .contains(ErlDebugFlags.ATTACH_ON_FIRST_CALL));
        attachOnBreakpointCheck.setSelection(debugFlags
                .contains(ErlDebugFlags.ATTACH_ON_BREAKPOINT));
        attachOnExitCheck.setSelection(debugFlags.contains(ErlDebugFlags.ATTACH_ON_EXIT));
        distributedDebugCheck.setSelection(debugFlags
                .contains(ErlDebugFlags.DISTRIBUTED_DEBUG));
    }

    /**
     * get flag settings by reading checkboxes
     *
     * @return flags as int
     */
    private EnumSet<ErlDebugFlags> getFlagCheckboxes() {
        final EnumSet<ErlDebugFlags> result = EnumSet.noneOf(ErlDebugFlags.class);
        if (attachOnFirstCallCheck.getSelection()) {
            result.add(ErlDebugFlags.ATTACH_ON_FIRST_CALL);
        }
        if (attachOnBreakpointCheck.getSelection()) {
            result.add(ErlDebugFlags.ATTACH_ON_BREAKPOINT);
        }
        if (attachOnExitCheck.getSelection()) {
            result.add(ErlDebugFlags.ATTACH_ON_EXIT);
        }
        if (distributedDebugCheck.getSelection()) {
            result.add(ErlDebugFlags.DISTRIBUTED_DEBUG);
        }
        return result;
    }

    private final SelectionListener fBasicSelectionListener = new SelectionListener() {
        @Override
        public void widgetDefaultSelected(final SelectionEvent e) {
            updateLaunchConfigurationDialog();
        }

        @Override
        public void widgetSelected(final SelectionEvent e) {
            updateLaunchConfigurationDialog();
        }
    };

}
