package org.erlide.debug.ui.actions;

import java.util.ArrayList;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;
import org.erlide.ui.launch.DebugTab;
import org.erlide.ui.launch.DebugTab.TreeContentProvider;
import org.erlide.ui.launch.DebugTab.TreeLabelProvider;
import org.erlide.ui.launch.DebugTreeItem;
import org.erlide.ui.properties.ErlTracingPropertyPage;

public class ErlangTracingDialog extends Dialog {

    private ArrayList<IErlModule> tracedModules;
    private CheckboxTreeViewer checkboxTreeViewer;

    protected ErlangTracingDialog(final Shell parentShell) {
        super(parentShell);
        setShellStyle(getShellStyle() | SWT.RESIZE | SWT.MAX);
    }

    @Override
    public void create() {

        super.create();

        final Shell shell = getShell();
        // shell.addShellListener(fActivationListener);

        // set help context
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(shell, IErlangHelpContextIds.ERLANG_TRACING_DIALOG);

        // fill in combo contents
        // fFindField.removeModifyListener(fFindModifyListener);
        // updateCombo(fFindField, fFindHistory);
        // fFindField.addModifyListener(fFindModifyListener);
        // updateCombo(fReplaceField, fReplaceHistory);

        // get find string
        // initFindStringFromSelection();

        // set dialog position
        // if (fDialogPositionInit != null)
        // shell.setBounds(fDialogPositionInit);

        // shell.setText(EditorMessages.FindReplace_title);
        // shell.setImage(null);
    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        // create composite
        final Composite comp = (Composite) super.createDialogArea(parent);
        tracedModules = new ArrayList<IErlModule>();

        final GridLayout topLayout = new GridLayout();
        comp.setLayout(topLayout);

        final Group tracedModulesGroup = new Group(comp, SWT.NONE);
        tracedModulesGroup.setText("Traced modules");
        final GridData gd_interpretedModulesGroup = new GridData();
        tracedModulesGroup.setLayoutData(gd_interpretedModulesGroup);
        tracedModulesGroup.setLayout(new GridLayout());

        checkboxTreeViewer = new CheckboxTreeViewer(tracedModulesGroup,
                SWT.BORDER);
        checkboxTreeViewer.addCheckStateListener(new ICheckStateListener() {
            @Override
            @SuppressWarnings("synthetic-access")
            public void checkStateChanged(final CheckStateChangedEvent event) {
                final DebugTreeItem dti = (DebugTreeItem) event.getElement();
                checkboxTreeViewer.setGrayed(dti, false);
                final boolean checked = event.getChecked();
                ErlTracingPropertyPage.setSubtreeChecked(dti, checked,
                        tracedModules, checkboxTreeViewer);
                DebugTab.checkUpwards(checkboxTreeViewer, dti, checked, false);
            }

        });
        checkboxTreeViewer.setLabelProvider(new TreeLabelProvider());
        checkboxTreeViewer.setContentProvider(new TreeContentProvider());
        final Tree tree = checkboxTreeViewer.getTree();
        final GridData gd_tree = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd_tree.minimumWidth = 250;
        gd_tree.minimumHeight = 120;
        gd_tree.widthHint = 256;
        gd_tree.heightHint = 220;
        tree.setLayoutData(gd_tree);

        applyDialogFont(comp);
        return comp;
    }

    public void initializeFrom(final ILaunch launch) {
        // final List<String> traces;
        // String prjs;
        // try {
        // trace = config.getAttribute(
        // IErlLaunchAttributes.DEBUG_TRACED_MODULES,
        // new ArrayList<String>());
        // prjs = config.getAttribute(IErlLaunchAttributes.PROJECTS, "")
        // .trim();
        // } catch (final CoreException e1) {
        // trace = new ArrayList<String>();
        // prjs = "";
        // }
        // final String[] projectNames = prjs.length() == 0 ? new String[] {}
        // : prjs.split(";");
        // final Set<IProject> projects = new HashSet<IProject>();
        // for (final String s : projectNames) {
        // final IProject project = ResourcesPlugin.getWorkspace().getRoot()
        // .getProject(s);
        // if (project == null) {
        // continue;
        // }
        // projects.add(project);
        // }

        tracedModules = new ArrayList<IErlModule>();

        // DebugTab.addModules(trace, tracedModules);

        if (checkboxTreeViewer != null) {
            // checkboxTreeViewer.setInput(config);
            final DebugTreeItem root = ((TreeContentProvider) checkboxTreeViewer
                    .getContentProvider()).getRoot();
            root.setChecked(checkboxTreeViewer, tracedModules);
            checkboxTreeViewer.expandAll();
        }
    }
}
