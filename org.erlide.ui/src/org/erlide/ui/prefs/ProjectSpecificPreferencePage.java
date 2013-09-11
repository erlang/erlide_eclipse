package org.erlide.ui.prefs;

import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.dialogs.ControlEnableState;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.dialogs.PropertyPage;

import com.google.common.collect.Sets;

public abstract class ProjectSpecificPreferencePage extends PropertyPage {

    protected IProject fProject;
    protected Button fUseProjectSettings;
    protected Link fChangeWorkspaceSettings;
    protected Composite prefsComposite;
    protected ControlEnableState fBlockEnableState;

    public ProjectSpecificPreferencePage() {
        super();
    }

    protected boolean isProjectPreferencePage() {
        return fProject != null;
    }

    protected void enableProjectSpecificSettings(
            final boolean useProjectSpecificSettings) {
        fUseProjectSettings.setSelection(useProjectSpecificSettings);
        enablePreferenceContent(useProjectSpecificSettings);
        fChangeWorkspaceSettings.setEnabled(!useProjectSpecificSettings);
        // doStatusChanged();
    }

    private void enablePreferenceContent(
            final boolean useProjectSpecificSettings) {
        if (useProjectSpecificSettings) {
            if (fBlockEnableState != null) {
                fBlockEnableState.restore();
                fBlockEnableState = null;
            }
        } else {
            if (fBlockEnableState == null) {
                fBlockEnableState = ControlEnableState.disable(prefsComposite);
            }
        }
    }

    protected void createProjectSpecificSettingsCheckBoxAndLink(
            final Composite parent) {
        if (isProjectPreferencePage()) {
            final Composite composite = new Composite(parent, SWT.NONE);
            composite.setFont(parent.getFont());
            final GridLayout layout = new GridLayout(2, false);
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            composite.setLayout(layout);
            composite.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                    false));

            // final IDialogFieldListener listener = new IDialogFieldListener()
            // {
            // public void dialogFieldChanged(final DialogField field) {
            // final boolean enabled = ((SelectionButtonDialogField) field)
            // .isSelected();
            // enableProjectSpecificSettings(enabled);
            //
            // if (enabled && getData() != null) {
            // applyData(getData());
            // }
            // }
            // };

            fUseProjectSettings = new Button(composite, SWT.CHECK);
            fUseProjectSettings.setText("Enable project specific settings");
            fUseProjectSettings.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(final SelectionEvent e) {
                    final boolean sel = fUseProjectSettings.getSelection();
                    enableProjectSpecificSettings(sel);
                    super.widgetSelected(e);
                }
            });
            // fUseProjectSettings.setDialogFieldListener(listener);
            // fUseProjectSettings
            // .setLabelText(PreferencesMessages.PropertyAndPreferencePage_useprojectsettings_label);
            // LayoutUtil.setHorizontalGrabbing(fUseProjectSettings
            // .getSelectionButton(null));

            if (true) { // if (offerLink()) {
                fChangeWorkspaceSettings = createLink(composite,
                        "Configure Workspace settings...");
                fChangeWorkspaceSettings.setLayoutData(new GridData(SWT.END,
                        SWT.CENTER, false, false));
            }
            // else {
            // LayoutUtil.setHorizontalSpan(fUseProjectSettings
            // .getSelectionButton(null), 2);
            // }

            final Label horizontalLine = new Label(composite, SWT.SEPARATOR
                    | SWT.HORIZONTAL);
            horizontalLine.setLayoutData(new GridData(GridData.FILL,
                    GridData.FILL, true, false, 2, 1));
            horizontalLine.setFont(composite.getFont());
        } else { // if (supportsProjectSpecificOptions() && offerLink()) {
            fChangeWorkspaceSettings = createLink(parent,
                    "Configure project specific settings..");
            fChangeWorkspaceSettings.setLayoutData(new GridData(SWT.END,
                    SWT.CENTER, true, false));
        }

    }

    Link createLink(final Composite composite, final String theText) {
        final Link link = new Link(composite, SWT.NONE);
        link.setFont(composite.getFont());
        link.setText("<A>" + theText + "</A>"); //$NON-NLS-1$//$NON-NLS-2$
        link.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                doLinkActivated((Link) e.widget);
            }

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                doLinkActivated((Link) e.widget);
            }
        });
        return link;
    }

    protected void openProjectProperties(final IProject project) {
        final String id = getPropertyPageID();
        if (id != null) {
            PreferencesUtil.createPropertyDialogOn(getShell(), project, id,
                    new String[] { id }, null).open();
        }
    }

    protected abstract String getPropertyPageID();

    protected final void openWorkspacePreferences(final Object data) {
        final String id = getPreferencePageID();
        PreferencesUtil.createPreferenceDialogOn(getShell(), id,
                new String[] { id }, data).open();
    }

    protected abstract String getPreferencePageID();

    void doLinkActivated(final Link widget) {
        if (isProjectPreferencePage()) {
            openWorkspacePreferences(null);
        } else {
            openProjectPreferences();
        }
    }

    protected void openProjectPreferences() {
        final Set<IProject> projectsWithSpecifics = Sets.newHashSet();
        Set<IProject> openProjects = Sets.newHashSet();
        IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
        for (IProject p : workspaceRoot.getProjects()) {
            if (p.isAccessible()) {
                openProjects.add(p);
                if (hasProjectSpecificOptions(p))
                    projectsWithSpecifics.add(p);
            }
        }
        final ProjectSelectionDialog dialog = new ProjectSelectionDialog(
                getShell(), openProjects, projectsWithSpecifics);
        if (dialog.open() == Window.OK) {
            final IProject res = (IProject) dialog.getFirstResult();
            openProjectProperties(res);
        }
    }

    protected abstract boolean hasProjectSpecificOptions(IProject p);

}
