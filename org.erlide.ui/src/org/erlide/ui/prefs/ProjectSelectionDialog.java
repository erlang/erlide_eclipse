package org.erlide.ui.prefs;

import java.util.Collection;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.SelectionStatusDialog;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.StatusInfo;

public class ProjectSelectionDialog extends SelectionStatusDialog {

    final static class OurLabelProvider implements ILabelProvider {
        @Override
        public Image getImage(final Object element) {
            return null;
        }

        @Override
        public String getText(final Object element) {
            final IProject p = (IProject) element;
            return p.getName();
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
            return true;
        }

        @Override
        public void removeListener(final ILabelProviderListener listener) {
        }
    }

    final class OurContentProvider implements IStructuredContentProvider {
        @Override
        public void dispose() {
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
        }

        @Override
        public Object[] getElements(final Object inputElement) {
            return fErlProjects.toArray();
        }
    }

    // the visual selection widget group
    private TableViewer fTableViewer;
    private final Collection<IProject> fErlProjects;
    private final Set<IProject> fProjectsWithSpecifics;

    // sizing constants
    private final static int SIZING_SELECTION_WIDGET_HEIGHT = 250;
    private final static int SIZING_SELECTION_WIDGET_WIDTH = 300;

    private static final String DIALOG_SETTINGS = "org.erlide.ui.dialogs.ProjectSelectionDialog"; //$NON-NLS-1$
    private final static String DIALOG_SETTINGS_SHOW_ALL = "ProjectSelectionDialog.show_all"; //$NON-NLS-1$

    private final ViewerFilter fFilter;

    public ProjectSelectionDialog(final Shell parentShell,
            final Collection<IProject> erlProjects,
            final Set<IProject> projectsWithSpecifics) {
        super(parentShell);
        setTitle("Project Specific Configuration");
        setMessage("&Select the project to configure");
        fErlProjects = erlProjects;
        fProjectsWithSpecifics = projectsWithSpecifics;
        fFilter = new ViewerFilter() {
            @Override
            public boolean select(final Viewer viewer,
                    final Object parentElement, final Object element) {
                return fProjectsWithSpecifics.contains(element);
            }
        };
    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        // page group
        final Composite composite = (Composite) super.createDialogArea(parent);

        final Font font = parent.getFont();
        composite.setFont(font);

        createMessageArea(composite);

        fTableViewer = new TableViewer(composite, SWT.H_SCROLL | SWT.V_SCROLL
                | SWT.BORDER);
        fTableViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    @Override
                    public void selectionChanged(
                            final SelectionChangedEvent event) {
                        doSelectionChanged(((IStructuredSelection) event
                                .getSelection()).toArray());
                    }
                });
        fTableViewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(final DoubleClickEvent event) {
                okPressed();
            }
        });
        final GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.heightHint = SIZING_SELECTION_WIDGET_HEIGHT;
        data.widthHint = SIZING_SELECTION_WIDGET_WIDTH;
        fTableViewer.getTable().setLayoutData(data);

        fTableViewer.setLabelProvider(new OurLabelProvider());
        fTableViewer.setContentProvider(new OurContentProvider());
        fTableViewer.setComparator(new ViewerComparator());
        fTableViewer.getControl().setFont(font);

        final Button checkbox = new Button(composite, SWT.CHECK);
        checkbox.setText("Show only &projects with project specific settings");
        checkbox.setLayoutData(new GridData(SWT.BEGINNING, SWT.CENTER, true,
                false));
        checkbox.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateFilter(((Button) e.widget).getSelection());
            }

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                updateFilter(((Button) e.widget).getSelection());
            }
        });
        final IDialogSettings dialogSettings = ErlideUIPlugin.getDefault()
                .getDialogSettingsSection(DIALOG_SETTINGS);
        final boolean doFilter = !dialogSettings
                .getBoolean(DIALOG_SETTINGS_SHOW_ALL)
                && !fProjectsWithSpecifics.isEmpty();
        checkbox.setSelection(doFilter);
        updateFilter(doFilter);

        fTableViewer.setInput(new Object());

        doSelectionChanged(new Object[0]);
        Dialog.applyDialogFont(composite);
        return composite;
    }

    protected void updateFilter(final boolean selected) {
        if (selected) {
            fTableViewer.addFilter(fFilter);
        } else {
            fTableViewer.removeFilter(fFilter);
        }
        final IDialogSettings dialogSettings = ErlideUIPlugin.getDefault()
                .getDialogSettingsSection(DIALOG_SETTINGS);
        dialogSettings.put(DIALOG_SETTINGS_SHOW_ALL, !selected);
    }

    void doSelectionChanged(final Object[] objects) {
        if (objects.length != 1) {
            updateStatus(new StatusInfo(IStatus.ERROR, "")); //$NON-NLS-1$
            setSelectionResult(null);
        } else {
            updateStatus(new StatusInfo());
            setSelectionResult(objects);
        }
    }

    @Override
    protected void computeResult() {
    }
}
