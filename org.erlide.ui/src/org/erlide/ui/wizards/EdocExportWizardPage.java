/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.ui.wizards;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.erlide.core.ErlangCore;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class EdocExportWizardPage extends WizardPage {

    private CheckboxTableViewer checkboxTableViewer;

    static class TableLabelProvider extends LabelProvider implements
            ITableLabelProvider {
        @Override
        public String getColumnText(final Object element, final int columnIndex) {
            return ((IProject) element).getName();
        }

        @Override
        public Image getColumnImage(final Object element, final int columnIndex) {
            return null;
        }
    }

    static class TableContentProvider implements IStructuredContentProvider {

        @Override
        public Object[] getElements(final Object inputElement) {
            final java.util.List<IProject> ps = new ArrayList<IProject>();

            final IProject[] projects = ResourcesPlugin.getWorkspace()
                    .getRoot().getProjects();
            for (final IProject p : projects) {
                if (p.isAccessible()) {
                    IProjectNature n = null;
                    try {
                        n = p.getNature(ErlangCore.NATURE_ID);
                        if (n != null) {
                            ps.add(p);
                        }
                    } catch (final CoreException e) {
                    }
                }
            }
            return ps.toArray(new IProject[0]);
        }

        @Override
        public void dispose() {
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
        }

    }

    private Text destination;
    private Table table;

    protected EdocExportWizardPage(final String pageName,
            final IStructuredSelection selection) {
        super(pageName);
        setTitle("eDoc export (work in progress!)");
        setDescription("Select the projects and files for which you want to generate eDoc documentation");
    }

    @Override
    public void createControl(final Composite parent) {
        final Composite composite = new Composite(parent, SWT.NONE);
        final GridLayout gridLayout = new GridLayout();
        composite.setLayout(gridLayout);
        setControl(composite);

        final Label selectProjectsForLabel = new Label(composite, SWT.NONE);
        selectProjectsForLabel.setLayoutData(new GridData());
        selectProjectsForLabel
                .setText("Select projects for which edoc documentation will be generated:");

        checkboxTableViewer = CheckboxTableViewer.newCheckList(composite,
                SWT.BORDER);
        checkboxTableViewer.setContentProvider(new TableContentProvider());
        checkboxTableViewer.setLabelProvider(new TableLabelProvider());
        table = checkboxTableViewer.getTable();
        final GridData gd_table = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd_table.widthHint = 423;
        table.setLayoutData(gd_table);
        checkboxTableViewer.setInput(this);

        final Group optionsGroup = new Group(composite, SWT.NONE);
        optionsGroup.setText("Options");
        final GridData gd_optionsGroup = new GridData(SWT.FILL, SWT.CENTER,
                false, false);
        optionsGroup.setLayoutData(gd_optionsGroup);
        final GridLayout gridLayout_1 = new GridLayout();
        gridLayout_1.numColumns = 2;
        optionsGroup.setLayout(gridLayout_1);

        final Label selectLabel = new Label(optionsGroup, SWT.NONE);
        selectLabel.setText("Destination (relative to each project):");

        destination = new Text(optionsGroup, SWT.BORDER);
        final GridData gd_destination = new GridData(SWT.FILL, SWT.CENTER,
                false, false);
        gd_destination.widthHint = 179;
        destination.setLayoutData(gd_destination);
        destination.setText("doc");
    }

    public Collection<IProject> getSelectedResources() {
        final ArrayList<IProject> result = new ArrayList<IProject>();
        final Object[] sel = checkboxTableViewer.getCheckedElements();
        for (final Object o : sel) {
            result.add((IProject) o);
        }
        return result;
    }

    public Map<String, OtpErlangObject> getOptions() {
        final HashMap<String, OtpErlangObject> result = new HashMap<String, OtpErlangObject>();
        result.put("dir", new OtpErlangString(destination.getText()));
        // result.put("preprocess", new OtpErlangBoolean(false));
        return result;
    }

    public String getDestination() {
        return destination.getText();
    }

}
