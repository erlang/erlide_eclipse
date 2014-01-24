/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.properties;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.beans.PojoProperties;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.swt.WidgetProperties;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;

public class ErlangProjectPropertyPage extends PropertyPage {

    private final ErlangProjectProperties model;
    private Text text;
    private Text text_1;

    public ErlangProjectPropertyPage() {
        super();
        // noDefaultAndApplyButton();
        model = new ErlangProjectProperties();
    }

    protected void createFieldEditors() {
        // final IProject prj = (IProject)
        // getElement().getAdapter(IProject.class);
        // final IErlProject erlPrj =
        // ErlangEngine.getInstance().getModel().findProject(prj);
        // final BuilderConfig config = erlPrj.getBuilderConfig();
        // final boolean readonly = config != BuilderConfig.INTERNAL;
        //
        // try {
        // prj.getFolder(new
        // Path(".settings")).refreshLocal(IResource.DEPTH_ONE, null);
        // } catch (final CoreException e) {
        // }

        // final ProjectDirectoryFieldEditor out = new
        // ProjectDirectoryFieldEditor(
        // ProjectPreferencesConstants.OUTPUT_DIR, "Output directory:",
        // getFieldEditorParent(), prj, false);
        // addField(out);
        // out.setEnabled(!readonly, getFieldEditorParent());
        //
        // final ProjectPathEditor src = new ProjectPathEditor(
        // ProjectPreferencesConstants.SOURCE_DIRS, "Source directories:",
        // "Select directory:", getFieldEditorParent(), prj);
        // addField(src);
        // src.setEnabled(!readonly, getFieldEditorParent());
        //
        // final ProjectPathEditor inc = new ProjectPathEditor(
        // ProjectPreferencesConstants.INCLUDE_DIRS, "Include directories:",
        // "Select directory:", getFieldEditorParent(), prj);
        // addField(inc);
        // inc.setEnabled(!readonly, getFieldEditorParent());
        //
        // // IPreferenceStore ps = getPreferenceStore();
        // // OldErlangProjectProperties props = new
        // // OldErlangProjectProperties(prj);
        // // List<String> tstDirs = props.getTestDirs();
        // // String tstStr = PreferencesUtils.packList(tstDirs);
        // // ps.setValue(ProjectPreferencesConstants.TEST_DIRS, tstStr);
        // //
        // // ProjectPathEditor tst = new ProjectPathEditor(
        // // ProjectPreferencesConstants.TEST_DIRS,
        // // "Test source directories:", "Select directory:",
        // // fieldEditorParent, prj);
        // // tst.setEnabled(false, fieldEditorParent);
        // // addField(tst);
        //
        // final List<String> versions = BackendCore.getRuntimeInfoCatalog()
        // .getAllRuntimesVersions();
        // final String[][] versionsArray = new String[versions.size()][2];
        // for (int i = 0; i < versionsArray.length; i++) {
        // versionsArray[i][0] = versions.get(i);
        // versionsArray[i][1] = versionsArray[i][0];
        // }
        // addField(new
        // ComboFieldEditor(ProjectPreferencesConstants.RUNTIME_VERSION,
        // "Runtime version:", versionsArray, getFieldEditorParent()));

    }

    @Override
    public boolean performOk() {
        final IProject project = (IProject) getElement().getAdapter(IProject.class);
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        erlProject.clearCaches();
        return super.performOk();
    }

    @Override
    protected Control createContents(final Composite parent) {
        final Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(1, false));
        {
            text = new Text(composite, SWT.BORDER);
            text.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
        }
        {
            text_1 = new Text(composite, SWT.BORDER);
            text_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
        }
        initDataBindings();
        return composite;
    }

    protected DataBindingContext initDataBindings() {
        final DataBindingContext bindingContext = new DataBindingContext();
        //
        final IObservableValue observeTextTextObserveWidget = SWTObservables.observeText(
                text, SWT.Modify);
        final IObservableValue outputDirModelObserveValue = PojoProperties.value(
                "outputDir").observe(model);
        final UpdateValueStrategy strategy = new UpdateValueStrategy();
        strategy.setConverter(new StringToIPathConverter());
        bindingContext.bindValue(observeTextTextObserveWidget,
                outputDirModelObserveValue, strategy, null);
        //
        final IObservableValue observeTextText_1ObserveWidget = WidgetProperties.text(
                SWT.Modify).observe(text_1);
        final IObservableValue sourceDirsModelObserveValue = PojoProperties.value(
                "sourceDirs").observe(model);
        final UpdateValueStrategy strategy1 = new UpdateValueStrategy();
        strategy1.setConverter(new StringToIPathListConverter());
        bindingContext.bindValue(observeTextText_1ObserveWidget,
                sourceDirsModelObserveValue, strategy1, null);
        //
        return bindingContext;
    }
}
