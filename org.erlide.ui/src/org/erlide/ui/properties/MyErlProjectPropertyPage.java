/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.ui.ErlideUIPlugin;

import com.bdaum.overlayPages.FieldEditorOverlayPage;
import com.bdaum.overlayPages.OverlayPage;

public class MyErlProjectPropertyPage extends OverlayPage {

	public MyErlProjectPropertyPage() {
		super();
	}

	@Override
	protected IPreferenceStore doGetPreferenceStore() {
		return ErlideUIPlugin.getDefault().getPreferenceStore();
	}

	private List list;
	private Combo combo;
	private Text text_1;
	private Text backendCookie;
	private Text backendName;
	private TabFolder tabFolder;
	private ErlangProjectProperties prefs;
	private PathEditor fextinc;
	private PathEditor fSourceEditor;
	private PathEditor fIncludeEditor;
	private PathEditor fExternalIncludeEditor;

	@Override
	protected Control createContents(Composite aparent) {
		Composite parent = (Composite) super.createContents(aparent);

		// create the composite to hold the widgets
		final Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FillLayout());

		// fextinc = new PathEditor("ext include", "ext inc", "New",
		// this.composite_1);

		this.tabFolder = new TabFolder(composite, SWT.NONE);

		final TabItem sourceTab = new TabItem(this.tabFolder, SWT.NONE);
		sourceTab.setText("Source");

		final TabItem t2 = new TabItem(this.tabFolder, SWT.NONE);
		t2.setText("Include");

		final Composite includeComposite = new Composite(this.tabFolder,
				SWT.NONE);
		includeComposite.setBounds(0, 0, 443, 305);
		includeComposite.setLayout(new GridLayout());
		t2.setControl(includeComposite);
		final Composite composite_5 = new Composite(includeComposite, SWT.NONE);
		final GridData gd_composite_5 = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		composite_5.setLayoutData(gd_composite_5);
		composite_5.setLayout(new GridLayout());

		final Composite composite_3 = new Composite(includeComposite, SWT.NONE);
		final GridData gd_composite_3 = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		composite_3.setLayoutData(gd_composite_3);
		composite_3.setLayout(new GridLayout());

		fExternalIncludeEditor = new PathEditor("ext include",
				"External include directories:", "New", composite_3);
		fIncludeEditor = new PathEditor("ext include",
				"Project include directories:", "New", composite_5);

		final TabItem t3 = new TabItem(this.tabFolder, SWT.NONE);
		t3.setText("Dependencies");

		final Composite composite_1 = new Composite(this.tabFolder, SWT.NONE);
		composite_1.setLayout(new GridLayout());
		t3.setControl(composite_1);

		final Composite sourceComposite = new Composite(this.tabFolder,
				SWT.NONE);
		sourceComposite.setBounds(0, 0, 443, 305);
		final GridLayout gridLayout = new GridLayout();

		sourceComposite.setLayout(gridLayout);
		sourceTab.setControl(sourceComposite);

		final Composite composite_2 = new Composite(sourceComposite, SWT.NONE);
		final GridData gd_composite_2 = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		composite_2.setLayoutData(gd_composite_2);
		composite_2.setLayout(new GridLayout());

		fSourceEditor = new PathEditor("sources",
				"Source directories for this project:", "New", composite_2);

		final Composite composite_8 = new Composite(sourceComposite, SWT.NONE);
		composite_8
				.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false));
		composite_8.setLayout(new GridLayout());

		new StringFieldEditor("output", "Output directory:", composite_8);

		final TabItem backendTab = new TabItem(this.tabFolder, SWT.NONE);
		backendTab.setText("Backend");

		final Composite composite_6 = new Composite(this.tabFolder, SWT.NONE);
		final GridLayout gridLayout_1 = new GridLayout();
		gridLayout_1.numColumns = 2;
		composite_6.setLayout(gridLayout_1);
		backendTab.setControl(composite_6);

		final Label runtimeLabel = new Label(composite_6, SWT.NONE);
		runtimeLabel.setLayoutData(new GridData());
		runtimeLabel.setText("Runtime");

		combo = new Combo(composite_6, SWT.NONE);
		combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final Label nodeNameLabel = new Label(composite_6, SWT.NONE);
		nodeNameLabel.setText("Node name");

		this.backendName = new Text(composite_6, SWT.BORDER);
		final GridData gd_backendName = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		this.backendName.setLayoutData(gd_backendName);

		final Label nodeCookieLabel = new Label(composite_6, SWT.NONE);
		nodeCookieLabel.setText("Node cookie");

		this.backendCookie = new Text(composite_6, SWT.BORDER);
		final GridData gd_backendCookie = new GridData(SWT.FILL, SWT.CENTER,
				true, false);
		this.backendCookie.setLayoutData(gd_backendCookie);

		final Label extraArgsLabel = new Label(composite_6, SWT.NONE);
		extraArgsLabel.setText("Extra args");

		text_1 = new Text(composite_6, SWT.BORDER);
		text_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final TabItem codepathTabItem = new TabItem(tabFolder, SWT.NONE);
		codepathTabItem.setText("Codepath");

		final Composite composite_4 = new Composite(tabFolder, SWT.NONE);
		final GridLayout gridLayout_2 = new GridLayout();
		gridLayout_2.numColumns = 2;
		composite_4.setLayout(gridLayout_2);
		codepathTabItem.setControl(composite_4);

		final Label codepathOrderLabel = new Label(composite_4, SWT.NONE);
		codepathOrderLabel.setText("Code:path order");
		new Label(composite_4, SWT.NONE);

		list = new List(composite_4, SWT.BORDER);
		final GridData gd_list = new GridData(SWT.FILL, SWT.CENTER, false,
				false);
		gd_list.heightHint = 189;
		gd_list.widthHint = 383;
		list.setLayoutData(gd_list);

		final Composite composite_7 = new Composite(composite_4, SWT.NONE);
		composite_7
				.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false));
		composite_7.setLayout(new GridLayout());

		final Button button = new Button(composite_7, SWT.NONE);
		button.setLayoutData(new GridData(67, SWT.DEFAULT));
		button.setText("Add...");

		final Button removeButton = new Button(composite_7, SWT.NONE);
		removeButton.setLayoutData(new GridData(67, SWT.DEFAULT));
		removeButton.setText("Remove");

		final Button moveUpButton = new Button(composite_7, SWT.NONE);
		moveUpButton.setLayoutData(new GridData(67, SWT.DEFAULT));
		moveUpButton.setText("Move up");

		final Button moveDownButton = new Button(composite_7, SWT.NONE);
		moveDownButton.setText("Move down");

		return parent;
	}

	@Override
	protected String getPageId() {
		return "org.erlide.ui.properties.myErlangProjectPropertyPage";
	}

	public void init(IWorkbench workbench) {
		setDescription("These values will be used as defaults for newly created Erlang projects.");
	}

	public static String getOverlayedPreferenceValue(IPreferenceStore store,
			IResource resource, String pageId, String key) {
		IProject project = resource.getProject();
		String value = null;
		if (useProjectSettings(project, pageId)) {
			value = getProperty(resource, pageId, key);
		}
		if (value != null)
			return value;
		return store.getString(key);
	}

	private static boolean useProjectSettings(IResource resource, String pageId) {
		String use = getProperty(resource, pageId,
				FieldEditorOverlayPage.USEPROJECTSETTINGS);
		return "true".equals(use);
	}

	private static String getProperty(IResource resource, String pageId,
			String key) {
		try {
			return resource
					.getPersistentProperty(new QualifiedName(pageId, key));
		} catch (CoreException e) {
		}
		return null;
	}

	@Override
	public boolean performOk() {
		return super.performOk();
	}

	@Override
	public IAdaptable getElement() {
		final IProject prj = (IProject) super.getElement();
		prefs = new ErlangProjectProperties(prj);
		return prj;
	}
}
