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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.dialogs.PropertyPage;
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.RuntimeInfo;

/*TODO ...this should implement IworkspacePreferencePage if it's going to be 
 * used as "default erlang project" page...*/
public class ErlProjectPropertyPage extends PropertyPage {

	public ErlProjectPropertyPage() {
		super();
	}

	@Override
	protected IPreferenceStore doGetPreferenceStore() {
		final IPreferenceStore store = new ScopedPreferenceStore(
				new ProjectScope(getProject()), ErlangPlugin.PLUGIN_ID);
		return store;
	}

	private TabFolder tabFolder;
	private Button btnFolder;
	private Button btnSource;
	private Button btnEdit;
	private Text output_text;
	private Button btnAllowOutputFolders;
	private FormData formData_2;
	private Label lblSourceFoldersIn;
	private FormData formData_3;
	private FormData formData_4;
	private Button btnRemove;
	private Button btnBrowse;
	private FormData formData_5;
	private FormData formData_6;
	private FormData formData_1;

	@Override
	protected Control createContents(final Composite parent) {
		final Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FillLayout(SWT.HORIZONTAL));
		tabFolder = new TabFolder(composite, SWT.NONE);

		// /////////////////////////////////////
		final TabItem sourceTab = new TabItem(tabFolder, SWT.NONE);
		sourceTab.setText("Source");

		final Composite sourceComposite = new Composite(tabFolder, SWT.NONE);
		sourceComposite.setLayout(new FormLayout());
		sourceComposite.setBounds(0, 0, 443, 305);
		sourceTab.setControl(sourceComposite);
		{
			lblSourceFoldersIn = new Label(sourceComposite, SWT.NONE);
			{
				formData_2 = new FormData();
				formData_2.left = new FormAttachment(0, 8);
				lblSourceFoldersIn.setLayoutData(formData_2);
			}
			lblSourceFoldersIn.setText("Source folders in the project:");
		}
		{
			btnFolder = new Button(sourceComposite, SWT.NONE);
			formData_2.bottom = new FormAttachment(btnFolder, -6);
			formData_2.right = new FormAttachment(btnFolder, 0, SWT.RIGHT);
			{
				formData_5 = new FormData();
				formData_5.left = new FormAttachment(0, 353);
				formData_5.width = 84;
				btnFolder.setLayoutData(formData_5);
			}
			btnFolder.setText("Add folder...");
		}
		{
			btnSource = new Button(sourceComposite, SWT.NONE);
			formData_5.bottom = new FormAttachment(btnSource, -6);
			btnSource.setEnabled(false);
			{
				FormData formData = new FormData();
				formData.left = new FormAttachment(0, 353);
				formData.right = new FormAttachment(100, -23);
				formData.top = new FormAttachment(0, 62);
				formData.width = 73;
				btnSource.setLayoutData(formData);
			}
			btnSource.setText("Link source...");
		}
		{
			btnEdit = new Button(sourceComposite, SWT.NONE);
			{
				formData_4 = new FormData();
				formData_4.left = new FormAttachment(0, 353);
				formData_4.right = new FormAttachment(100, -23);
				formData_4.width = 73;
				btnEdit.setLayoutData(formData_4);
			}
			btnEdit.setText("Edit...");
		}
		{
			btnRemove = new Button(sourceComposite, SWT.NONE);
			formData_4.bottom = new FormAttachment(btnRemove, -6);
			{
				FormData formData = new FormData();
				formData.left = new FormAttachment(0, 353);
				formData.right = new FormAttachment(100, -23);
				formData.width = 73;
				formData.top = new FormAttachment(0, 134);
				btnRemove.setLayoutData(formData);
			}
			btnRemove.setText("Remove");
		}
		{
			btnAllowOutputFolders = new Button(sourceComposite, SWT.CHECK);
			{
				formData_6 = new FormData();
				formData_6.left = new FormAttachment(0, 10);
				btnAllowOutputFolders.setLayoutData(formData_6);
			}
			btnAllowOutputFolders
					.setText("Allow output folders for source folders");
		}
		{
			output_text = new Text(sourceComposite, SWT.BORDER);
			formData_6.bottom = new FormAttachment(output_text, -6);
			{
				formData_3 = new FormData();
				formData_3.left = new FormAttachment(0, 10);
				formData_3.top = new FormAttachment(0, 268);
				output_text.setLayoutData(formData_3);
			}
		}
		{
			btnBrowse = new Button(sourceComposite, SWT.NONE);
			formData_3.right = new FormAttachment(btnBrowse, -6);
			{
				FormData formData = new FormData();
				formData.right = new FormAttachment(100, -23);
				formData.left = new FormAttachment(0, 353);
				formData.bottom = new FormAttachment(output_text, 0, SWT.BOTTOM);
				btnBrowse.setLayoutData(formData);
			}
			btnBrowse.setText("Browse...");
		}
		{
			TreeViewer treeViewer = new TreeViewer(sourceComposite, SWT.BORDER);
			final Tree sources_tree = treeViewer.getTree();
			{
				FormData formData = new FormData();
				formData.top = new FormAttachment(lblSourceFoldersIn, 6);
				formData.bottom = new FormAttachment(btnAllowOutputFolders, -6);
				formData.right = new FormAttachment(btnRemove, -6);
				formData.left = new FormAttachment(0, 10);
				sources_tree.setLayoutData(formData);
			}
		}
		sourceComposite.setTabList(new Control[] { btnFolder, btnSource,
				btnEdit, btnRemove, btnAllowOutputFolders, output_text,
				btnBrowse });

		final TabItem includeTab = new TabItem(tabFolder, SWT.NONE);
		includeTab.setText("Include");

		final Composite includeComposite = new Composite(tabFolder, SWT.NONE);
		includeComposite.setLayout(new FormLayout());
		includeComposite.setBounds(0, 0, 443, 305);
		includeTab.setControl(includeComposite);

		final Label lblFoldersWithInclude = new Label(includeComposite,
				SWT.NONE);
		{
			FormData formData = new FormData();
			formData.right = new FormAttachment(100, -10);
			formData.top = new FormAttachment(0, 10);
			formData.left = new FormAttachment(0, 10);
			lblFoldersWithInclude.setLayoutData(formData);
		}
		lblFoldersWithInclude.setText("Folders with include files:");

		final Button btnAddFolder = new Button(includeComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 309);
			formData.top = new FormAttachment(lblFoldersWithInclude, 6);
			btnAddFolder.setLayoutData(formData);
		}
		btnAddFolder.setText("Add folder...");

		final Button btnEdit_1 = new Button(includeComposite, SWT.NONE);
		{
			formData_1 = new FormData();
			formData_1.right = new FormAttachment(btnAddFolder, 0, SWT.RIGHT);
			formData_1.width = 77;
			btnEdit_1.setLayoutData(formData_1);
		}
		btnEdit_1.setText("Edit...");

		final Button btnRemove_1 = new Button(includeComposite, SWT.NONE);
		formData_1.bottom = new FormAttachment(100, -215);
		{
			FormData formData = new FormData();
			formData.top = new FormAttachment(btnEdit_1, 6);
			formData.right = new FormAttachment(btnAddFolder, 0, SWT.RIGHT);
			formData.width = 77;
			btnRemove_1.setLayoutData(formData);
		}
		btnRemove_1.setText("Remove");
		{
			ListViewer listViewer = new ListViewer(includeComposite, SWT.BORDER);
			final org.eclipse.swt.widgets.List includes_list = listViewer
					.getList();
			{
				FormData formData = new FormData();
				formData.bottom = new FormAttachment(100, -10);
				formData.right = new FormAttachment(btnAddFolder, -6);
				formData.top = new FormAttachment(lblFoldersWithInclude, 6);
				formData.left = new FormAttachment(0, 10);
				includes_list.setLayoutData(formData);
			}
		}

		final TabItem projectsTab = new TabItem(this.tabFolder, SWT.NONE);
		projectsTab.setText("Projects");

		final Composite dependenciesComposite = new Composite(this.tabFolder,
				SWT.NONE);
		dependenciesComposite.setLayout(new FormLayout());
		projectsTab.setControl(dependenciesComposite);

		final Label lblProjectsOnThe = new Label(dependenciesComposite,
				SWT.NONE);
		{
			FormData formData = new FormData();
			formData.right = new FormAttachment(100, -10);
			formData.top = new FormAttachment(0, 10);
			formData.left = new FormAttachment(0, 10);
			lblProjectsOnThe.setLayoutData(formData);
		}
		lblProjectsOnThe.setText("Required projects on the build path:");

		final Button button = new Button(dependenciesComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 319);
			formData.top = new FormAttachment(lblProjectsOnThe, 6);
			button.setLayoutData(formData);
		}
		button.setText("New Button");

		final Button button_1 = new Button(dependenciesComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 319);
			formData.top = new FormAttachment(button, 23);
			button_1.setLayoutData(formData);
		}
		button_1.setText("New Button");

		final Button button_2 = new Button(dependenciesComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 319);
			formData.top = new FormAttachment(button_1, 6);
			button_2.setLayoutData(formData);
		}
		button_2.setText("New Button");
		{
			ListViewer listViewer = new ListViewer(dependenciesComposite,
					SWT.BORDER);
			final org.eclipse.swt.widgets.List projects_list = listViewer
					.getList();
			{
				FormData formData = new FormData();
				formData.bottom = new FormAttachment(100, -6);
				formData.right = new FormAttachment(button, -6);
				formData.top = new FormAttachment(lblProjectsOnThe, 6);
				formData.left = new FormAttachment(0, 10);
				projects_list.setLayoutData(formData);
			}
		}

		final Collection<RuntimeInfo> rs = ErlangCore.getRuntimeInfoManager()
				.getRuntimes();
		final List<String[]> vv = new ArrayList<String[]>();
		for (final RuntimeInfo ri : rs) {
			vv.add(new String[] { ri.getName(), ri.getName() });
		}
		final String[][] values = vv.toArray(new String[][] {});

		final TabItem librariesTab = new TabItem(tabFolder, SWT.NONE);
		librariesTab.setText("Libraries");

		final Composite librariesComposite = new Composite(tabFolder, SWT.NONE);
		librariesComposite.setLayout(new FormLayout());
		librariesTab.setControl(librariesComposite);

		final Label lblExternalLibrariesRequired = new Label(
				librariesComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.right = new FormAttachment(100, -10);
			formData.top = new FormAttachment(0, 10);
			formData.left = new FormAttachment(0, 10);
			lblExternalLibrariesRequired.setLayoutData(formData);
		}
		lblExternalLibrariesRequired
				.setText("External libraries required on build path:");

		final Button button_3 = new Button(librariesComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 322);
			formData.top = new FormAttachment(lblExternalLibrariesRequired, 6);
			button_3.setLayoutData(formData);
		}
		button_3.setText("New Button");

		final Button button_4 = new Button(librariesComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 322);
			formData.top = new FormAttachment(button_3, 6);
			button_4.setLayoutData(formData);
		}
		button_4.setText("New Button");

		final Button button_5 = new Button(librariesComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 322);
			formData.top = new FormAttachment(button_4, 23);
			button_5.setLayoutData(formData);
		}
		button_5.setText("New Button");

		final Button button_6 = new Button(librariesComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 322);
			formData.top = new FormAttachment(button_5, 6);
			button_6.setLayoutData(formData);
		}
		button_6.setText("New Button");
		{
			TreeViewer treeViewer = new TreeViewer(librariesComposite,
					SWT.BORDER);
			final Tree libraries_tree = treeViewer.getTree();
			{
				FormData formData = new FormData();
				formData.right = new FormAttachment(button_3, -6);
				formData.top = new FormAttachment(lblExternalLibrariesRequired,
						4);
				formData.left = new FormAttachment(
						lblExternalLibrariesRequired, 0, SWT.LEFT);
				formData.bottom = new FormAttachment(100, -10);
				libraries_tree.setLayoutData(formData);
			}
		}

		final TabItem orderTab = new TabItem(tabFolder, SWT.NONE);
		orderTab.setText("Codepath order");

		final Composite codepathComposite = new Composite(tabFolder, SWT.NONE);
		codepathComposite.setLayout(new FormLayout());
		orderTab.setControl(codepathComposite);

		final Label lblOrderOfThe = new Label(codepathComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.right = new FormAttachment(100, -10);
			formData.top = new FormAttachment(0, 10);
			formData.left = new FormAttachment(0, 10);
			lblOrderOfThe.setLayoutData(formData);
		}
		lblOrderOfThe
				.setText("Order of the dependencies on runtime code path:");

		final Button button_7 = new Button(codepathComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 319);
			formData.top = new FormAttachment(lblOrderOfThe, 6);
			button_7.setLayoutData(formData);
		}
		button_7.setText("New Button");

		final Button button_8 = new Button(codepathComposite, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 319);
			formData.top = new FormAttachment(button_7, 6);
			button_8.setLayoutData(formData);
		}
		button_8.setText("New Button");
		{
			ListViewer listViewer = new ListViewer(codepathComposite,
					SWT.BORDER);
			final org.eclipse.swt.widgets.List codepath_list = listViewer
					.getList();
			{
				FormData formData = new FormData();
				formData.right = new FormAttachment(button_7, -6);
				formData.bottom = new FormAttachment(100, -10);
				formData.top = new FormAttachment(lblOrderOfThe, 6);
				formData.left = new FormAttachment(0, 10);
				codepath_list.setLayoutData(formData);
			}
		}

		final TabItem tbtmPreprocessor = new TabItem(tabFolder, SWT.NONE);
		tbtmPreprocessor.setText("Preprocessor");

		final Composite composite_1 = new Composite(tabFolder, SWT.NONE);
		composite_1.setLayout(new FormLayout());
		tbtmPreprocessor.setControl(composite_1);

		final Label lblMacrosDefinedFor = new Label(composite_1, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.top = new FormAttachment(0, 10);
			formData.left = new FormAttachment(0, 10);
			lblMacrosDefinedFor.setLayoutData(formData);
		}
		lblMacrosDefinedFor.setText("Macros defined for this project");

		final Button button_9 = new Button(composite_1, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.top = new FormAttachment(0, 31);
			formData.left = new FormAttachment(0, 311);
			button_9.setLayoutData(formData);
		}
		button_9.setText("New Button");

		final Button button_10 = new Button(composite_1, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.left = new FormAttachment(0, 311);
			formData.top = new FormAttachment(button_9, 20);
			button_10.setLayoutData(formData);
		}
		button_10.setText("New Button");

		final Button button_11 = new Button(composite_1, SWT.NONE);
		{
			FormData formData = new FormData();
			formData.top = new FormAttachment(button_10, 6);
			formData.right = new FormAttachment(button_9, 0, SWT.RIGHT);
			button_11.setLayoutData(formData);
		}
		button_11.setText("New Button");
		{
			ListViewer listViewer = new ListViewer(composite_1, SWT.BORDER);
			final org.eclipse.swt.widgets.List macros_list = listViewer
					.getList();
			{
				FormData formData = new FormData();
				formData.bottom = new FormAttachment(100, -10);
				formData.right = new FormAttachment(button_9, -6);
				formData.top = new FormAttachment(lblMacrosDefinedFor, 6);
				formData.left = new FormAttachment(lblMacrosDefinedFor, 0,
						SWT.LEFT);
				macros_list.setLayoutData(formData);
			}
		}

		return composite;
	}

	private IProject getProject() {
		return (IProject) getElement().getAdapter(IProject.class);
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		return super.performOk();
	}
}
