/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Lukas Larsson
 *******************************************************************************/

package org.erlide.ui.erlangsource.wizards;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IDialogPage;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateBuffer;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.ui.erlangsource.templates.ErlangSourceContextTypeBehaviour;
import org.erlide.ui.erlangsource.templates.ErlangSourceContextTypeComment;
import org.erlide.ui.erlangsource.templates.ErlangSourceContextTypeLayout;
import org.erlide.ui.erlangsource.templates.ExportedFunctionsVariableResolver;
import org.erlide.ui.erlangsource.templates.LocalFunctionsVariableResolver;
import org.erlide.ui.erlangsource.templates.ModuleVariableResolver;

/**
 * The "New" wizard page allows setting the container for the new file as well
 * as the file name. The page will only accept file name without the extension
 * OR with the extension that matches the expected one (erl).
 */

public class ErlangFileWizardPage extends WizardPage implements
		SelectionListener {

	public boolean gettingInput = false;

	private Text containerText;

	private Text fileText;

	private Combo applications;

	private Combo skeleton;

	FunctionGroup functionGroup;

	private ISelection fSelection;

	Template[] behaviours;

	/**
	 * Constructor for SampleNewWizardPage.
	 * 
	 * @param pageName
	 */
	public ErlangFileWizardPage(ISelection selection) {
		super("wizardPage");
		setTitle("Erlang Source File");
		setDescription("This wizard creates a new erlang source file.");
		fSelection = selection;

		behaviours = ErlangSourceContextTypeComment.getDefault()
				.getTemplateStore().getTemplates(
						ErlangSourceContextTypeBehaviour.getDefault().getId());

	}

	/**
	 * @see IDialogPage#createControl(Composite)
	 */
	public void createControl(Composite parent) {

		final Composite container = new Composite(parent, SWT.NULL);

		final GridLayout grid = new GridLayout(1, true);
		container.setLayout(grid);

		final Composite filePanel = new Composite(container, SWT.NULL);
		GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		filePanel.setLayoutData(gd);
		final GridLayout layout = new GridLayout();
		filePanel.setLayout(layout);
		layout.numColumns = 3;
		layout.verticalSpacing = 9;
		Label label = new Label(filePanel, SWT.NULL);
		label.setText("&Container:");

		containerText = new Text(filePanel, SWT.BORDER | SWT.SINGLE);
		gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
		containerText.setLayoutData(gd);
		containerText.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				dialogChanged();
			}
		});

		final Button button = new Button(filePanel, SWT.PUSH);
		button.setText("Browse...");
		button.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				handleBrowse();
			}
		});

		label = new Label(filePanel, SWT.NULL);
		label.setText("&Module name:");

		fileText = new Text(filePanel, SWT.BORDER | SWT.SINGLE);
		gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
		fileText.setLayoutData(gd);
		fileText.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				dialogChanged();
			}
		});

		new Label(filePanel, SWT.NULL);

		label = new Label(filePanel, SWT.NULL);
		label.setText("&Application name:");

		applications = new Combo(filePanel, SWT.BORDER | SWT.DROP_DOWN
				| SWT.READ_ONLY);
		applications.add("None");
		gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
		applications.setLayoutData(gd);
		applications.select(0);
		applications.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				dialogChanged();
			}
		});

		new Label(filePanel, SWT.NULL);

		label = new Label(filePanel, SWT.NULL);
		gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
		label.setLayoutData(gd);
		label.setText("&Skeleton");

		skeleton = new Combo(filePanel, SWT.BORDER | SWT.DROP_DOWN
				| SWT.READ_ONLY);
		skeleton.add("None");

		for (Template element : behaviours) {
			skeleton.add(element.getName());
		}
		skeleton.select(0);

		functionGroup = new FunctionGroup(container, this);

		initialize();
		dialogChanged();
		setControl(container);
	}

	/**
	 * Tests if the current workbench selection is a suitable container to use.
	 */

	private void initialize() {
		if (fSelection != null && !fSelection.isEmpty()
				&& fSelection instanceof IStructuredSelection) {
			final IStructuredSelection ssel = (IStructuredSelection) fSelection;
			if (ssel.size() > 1) {
				return;
			}
			final Object obj = ssel.getFirstElement();
			if (obj instanceof IResource) {
				IContainer container;
				if (obj instanceof IContainer) {
					container = (IContainer) obj;
				} else {
					container = ((IResource) obj).getParent();
				}
				ErlangProjectProperties pp = new ErlangProjectProperties(
						((IResource) obj).getProject());
				if (pp.hasSourceDir(container.getFullPath())) {
					containerText.setText(container.getFullPath().toString());
				} else if (pp.getSourceDirs().length > 0) {
					containerText.setText(pp.getSourceDirs()[0].toString());
				} else {
					containerText.setText(container.getFullPath().toString());
				}

			}
		}

		fileText.setText("new_file");
	}

	/**
	 * Uses the standard container selection dialog to choose the new value for
	 * the container field.
	 */

	void handleBrowse() {
		final ContainerSelectionDialog dialog = new ContainerSelectionDialog(
				getShell(), ResourcesPlugin.getWorkspace().getRoot(), false,
				"Select new file container");
		if (dialog.open() == Window.OK) {
			final Object[] result = dialog.getResult();
			if (result.length == 1) {
				containerText.setText(((Path) result[0]).toString());
			}
		}
	}

	/**
	 * Ensures that both text fields are set.
	 */

	void dialogChanged() {
		final IResource container = ResourcesPlugin.getWorkspace().getRoot()
				.findMember(new Path(getContainerName()));
		final String fileName = getFileName();

		if (getContainerName().length() == 0) {
			updateStatus("File container must be specified");
			return;
		}
		if (container == null
				|| (container.getType() & (IResource.PROJECT | IResource.FOLDER)) == 0) {
			updateStatus("File container must exist");
			return;
		}
		if (!container.isAccessible()) {
			updateStatus("Project must be writable");
			return;
		}
		if (fileName.length() == 0) {
			updateStatus("File name must be specified");
			return;
		}
		if (fileName.replace('\\', '/').indexOf('/', 1) > 0) {
			updateStatus("File name must be valid");
			return;
		}
		final int dotLoc = fileName.lastIndexOf('.');
		if (dotLoc != -1) {
			final String ext = fileName.substring(dotLoc + 1);
			if (!ext.equalsIgnoreCase("erl")) {
				updateStatus("File extension must be \"erl\"");
				return;
			}
		}
		updateStatus(null);
	}

	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

	public String getContainerName() {
		return containerText.getText();
	}

	public String getFileName() {
		return fileText.getText();
	}

	/**
	 * Get the skeleton that is to be generated.
	 * 
	 * @return The skeleton that the new file is to consist of.
	 */
	public String getSkeleton() {
		if (skeleton.getSelectionIndex() > 0) {
			return parse(behaviours[skeleton.getSelectionIndex() - 1],
					ErlangSourceContextTypeBehaviour.getDefault());
		}
		return parse(ErlangSourceContextTypeComment.getDefault()
				.getTemplateStore().getTemplateData(
						"org.erlide.ui.erlangsource.modulelayout")
				.getTemplate(), ErlangSourceContextTypeLayout.getDefault());
	}

	private String parse(Template template, TemplateContextType contextType) {
		ModuleVariableResolver.getDefault().setModule(getFileName());

		ExportedFunctionsVariableResolver.getDefault().clearFunctions();
		LocalFunctionsVariableResolver.getDefault().clearFunctions();

		/*
		 * final LocalFunctionsVariableResolver h =
		 * LocalFunctionsVariableResolver .getDefault();
		 */

		for (int i = 0; i < functionGroup.getFunctionData().length; i++) {
			final Function fun = functionGroup.getFunctionData()[i];
			if (fun.isExported) {
				ExportedFunctionsVariableResolver.getDefault().addFunction(
						fun.name, fun.arity);
			} else {
				LocalFunctionsVariableResolver.getDefault().addFunction(
						fun.name, fun.arity);
			}
		}

		TemplateBuffer tb = null;

		try {
			final DocumentTemplateContext context = new DocumentTemplateContext(
					contextType, new Document(template.getPattern()), 0,
					template.getPattern().length());
			tb = context.evaluate(template);
		} catch (final BadLocationException e) {
			e.printStackTrace();
		} catch (final TemplateException e) {
			e.printStackTrace();
		}

		if (tb == null) {
			return null;
		}
		return tb.getString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse
	 * .swt.events.SelectionEvent)
	 */
	public void widgetDefaultSelected(SelectionEvent e) {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt
	 * .events.SelectionEvent)
	 */
	public void widgetSelected(SelectionEvent e) {

	}
}
