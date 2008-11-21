/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Lukas Larsson
 *******************************************************************************/
package org.erlide.ui.erlangsource.prefs;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.ResourceBundle;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.persistence.TemplatePersistenceData;
import org.eclipse.jface.text.templates.persistence.TemplateReaderWriter;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.runtime.ErlLogger;
import org.erlide.ui.erlangsource.templates.ErlangSourceContextTypeComment;

/**
 * The ErlangSource Generation preference page.
 * 
 * @author Lukas Larsson [garazdawi at gmail dot com]
 */
public class ErlangSourcePreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage, SelectionListener, ISelectionChangedListener {

	@SuppressWarnings("unchecked")
	private final java.util.List fTreeModel = new ArrayList();

	private TreeViewer templateTree;

	private Button edit_button;

	private Button export_button;

	private Button export_all_button;

	private Button import_button;

	private Button new_button;

	private Button remove_button;

	ResourceBundle resourceBundle = ResourceBundle
			.getBundle("org.erlide.ui.erlangsource.prefs.ErlangSource");

	private SourceViewer preview;

	private TemplatePersistenceData selectedTemplate;

	/**
	 * Initialize the system preferences
	 * 
	 */
	public ErlangSourcePreferencePage() {
		super("Erlang Template");
		try {
			ErlangSourceContextTypeComment.getDefault().getTemplateStore()
					.load();
		} catch (final IOException e) {
			ErlLogger.warn(e);
		}
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.IPreferencePage#performOk()
	 */
	@Override
	public boolean performOk() {
		try {
			ErlangSourceContextTypeComment.getDefault().getTemplateStore()
					.save();
		} catch (final IOException e) {
			ErlLogger.warn(e);
		}
		return super.performOk();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse
	 * .swt.widgets.Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		final Composite top = new Composite(parent, SWT.LEFT);
		top.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		top.setLayout(new GridLayout(2, false));

		Label label = new Label(top, SWT.LEFT);
		label.setText(resourceBundle.getString("template_label"));
		label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2,
				1));

		templateTree = new TreeViewer(top, SWT.SINGLE | SWT.BORDER);
		templateTree.setLabelProvider(new TemplateLabelProvider());
		templateTree.setContentProvider(new TemplateContentProvider());
		templateTree.setInput(fTreeModel);
		templateTree.getControl().setLayoutData(
				new GridData(SWT.FILL, SWT.FILL, true, true, 1, 9));
		templateTree.addPostSelectionChangedListener(this);

		new_button = new Button(top, SWT.PUSH);
		new_button.setText(resourceBundle.getString("new_button"));
		new_button.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, false,
				false));
		new_button.addSelectionListener(this);

		edit_button = new Button(top, SWT.PUSH);
		edit_button.setText(resourceBundle.getString("edit_button"));
		edit_button.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, false,
				false));
		edit_button.setEnabled(false);
		edit_button.addSelectionListener(this);

		remove_button = new Button(top, SWT.PUSH);
		remove_button.setText(resourceBundle.getString("remove_button"));
		remove_button.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING,
				false, false));
		remove_button.setEnabled(false);
		remove_button.addSelectionListener(this);

		label = new Label(top, SWT.LEFT);

		import_button = new Button(top, SWT.PUSH);
		import_button.setText(resourceBundle.getString("import_button"));
		import_button.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING,
				false, false));
		import_button.addSelectionListener(this);

		export_button = new Button(top, SWT.PUSH);
		export_button.setText(resourceBundle.getString("export_button"));
		export_button.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING,
				false, false));
		export_button.addSelectionListener(this);
		export_button.setEnabled(false);

		export_all_button = new Button(top, SWT.PUSH);
		export_all_button
				.setText(resourceBundle.getString("export_all_button"));
		export_all_button.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING,
				false, false));
		export_all_button.addSelectionListener(this);

		label = new Label(top, SWT.LEFT);

		final Composite previewParent = new Composite(top, SWT.LEFT);
		previewParent.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
				false, 2, 1));
		final RowLayout layout = new RowLayout(SWT.VERTICAL);
		layout.justify = true;
		previewParent.setLayout(layout);

		final Label treeGroup = new Label(previewParent, SWT.LEFT);
		treeGroup.setText(resourceBundle.getString("preview_label"));

		preview = new SourceViewer(previewParent, null, null, false, SWT.MULTI
				| SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);

		final Document doc = new Document();
		preview.setDocument(doc);
		preview.getControl().setBackground(new Color(null, 255, 255, 255));
		preview.getControl().setLayoutData(new RowData(320, 100));
		preview.setEditable(false);

		return top;
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
		if (e.getSource() == new_button) {
			/* final ErlangSourceTemplateDialog diag = */
			new ErlangSourceTemplateDialog(getShell(), resourceBundle);
		} else if (e.getSource() == edit_button) {
			/* final ErlangSourceTemplateDialog diag = */
			new ErlangSourceTemplateDialog(getShell(), resourceBundle,
					selectedTemplate);
		} else if (e.getSource() == remove_button) {
			final IStructuredSelection selection = (IStructuredSelection) templateTree
					.getSelection();

			final Iterator<?> elements = selection.iterator();
			while (elements.hasNext()) {
				final TemplatePersistenceData data = (TemplatePersistenceData) elements
						.next();
				ErlangSourceContextTypeComment.getDefault().getTemplateStore()
						.delete(data);
			}

			templateTree.refresh();
		} else if (e.getSource() == import_button) {
			import_();
		} else if (e.getSource() == export_button) {
			final IStructuredSelection selection = (IStructuredSelection) templateTree
					.getSelection();

			final Object[] o = selection.toArray();
			final ArrayList<Object> list = new ArrayList<Object>();
			for (int i = 0; i < o.length; i++) {
				if (o[i] instanceof String) {
					final TemplatePersistenceData[] data = ErlangSourceContextTypeComment
							.getDefault().getTemplateStore().getTemplateData(
									true);
					for (TemplatePersistenceData element : data) {
						if (element.getTemplate().getContextTypeId().equals(
								o[i])) {
							list.add(element);
						}
					}
				}
				list.add(o[i]);
			}

			final TemplatePersistenceData[] templates = new TemplatePersistenceData[o.length];
			for (int i = 0; i < templates.length; i++) {
				if (o[i] instanceof String) {

				}
				templates[i] = (TemplatePersistenceData) o[i];
			}

			export(templates);
		} else if (e.getSource() == export_all_button) {
			export(ErlangSourceContextTypeComment.getDefault()
					.getTemplateStore().getTemplateData(true));
		}
		templateTree.refresh();
		final IStructuredSelection selection = (IStructuredSelection) templateTree
				.getSelection();
		if (selection != null) {
			preview.getDocument().set(
					((TemplatePersistenceData) selection.getFirstElement())
							.getTemplate().getPattern());
		}
	}

	private void import_() {
		final FileDialog dialog = new FileDialog(getShell());
		dialog.setText("Importing Templates");
		dialog.setFilterExtensions(new String[] { "*.xml" });
		final String path = dialog.open();

		if (path == null) {
			return;
		}

		try {
			final TemplateReaderWriter reader = new TemplateReaderWriter();
			final File file = new File(path);
			if (file.exists()) {
				final InputStream input = new BufferedInputStream(
						new FileInputStream(file));
				try {
					final TemplatePersistenceData[] datas = reader.read(input,
							null);
					for (final TemplatePersistenceData data : datas) {
						ErlangSourceContextTypeComment.getDefault()
								.getTemplateStore().add(data);
					}
				} finally {
					try {
						input.close();
					} catch (final IOException x) {
					}
				}
			}
		} catch (final FileNotFoundException e) {
			openWriteErrorDialog(e);
		} catch (final IOException e) {
			openWriteErrorDialog(e);
		}
	}

	private void export(TemplatePersistenceData[] templates) {
		final FileDialog dialog = new FileDialog(getShell(), SWT.SAVE);
		dialog.setText("Exporting " + templates.length + " templates.");
		dialog.setFilterExtensions(new String[] { "*.xml" });
		dialog.setFileName("templates.xml");
		final String path = dialog.open();

		final File file = new File(path);

		if (file.isHidden()) {
			final String title = "Error: Hidden file!";
			final String message = "A hidden file has been found at "
					+ file.getAbsolutePath();
			MessageDialog.openError(getShell(), title, message);
			return;
		}

		if (file.exists() && !file.canWrite()) {
			final String title = "Error: Cannot write!";
			final String message = "The file could not be written to that location.";
			MessageDialog.openError(getShell(), title, message);
			return;
		}

		if (!file.exists() || confirmOverwrite(file)) {
			OutputStream output = null;
			try {
				output = new BufferedOutputStream(new FileOutputStream(file));
				final TemplateReaderWriter writer = new TemplateReaderWriter();
				writer.save(templates, output);
				output.close();
			} catch (final IOException e) {
				if (output != null) {
					try {
						output.close();
					} catch (final IOException e2) {
						// ignore
					}
				}
				openWriteErrorDialog(e);
			}
		}
	}

	private boolean confirmOverwrite(File file) {
		return MessageDialog.openQuestion(getShell(), "Overwrite file?",
				"Are you sure that you want to overwrite "
						+ file.getAbsolutePath());
	}

	private void openWriteErrorDialog(Exception e) {
		final String title = "Error!";
		final String message = "An unexpected exception has been raised"
				+ e.getLocalizedMessage();
		MessageDialog.openError(getShell(), title, message);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(
	 * org.eclipse.jface.viewers.SelectionChangedEvent)
	 */
	public void selectionChanged(SelectionChangedEvent event) {
		final StructuredSelection selection = (StructuredSelection) event
				.getSelection();
		if (selection.getFirstElement() instanceof TemplatePersistenceData) {
			preview.getDocument().set(
					((TemplatePersistenceData) selection.getFirstElement())
							.getTemplate().getPattern());
			edit_button.setEnabled(true);
			remove_button.setEnabled(true);
			selectedTemplate = (TemplatePersistenceData) selection
					.getFirstElement();
		} else {
			export_button.setEnabled(true);
			edit_button.setEnabled(false);
			remove_button.setEnabled(false);
			selectedTemplate = null;
		}
	}

	/**
	 * Color list label provider.
	 * 
	 */
	class TemplateLabelProvider extends LabelProvider {

		/*
		 * @see
		 * org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
		 */
		@Override
		public String getText(Object element) {
			if (element instanceof String) {
				return resourceBundle.getString((String) element);
			}
			return ((TemplatePersistenceData) element).getTemplate().getName();
		}
	}

	/**
	 * Color list content provider.
	 * 
	 */
	static class TemplateContentProvider implements ITreeContentProvider {

		/*
		 * @see
		 * org.eclipse.jface.viewers.IStructuredContentProvider#getElements(
		 * java.lang.Object)
		 */
		public Object[] getElements(Object inputElement) {
			final Iterator<?> it = ErlangSourceContextTypeComment.getDefault()
					.getContextTypeRegistry().contextTypes();
			final ArrayList<String> al = new ArrayList<String>();
			while (it.hasNext()) {
				final TemplateContextType context = (TemplateContextType) it
						.next();
				al.add(context.getId());
			}
			return al.toArray();
		}

		/*
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		public void dispose() {
		}

		/*
		 * @see
		 * org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse
		 * .jface.viewers.Viewer, java.lang.Object, java.lang.Object)
		 */
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}

		public Object[] getChildren(Object parentElement) {
			if (parentElement instanceof String) {

				final TemplatePersistenceData[] templates = ErlangSourceContextTypeComment
						.getDefault().getTemplateStore().getTemplateData(false);

				final ArrayList<TemplatePersistenceData> list = new ArrayList<TemplatePersistenceData>();
				for (TemplatePersistenceData element : templates) {
					if (element.getTemplate().getContextTypeId().equals(
							parentElement)) {
						list.add(element);
					}
				}

				return list.toArray();
			}
			return new Object[0];
		}

		/* FIXME: Is convert needed ? */
		@SuppressWarnings("unused")
		private TemplatePersistenceData[] convert(Template[] templates) {
			final TemplatePersistenceData[] temp = new TemplatePersistenceData[templates.length];
			for (int i = 0; i < templates.length; i++) {
				temp[i] = new TemplatePersistenceData(templates[i], true);
			}
			return temp;
		}

		public Object getParent(Object element) {
			if (element instanceof String) {
				return null;
			}

			final TemplatePersistenceData child = (TemplatePersistenceData) element;
			return child.getTemplate().getContextTypeId();
		}

		public boolean hasChildren(Object element) {
			return element instanceof String;
		}
	}
}