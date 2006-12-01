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

import java.util.Iterator;
import java.util.ResourceBundle;

import org.eclipse.jface.dialogs.StatusDialog;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.persistence.TemplatePersistenceData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.erlide.ui.erlangsource.templates.ErlangSourceContextTypeComment;

/**
 * @author Lukas Larsson
 * 
 * An ErlangSource Generation editing dialog.
 */
public class ErlangSourceTemplateDialog extends StatusDialog implements
		SelectionListener {

	TemplatePersistenceData fTemplate;

	ResourceBundle rb;

	private SourceViewer fPreview;

	TemplateVariableProcessor fTemplateProcessor;

	private Text fDescription;

	private Text fName;

	private Combo fContext;

	private boolean fNewTemplate = false;

	private Button fVariables;

	public ErlangSourceTemplateDialog(Shell shell, ResourceBundle bundle) {
		super(shell);

		rb = bundle;

		this.fTemplate = new TemplatePersistenceData(new Template("", "",
				((TemplateContextType) ErlangSourceContextTypeComment
						.getDefault().getContextTypeRegistry().contextTypes()
						.next()).getId(), "", true), true);

		setShellStyle(getShellStyle() | SWT.RESIZE | SWT.MAX);
		setTitle(rb.getString("templateDialogDescriptionTitle"));

		fNewTemplate = true;

		fTemplateProcessor = new TemplateVariableProcessor();

		open();
	}

	public ErlangSourceTemplateDialog(Shell shell, ResourceBundle bundle,
			TemplatePersistenceData template) {
		super(shell);

		rb = bundle;

		this.fTemplate = template;

		setShellStyle(getShellStyle() | SWT.RESIZE | SWT.MAX);
		setTitle(rb.getString("templateDialogDescriptionTitle"));

		fTemplateProcessor = new TemplateVariableProcessor();

		open();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite top = new Composite(parent, SWT.NONE);
		top.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false));
		top.setLayout(new GridLayout(4, false));

		Label lab = new Label(top, SWT.LEFT);
		lab.setText(rb.getString("templateDialogNameLabel"));

		fName = new Text(top, SWT.BORDER | SWT.LEFT);
		fName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		lab = new Label(top, SWT.LEFT);
		lab.setText(rb.getString("templateDialogTypeLabel"));

		fContext = new Combo(top, SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
		for (final Iterator iter = ErlangSourceContextTypeComment.getDefault()
				.getContextTypeRegistry().contextTypes(); iter.hasNext();) {
			final TemplateContextType element = (TemplateContextType) iter.next();
			fContext.add(element.getName());
		}

		lab = new Label(top, SWT.NONE);
		lab.setText(rb.getString("templateDialogDescriptionLabel"));

		fDescription = new Text(top, SWT.LEFT | SWT.BORDER);
		GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gd.horizontalSpan = 3;
		fDescription.setLayoutData(gd);

		final Label label = new Label(top, SWT.NONE);
		label.setText(rb.getString("preview_label"));
		label.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));

		fPreview = new SourceViewer(top, null, null, false, SWT.BORDER
				| SWT.V_SCROLL | SWT.H_SCROLL);

		final SourceViewerConfiguration conf = new SourceViewerConfiguration() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getContentAssistant(org.eclipse.jface.text.source.ISourceViewer)
			 */
			@Override
			public IContentAssistant getContentAssistant(
					ISourceViewer sourceViewer) {
				ContentAssistant assistant = new ContentAssistant();
				assistant.enableAutoActivation(true);
				assistant.enableAutoInsert(true);
				assistant.setContentAssistProcessor(fTemplateProcessor,
						IDocument.DEFAULT_CONTENT_TYPE);
				assistant.enablePrefixCompletion(true);
				return assistant;
			}

		};

		fPreview.configure(conf);

		final Document doc = new Document(fTemplate.getTemplate().getPattern());
		fPreview.setDocument(doc);
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		final int nol = doc.getNumberOfLines();
		gd.heightHint = convertHeightInCharsToPixels((nol < 5) ? 5
				: ((nol > 12) ? 12 : nol));
		gd.widthHint = convertWidthInCharsToPixels(80);
		gd.horizontalSpan = 3;
		fPreview.getControl().setLayoutData(gd);

		fPreview.prependVerifyKeyListener(new VerifyKeyListener() {

			public void verifyKey(VerifyEvent event) {
				handleVerifyKeyPressed(event);
			}
		});

		lab = new Label(top, SWT.NONE);

		fVariables = new Button(top, SWT.PUSH);
		fVariables.setText(rb.getString("templateDialogVariables"));
		fVariables.addSelectionListener(this);

		load();
		return top;
	}

	private void load() {
		fName.setText(fTemplate.getTemplate().getName());
		fDescription.setText(fTemplate.getTemplate().getDescription());
		final String name = ErlangSourceContextTypeComment.getDefault()
				.getContextTypeRegistry().getContextType(
						fTemplate.getTemplate().getContextTypeId()).getName();
		fContext.select(fContext.indexOf(name));

		fTemplateProcessor.setContextType(ErlangSourceContextTypeComment
				.getDefault().getContextTypeRegistry().getContextType(
						fTemplate.getTemplate().getContextTypeId()));
	}

	void handleVerifyKeyPressed(VerifyEvent event) {
		if (!event.doit) {
			return;
		}

		if (event.stateMask != SWT.MOD1) {
			return;
		}

		switch (event.character) {
		case ' ':
			fPreview.doOperation(ISourceViewer.CONTENTASSIST_PROPOSALS);
			event.doit = false;
			break;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#okPressed()
	 */
	@Override
	protected void okPressed() {
		final Iterator it = ErlangSourceContextTypeComment.getDefault()
				.getContextTypeRegistry().contextTypes();
		String id = null;
		while (it.hasNext()) {
			final TemplateContextType element = (TemplateContextType) it.next();
			if (element.getName().equals(
					fContext.getItem(fContext.getSelectionIndex()))) {
				id = element.getId();
			}
		}
		fTemplate.setTemplate(new Template(fName.getText(), fDescription
				.getText(), id, fPreview.getDocument().get(), true));
		if (fNewTemplate) {
			ErlangSourceContextTypeComment.getDefault().getTemplateStore().add(
					fTemplate);
		}
		super.okPressed();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
	 */
	public void widgetDefaultSelected(SelectionEvent e) {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
	 */
	public void widgetSelected(SelectionEvent e) {
		fPreview.getTextWidget().setFocus();
		fPreview.doOperation(ISourceViewer.CONTENTASSIST_PROPOSALS);
	}
}
