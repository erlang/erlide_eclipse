/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.gunit.internal.ui;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import org.eclipse.core.runtime.CoreException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.TrayDialog;

import org.eclipse.ui.PlatformUI;

import org.erlide.gunit.internal.model.TestElement;

public class CompareResultDialog extends TrayDialog {
	private static final String PREFIX_SUFFIX_PROPERTY = "org.erlide.gunit.internal.ui.CompareResultDialog.prefixSuffix"; //$NON-NLS-1$

	private static class CompareResultMergeViewer extends TextMergeViewer {
		private CompareResultMergeViewer(Composite parent, int style,
				CompareConfiguration configuration) {
			super(parent, style, configuration);
		}

		protected void createControls(Composite composite) {
			super.createControls(composite);
			PlatformUI.getWorkbench().getHelpSystem().setHelp(composite,
					IGUnitHelpContextIds.RESULT_COMPARE_DIALOG);
		}

		// protected void createToolItems(ToolBarManager tbm) {
		// ResourceBundle bundle= CompareUI.getResourceBundle();
		// tbm.add(new IgnoreWhiteSpaceAction(bundle,
		// getCompareConfiguration()));
		// super.createToolItems(tbm);
		// }

		protected void configureTextViewer(TextViewer textViewer) {
			if (textViewer instanceof SourceViewer) {
				int[] prefixSuffixOffsets = (int[]) getCompareConfiguration()
						.getProperty(PREFIX_SUFFIX_PROPERTY);
				((SourceViewer) textViewer)
						.configure(new CompareResultViewerConfiguration(
								prefixSuffixOffsets));
			}
		}
	}

	private static class CompareResultViewerConfiguration extends
			SourceViewerConfiguration {
		private static class SimpleDamagerRepairer implements
				IPresentationDamager, IPresentationRepairer {
			private IDocument fDocument;
			private final int[] fPrefixSuffixOffsets2;

			public SimpleDamagerRepairer(int[] prefixSuffixOffsets) {
				fPrefixSuffixOffsets2 = prefixSuffixOffsets;
			}

			public void setDocument(IDocument document) {
				fDocument = document;
			}

			public IRegion getDamageRegion(ITypedRegion partition,
					DocumentEvent event, boolean changed) {
				return new Region(0, fDocument.getLength());
			}

			public void createPresentation(TextPresentation presentation,
					ITypedRegion damage) {
				int prefix = fPrefixSuffixOffsets2[0];
				int suffix = fPrefixSuffixOffsets2[1];
				TextAttribute attr = new TextAttribute(Display.getDefault()
						.getSystemColor(SWT.COLOR_RED), null, SWT.BOLD);
				presentation.addStyleRange(new StyleRange(prefix, fDocument
						.getLength()
						- suffix - prefix, attr.getForeground(), attr
						.getBackground(), attr.getStyle()));
			}
		}

		private final int[] fPrefixSuffixOffsets;

		public CompareResultViewerConfiguration(int[] prefixSuffixOffsets) {
			fPrefixSuffixOffsets = prefixSuffixOffsets;
		}

		public IPresentationReconciler getPresentationReconciler(
				ISourceViewer sourceViewer) {
			PresentationReconciler reconciler = new PresentationReconciler();
			SimpleDamagerRepairer dr = new SimpleDamagerRepairer(
					fPrefixSuffixOffsets);
			reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
			reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
			return reconciler;
		}
	}

	private static class CompareElement implements ITypedElement,
			IEncodedStreamContentAccessor {
		private String fContent;

		public CompareElement(String content) {
			fContent = content;
		}

		public String getName() {
			return "<no name>"; //$NON-NLS-1$
		}

		public Image getImage() {
			return null;
		}

		public String getType() {
			return "txt"; //$NON-NLS-1$
		}

		public InputStream getContents() {
			try {
				return new ByteArrayInputStream(fContent.getBytes("UTF-8")); //$NON-NLS-1$
			} catch (UnsupportedEncodingException e) {
				return new ByteArrayInputStream(fContent.getBytes());
			}
		}

		public String getCharset() throws CoreException {
			return "UTF-8"; //$NON-NLS-1$
		}
	}

	private TextMergeViewer fViewer;
	private String fExpected;
	private String fActual;
	private String fTestName;

	/**
	 * Lengths of common prefix and suffix. Note: this array is passed to the
	 * DamagerRepairer and the lengths are updated on content change.
	 */
	private final int[] fPrefixSuffix = new int[2];

	public CompareResultDialog(Shell parentShell, TestElement element) {
		super(parentShell);
		setShellStyle(SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX);
		setFailedTest(element);
	}

	private void setFailedTest(TestElement failedTest) {
		fTestName = failedTest.getTestName();
		fExpected = failedTest.getExpected();
		fActual = failedTest.getActual();
		computePrefixSuffix();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#getDialogBoundsSettings()
	 */
	@Override
	protected IDialogSettings getDialogBoundsSettings() {
		return GUnitPlugin.getDefault().getDialogSettingsSection(
				getClass().getName());
	}

	private void computePrefixSuffix() {
		int end = Math.min(fExpected.length(), fActual.length());
		int i = 0;
		for (; i < end; i++)
			if (fExpected.charAt(i) != fActual.charAt(i))
				break;
		fPrefixSuffix[0] = i;

		int j = fExpected.length() - 1;
		int k = fActual.length() - 1;
		int l = 0;
		for (; k >= i && j >= i; k--, j--) {
			if (fExpected.charAt(j) != fActual.charAt(k))
				break;
			l++;
		}
		fPrefixSuffix[1] = l;
	}

	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(JUnitMessages.CompareResultDialog_title);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(newShell,
				IGUnitHelpContextIds.RESULT_COMPARE_DIALOG);
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID,
				JUnitMessages.CompareResultDialog_labelOK, true);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		composite.setLayout(layout);

		CompareViewerPane pane = new CompareViewerPane(composite, SWT.BORDER
				| SWT.FLAT);
		pane.setText(fTestName);
		GridData data = new GridData(GridData.FILL_HORIZONTAL
				| GridData.FILL_VERTICAL);
		data.widthHint = convertWidthInCharsToPixels(120);
		data.heightHint = convertHeightInCharsToPixels(13);
		pane.setLayoutData(data);

		Control previewer = createPreviewer(pane);
		pane.setContent(previewer);
		GridData gd = new GridData(GridData.FILL_BOTH);
		previewer.setLayoutData(gd);
		applyDialogFont(parent);
		return composite;
	}

	private Control createPreviewer(Composite parent) {
		final CompareConfiguration compareConfiguration = new CompareConfiguration();
		compareConfiguration
				.setLeftLabel(JUnitMessages.CompareResultDialog_expectedLabel);
		compareConfiguration.setLeftEditable(false);
		compareConfiguration
				.setRightLabel(JUnitMessages.CompareResultDialog_actualLabel);
		compareConfiguration.setRightEditable(false);
		compareConfiguration.setProperty(
				CompareConfiguration.IGNORE_WHITESPACE, Boolean.FALSE);
		compareConfiguration.setProperty(PREFIX_SUFFIX_PROPERTY, fPrefixSuffix);

		fViewer = new CompareResultMergeViewer(parent, SWT.NONE,
				compareConfiguration);
		setCompareViewerInput();

		Control control = fViewer.getControl();
		control.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				compareConfiguration.dispose();
			}
		});
		return control;
	}

	private void setCompareViewerInput() {
		if (!fViewer.getControl().isDisposed())
			fViewer.setInput(new DiffNode(new CompareElement(fExpected),
					new CompareElement(fActual)));
	}

	public void setInput(TestElement failedTest) {
		setFailedTest(failedTest);
		setCompareViewerInput();
	}
}
