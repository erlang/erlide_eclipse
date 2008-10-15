/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs.plugin;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Scrollable;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.SimpleEditorConfiguration;
import org.erlide.ui.prefs.HighlightData;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.prefs.plugin.internal.ErlangSourceViewerUpdater;
import org.erlide.ui.util.IColorManager;
import org.erlide.ui.util.PixelConverter;

/**
 * The color preferences.
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ColoringPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	private static final String COMPILER_TASK_TAGS = ErlangCore.COMPILER_TASK_TAGS;

	public static final String COLORS_QUALIFIER = ErlideUIPlugin.PLUGIN_ID
			+ "/editor/colors/";

	final String fErlangCategory = PreferencesMessages.ErlEditorPreferencePage_coloring_category_erlang;

	// final String fEdocCategory =
	// PreferencesMessages.ErlEditorPreferencePage_coloring_category_edoc;

	ColorSelector fSyntaxForegroundColorEditor;
	Label fColorEditorLabel;
	Button fEnableCheckbox;
	Button fBoldCheckBox;
	Button fItalicCheckBox;
	Button fStrikethroughCheckBox;
	Button fUnderlineCheckBox;

	private StructuredViewer fListViewer;
	private IColorManager fColorManager;
	private SourceViewer fPreviewViewer;

	Map<TokenHighlight, HighlightData> fColors;

	/**
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
		fColorManager = new ColorManager();

		fColors = new HashMap<TokenHighlight, HighlightData>();
		for (TokenHighlight th : TokenHighlight.values()) {
			fColors.put(th, th.getDefaultData());
		}
	}

	static class ColorListLabelProvider extends LabelProvider {

		/*
		 * @see
		 * org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
		 */
		@Override
		public String getText(Object element) {
			if (element instanceof String) {
				return (String) element;
			}
			return ((TokenHighlight) element).getName();
		}
	}

	class ColorListContentProvider implements ITreeContentProvider {

		/*
		 * @see
		 * org.eclipse.jface.viewers.IStructuredContentProvider#getElements(
		 * java.lang.Object)
		 */
		public Object[] getElements(Object inputElement) {
			return new String[] { fErlangCategory
			// , fEdocCategory
			};
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
				final String entry = (String) parentElement;
				if (fErlangCategory.equals(entry)) {
					return fColors.keySet().toArray();
				}
				// if (fEdocCategory.equals(entry)) {
				// return fListModel.subList(0, 4).toArray();
				// }
			}
			return new Object[0];
		}

		public Object getParent(Object element) {
			if (element instanceof String) {
				return null;
			}
			// final int index = fListModel.indexOf(element);
			// if (index < 4) {
			// return fEdocCategory;
			// }
			return fErlangCategory;
		}

		public boolean hasChildren(Object element) {
			return element instanceof String;
		}
	}

	@Override
	public void performDefaults() {
		super.performDefaults();

		handleSyntaxColorListSelection();
		fPreviewViewer.invalidateTextPresentation();
	}

	@Override
	public boolean performOk() {
		// TODO
		return super.performOk();
	}

	@Override
	public void dispose() {
		fColorManager.dispose();

		super.dispose();
	}

	void handleSyntaxColorListSelection() {
		final TokenHighlight item = getHighlight();
		if (item == null) {
			fEnableCheckbox.setEnabled(false);
			fSyntaxForegroundColorEditor.getButton().setEnabled(false);
			fColorEditorLabel.setEnabled(false);
			fBoldCheckBox.setEnabled(false);
			fItalicCheckBox.setEnabled(false);
			fStrikethroughCheckBox.setEnabled(false);
			fUnderlineCheckBox.setEnabled(false);
			return;
		}
		// final RGB rgb = PreferenceConverter.getColor(getPreferenceStore(),
		// item
		// .getColorKey());
		// fSyntaxForegroundColorEditor.setColorValue(rgb);
		// fBoldCheckBox.setSelection(getPreferenceStore().getBoolean(
		// item.getBoldKey()));
		// fItalicCheckBox.setSelection(getPreferenceStore().getBoolean(
		// item.getItalicKey()));
		// fStrikethroughCheckBox.setSelection(getPreferenceStore().getBoolean(
		// item.getStrikethroughKey()));
		// fUnderlineCheckBox.setSelection(getPreferenceStore().getBoolean(
		// item.getUnderlineKey()));
		// if (item instanceof SemanticHighlightingColorListItem) {
		// fEnableCheckbox.setEnabled(true);
		// final boolean enable = getPreferenceStore().getBoolean(
		// ((SemanticHighlightingColorListItem) item).getEnableKey());
		// fEnableCheckbox.setSelection(enable);
		// fSyntaxForegroundColorEditor.getButton().setEnabled(enable);
		// fColorEditorLabel.setEnabled(enable);
		// fBoldCheckBox.setEnabled(enable);
		// fItalicCheckBox.setEnabled(enable);
		// fStrikethroughCheckBox.setEnabled(enable);
		// fUnderlineCheckBox.setEnabled(enable);
		// } else {
		// fSyntaxForegroundColorEditor.getButton().setEnabled(true);
		// fColorEditorLabel.setEnabled(true);
		// fBoldCheckBox.setEnabled(true);
		// fItalicCheckBox.setEnabled(true);
		// fStrikethroughCheckBox.setEnabled(true);
		// fUnderlineCheckBox.setEnabled(true);
		// fEnableCheckbox.setEnabled(false);
		// fEnableCheckbox.setSelection(true);
		// }
	}

	private Control createSyntaxPage(final Composite parent) {
		final Composite colorComposite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		colorComposite.setLayout(layout);

		final Link link = new Link(colorComposite, SWT.NONE);
		link
				.setText(PreferencesMessages.ErlEditorColoringConfigurationBlock_link);
		link.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				PreferencesUtil.createPreferenceDialogOn(parent.getShell(),
						e.text, null, null);
			}
		});

		final GridData gridData = new GridData(SWT.FILL, SWT.BEGINNING, true,
				false);
		gridData.widthHint = 150; // only expand further if anyone else
		// requires it
		gridData.horizontalSpan = 2;
		link.setLayoutData(gridData);

		addFiller(colorComposite, 1);

		Label label;
		label = new Label(colorComposite, SWT.LEFT);
		label
				.setText(PreferencesMessages.ErlEditorPreferencePage_coloring_element);
		label.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final Composite editorComposite = new Composite(colorComposite,
				SWT.NONE);
		layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		editorComposite.setLayout(layout);
		GridData gd = new GridData(SWT.FILL, SWT.BEGINNING, true, false);
		editorComposite.setLayoutData(gd);

		fListViewer = new TreeViewer(editorComposite, SWT.SINGLE | SWT.BORDER);
		fListViewer.setLabelProvider(new ColorListLabelProvider());
		fListViewer.setContentProvider(new ColorListContentProvider());
		fListViewer.setInput(fColors);
		fListViewer.setSelection(new StructuredSelection(fErlangCategory));
		fListViewer.setSorter(new ViewerSorter() {

			@Override
			public int category(Object element) {
				// don't sort the top level categories
				if (fErlangCategory.equals(element)) {
					return 0;
				}
				// if (fEdocCategory.equals(element)) {
				// return 1;
				// }
				// to sort semantic settings after partition based ones:
				// if (element instanceof SemanticHighlightingColorListItem)
				// return 1;
				return 0;
			}
		});
		gd = new GridData(SWT.BEGINNING, SWT.BEGINNING, false, true);
		gd.heightHint = convertHeightInCharsToPixels(9);
		int maxWidth = 0;
		for (TokenHighlight item : fColors.keySet()) {
			maxWidth = Math.max(maxWidth, convertWidthInCharsToPixels(item
					.getName().length()));
		}
		final ScrollBar vBar = ((Scrollable) fListViewer.getControl())
				.getVerticalBar();
		if (vBar != null) {
			maxWidth += vBar.getSize().x * 3; // scrollbars and tree
		}
		// indentation guess
		gd.widthHint = maxWidth;

		fListViewer.getControl().setLayoutData(gd);

		final Composite stylesComposite = new Composite(editorComposite,
				SWT.NONE);
		layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.numColumns = 2;
		stylesComposite.setLayout(layout);
		stylesComposite.setLayoutData(new GridData(GridData.FILL_BOTH));

		fEnableCheckbox = new Button(stylesComposite, SWT.CHECK);
		fEnableCheckbox
				.setText(PreferencesMessages.ErlEditorPreferencePage_enable);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalAlignment = GridData.BEGINNING;
		gd.horizontalSpan = 2;
		fEnableCheckbox.setLayoutData(gd);

		fColorEditorLabel = new Label(stylesComposite, SWT.LEFT);
		fColorEditorLabel
				.setText(PreferencesMessages.ErlEditorPreferencePage_color);
		gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
		gd.horizontalIndent = 20;
		fColorEditorLabel.setLayoutData(gd);

		fSyntaxForegroundColorEditor = new ColorSelector(stylesComposite);
		final Button foregroundColorButton = fSyntaxForegroundColorEditor
				.getButton();
		gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
		foregroundColorButton.setLayoutData(gd);

		fBoldCheckBox = new Button(stylesComposite, SWT.CHECK);
		fBoldCheckBox.setText(PreferencesMessages.ErlEditorPreferencePage_bold);
		gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
		gd.horizontalIndent = 20;
		gd.horizontalSpan = 2;
		fBoldCheckBox.setLayoutData(gd);

		fItalicCheckBox = new Button(stylesComposite, SWT.CHECK);
		fItalicCheckBox
				.setText(PreferencesMessages.ErlEditorPreferencePage_italic);
		gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
		gd.horizontalIndent = 20;
		gd.horizontalSpan = 2;
		fItalicCheckBox.setLayoutData(gd);

		fStrikethroughCheckBox = new Button(stylesComposite, SWT.CHECK);
		fStrikethroughCheckBox
				.setText(PreferencesMessages.ErlEditorPreferencePage_strikeout);
		gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
		gd.horizontalIndent = 20;
		gd.horizontalSpan = 2;
		fStrikethroughCheckBox.setLayoutData(gd);

		fUnderlineCheckBox = new Button(stylesComposite, SWT.CHECK);
		fUnderlineCheckBox
				.setText(PreferencesMessages.ErlEditorPreferencePage_underline);
		gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
		gd.horizontalIndent = 20;
		gd.horizontalSpan = 2;
		fUnderlineCheckBox.setLayoutData(gd);

		label = new Label(colorComposite, SWT.LEFT);
		label.setText(PreferencesMessages.ErlEditorPreferencePage_preview);
		label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final Control previewer = createPreviewer(colorComposite);
		gd = new GridData(GridData.FILL_BOTH);
		gd.widthHint = convertWidthInCharsToPixels(20);
		gd.heightHint = convertHeightInCharsToPixels(5);
		previewer.setLayoutData(gd);

		fListViewer
				.addSelectionChangedListener(new ISelectionChangedListener() {

					public void selectionChanged(SelectionChangedEvent event) {
						handleSyntaxColorListSelection();
					}
				});

		foregroundColorButton.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				// final TokenHighlight item = getHighlight();
				// HighlightData data = fColors.get(item);
				// data.setColor(fSyntaxForegroundColorEditor.getColorValue());
			}
		});

		fBoldCheckBox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final TokenHighlight item = getHighlight();
				HighlightData data = fColors.get(item);
				// getPreferenceStore().setValue(item.getBoldKey(),
				// fBoldCheckBox.getSelection());
			}
		});

		fItalicCheckBox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final TokenHighlight item = getHighlight();
				// getPreferenceStore().setValue(item.getItalicKey(),
				// fItalicCheckBox.getSelection());
			}
		});
		fStrikethroughCheckBox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final TokenHighlight item = getHighlight();
				// getPreferenceStore().setValue(item.getStrikethroughKey(),
				// fStrikethroughCheckBox.getSelection());
			}
		});

		fUnderlineCheckBox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final TokenHighlight item = getHighlight();
				// getPreferenceStore().setValue(item.getUnderlineKey(),
				// fUnderlineCheckBox.getSelection());
			}
		});

		fEnableCheckbox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final TokenHighlight item = getHighlight();
				// if (item instanceof SemanticHighlightingColorListItem) {
				// final boolean enable = fEnableCheckbox.getSelection();
				// getPreferenceStore().setValue(
				// ((SemanticHighlightingColorListItem) item)
				// .getEnableKey(), enable);
				// fEnableCheckbox.setSelection(enable);
				// fSyntaxForegroundColorEditor.getButton().setEnabled(enable);
				// fColorEditorLabel.setEnabled(enable);
				// fBoldCheckBox.setEnabled(enable);
				// fItalicCheckBox.setEnabled(enable);
				// fStrikethroughCheckBox.setEnabled(enable);
				// fUnderlineCheckBox.setEnabled(enable);
				// }
			}
		});

		colorComposite.layout(false);

		return colorComposite;
	}

	private void addFiller(Composite composite, int horizontalSpan) {
		final PixelConverter pixelConverter = new PixelConverter(composite);
		final Label filler = new Label(composite, SWT.LEFT);
		final GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
		gd.horizontalSpan = horizontalSpan;
		gd.heightHint = pixelConverter.convertHeightInCharsToPixels(1) / 2;
		filler.setLayoutData(gd);
	}

	private Control createPreviewer(Composite parent) {

		final IPreferenceStore generalTextStore = EditorsUI
				.getPreferenceStore();
		final IPreferenceStore store = new ChainedPreferenceStore(
				new IPreferenceStore[] {
						new PreferencesAdapter(
								createTemporaryCorePreferenceStore()),
						generalTextStore });
		fPreviewViewer = new ProjectionViewer(parent, null, null, false,
				SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
		final SimpleEditorConfiguration configuration = new SimpleEditorConfiguration(
				store, null, fColorManager);
		fPreviewViewer.configure(configuration);

		final Font font = JFaceResources
				.getFont(PreferenceConstants.EDITOR_TEXT_FONT);
		fPreviewViewer.getTextWidget().setFont(font);
		new ErlangSourceViewerUpdater(fPreviewViewer, configuration, store);
		fPreviewViewer.setEditable(false);

		final String content = loadPreviewContentFromFile("ColorSettingPreviewCode.txt"); //$NON-NLS-1$
		final IDocument document = new Document(content);
		fPreviewViewer.setDocument(document);

		return fPreviewViewer.getControl();
	}

	private Preferences createTemporaryCorePreferenceStore() {
		final Preferences result = new Preferences();

		result.setValue(COMPILER_TASK_TAGS, "TASK,TODO"); //$NON-NLS-1$

		return result;
	}

	private String loadPreviewContentFromFile(String filename) {
		String line;
		final String separator = System.getProperty("line.separator"); //$NON-NLS-1$
		final StringBuilder buffer = new StringBuilder(512);
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new InputStreamReader(getClass()
					.getResourceAsStream(filename)));
			while ((line = reader.readLine()) != null) {
				buffer.append(line);
				buffer.append(separator);
			}
		} catch (final IOException io) {
			ErlangPlugin.log(io);
		} finally {
			if (reader != null) {
				try {
					reader.close();
				} catch (final IOException e) {
				}
			}
		}
		return buffer.toString();
	}

	/**
	 * Returns the current highlighting color list item.
	 * 
	 * @return the current highlighting color list item
	 * 
	 */
	TokenHighlight getHighlight() {
		final IStructuredSelection selection = (IStructuredSelection) fListViewer
				.getSelection();
		final Object element = selection.getFirstElement();
		if (element instanceof String) {
			return null;
		}
		return (TokenHighlight) element;
	}

	@Override
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);
		return createSyntaxPage(parent);
	}

}
