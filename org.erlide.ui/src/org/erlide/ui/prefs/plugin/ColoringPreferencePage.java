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
import java.util.ArrayList;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
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
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
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
import org.erlide.ui.prefs.PreferenceConstants;
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

	/**
	 * Default constructor
	 * 
	 */
	public ColoringPreferencePage() {
		super();
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
		// Set the preference store for the preference page.
		final IPreferenceStore store = ErlideUIPlugin.getDefault()
				.getPreferenceStore();
		setPreferenceStore(store);

		fColorManager = new ColorManager();

		for (String[] element : fSyntaxColorListModel) {
			fListModel.add(new HighlightingColorListItem(element[0],
					element[1], element[1] + BOLD, element[1] + ITALIC,
					element[1] + STRIKETHROUGH, element[1] + UNDERLINE));
		}

		// store.addKeys(createOverlayStoreKeys());
	}

	// ////////////

	/**
	 * Item in the highlighting color list.
	 */
	private static class HighlightingColorListItem {

		/** Display name */
		private final String fDisplayName;

		/** Color preference key */
		private final String fColorKey;

		/** Bold preference key */
		private final String fBoldKey;

		/** Italic preference key */
		private final String fItalicKey;

		/**
		 * Strikethrough preference key.
		 */
		private final String fStrikethroughKey;

		/**
		 * Underline preference key.
		 */
		private final String fUnderlineKey;

		/**
		 * Initialize the item with the given values.
		 * 
		 * @param displayName
		 *            the display name
		 * @param colorKey
		 *            the color preference key
		 * @param boldKey
		 *            the bold preference key
		 * @param italicKey
		 *            the italic preference key
		 * @param strikethroughKey
		 *            the strikethrough preference key
		 * @param underlineKey
		 *            the underline preference key
		 */
		public HighlightingColorListItem(String displayName, String colorKey,
				String boldKey, String italicKey, String strikethroughKey,
				String underlineKey) {
			fDisplayName = displayName;
			fColorKey = colorKey;
			fBoldKey = boldKey;
			fItalicKey = italicKey;
			fStrikethroughKey = strikethroughKey;
			fUnderlineKey = underlineKey;
		}

		/**
		 * @return the bold preference key
		 */
		public String getBoldKey() {
			return fBoldKey;
		}

		/**
		 * @return the bold preference key
		 */
		public String getItalicKey() {
			return fItalicKey;
		}

		/**
		 * @return the strikethrough preference key
		 * 
		 */
		public String getStrikethroughKey() {
			return fStrikethroughKey;
		}

		/**
		 * @return the underline preference key
		 * 
		 */
		public String getUnderlineKey() {
			return fUnderlineKey;
		}

		/**
		 * @return the color preference key
		 */
		public String getColorKey() {
			return fColorKey;
		}

		/**
		 * @return the display name
		 */
		public String getDisplayName() {
			return fDisplayName;
		}
	}

	private static class SemanticHighlightingColorListItem extends
			HighlightingColorListItem {

		/** Enablement preference key */
		private final String fEnableKey;

		/**
		 * Initialize the item with the given values.
		 * 
		 * @param displayName
		 *            the display name
		 * @param colorKey
		 *            the color preference key
		 * @param boldKey
		 *            the bold preference key
		 * @param italicKey
		 *            the italic preference key
		 * @param strikethroughKey
		 *            the strikethroughKey preference key
		 * @param underlineKey
		 *            the underlineKey preference key
		 * @param enableKey
		 *            the enable preference key
		 */
		public SemanticHighlightingColorListItem(String displayName,
				String colorKey, String boldKey, String italicKey,
				String strikethroughKey, String underlineKey, String enableKey) {
			super(displayName, colorKey, boldKey, italicKey, strikethroughKey,
					underlineKey);
			fEnableKey = enableKey;
		}

		/**
		 * @return the enablement preference key
		 */
		public String getEnableKey() {
			return fEnableKey;
		}
	}

	/**
	 * Color list label provider.
	 * 
	 */
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
			return ((HighlightingColorListItem) element).getDisplayName();
		}
	}

	/**
	 * Color list content provider.
	 * 
	 */
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
					return fListModel.toArray();
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

	private static final String BOLD = PreferenceConstants.EDITOR_BOLD_SUFFIX;
	private static final String ITALIC = PreferenceConstants.EDITOR_ITALIC_SUFFIX;
	private static final String STRIKETHROUGH = PreferenceConstants.EDITOR_STRIKETHROUGH_SUFFIX;
	private static final String UNDERLINE = PreferenceConstants.EDITOR_UNDERLINE_SUFFIX;

	private static final String COMPILER_TASK_TAGS = ErlangCore.COMPILER_TASK_TAGS;

	/**
	 * The keys of the overlay store.
	 */
	private final String[][] fSyntaxColorListModel = new String[][] {
			{ PreferencesMessages.ErlEditorPreferencePage_comment,
					PreferenceConstants.COMMENT },
			{ PreferencesMessages.ErlEditorPreferencePage_attribute,
					PreferenceConstants.ATTRIBUTE },
			{ PreferencesMessages.ErlEditorPreferencePage_string,
					PreferenceConstants.STRING },
			{ PreferencesMessages.ErlEditorPreferencePage_default,
					PreferenceConstants.DEFAULT },
			{ PreferencesMessages.ErlEditorPreferencePage_keyword,
					PreferenceConstants.KEYWORD },
			{ PreferencesMessages.ErlEditorPreferencePage_variable,
					PreferenceConstants.VARIABLE },
			{ PreferencesMessages.ErlEditorPreferencePage_guard,
					PreferenceConstants.GUARD },
			{ PreferencesMessages.ErlEditorPreferencePage_macro,
					PreferenceConstants.MACRO },
			{ PreferencesMessages.ErlEditorPreferencePage_record,
					PreferenceConstants.RECORD },
			{ PreferencesMessages.ErlEditorPreferencePage_bif,
					PreferenceConstants.BIF },
			{ PreferencesMessages.ErlEditorPreferencePage_char,
					PreferenceConstants.CHAR },
			{ PreferencesMessages.ErlEditorPreferencePage_atom,
					PreferenceConstants.ATOM },
			{ PreferencesMessages.ErlEditorPreferencePage_arrow,
					PreferenceConstants.ARROW },
			{ PreferencesMessages.ErlEditorPreferencePage_integer,
					PreferenceConstants.INTEGER },
			{ PreferencesMessages.ErlEditorPreferencePage_float,
					PreferenceConstants.FLOAT }

	};

	final String fErlangCategory = PreferencesMessages.ErlEditorPreferencePage_coloring_category_erlang;

	// final String fEdocCategory =
	// PreferencesMessages.ErlEditorPreferencePage_coloring_category_edoc;

	ColorSelector fSyntaxForegroundColorEditor;

	Label fColorEditorLabel;

	Button fBoldCheckBox;

	Button fEnableCheckbox;

	Button fItalicCheckBox;

	Button fStrikethroughCheckBox;

	Button fUnderlineCheckBox;

	/**
	 * Highlighting color list
	 */
	final java.util.List<HighlightingColorListItem> fListModel = new ArrayList<HighlightingColorListItem>();

	/**
	 * Highlighting color list viewer
	 */
	private StructuredViewer fListViewer;

	private IColorManager fColorManager;

	private SourceViewer fPreviewViewer;

	private FontMetrics fFontMetrics;

	/* NOT USED */
	/*
	 * private OverlayKey[] createOverlayStoreKeys() {
	 * 
	 * final ArrayList<OverlayKey> overlayKeys = new ArrayList<OverlayKey>();
	 * 
	 * for (int i = 0, n = fListModel.size(); i < n; i++) { final
	 * HighlightingColorListItem item = (HighlightingColorListItem) fListModel
	 * .get(i); overlayKeys.add(new OverlayKey(
	 * OverlayPreferenceStore.TypeDescriptor.STRING, item .getColorKey()));
	 * overlayKeys.add(new OverlayKey(
	 * OverlayPreferenceStore.TypeDescriptor.BOOLEAN, item .getBoldKey()));
	 * overlayKeys.add(new OverlayKey(
	 * OverlayPreferenceStore.TypeDescriptor.BOOLEAN, item .getItalicKey()));
	 * overlayKeys.add(new OverlayKey(
	 * OverlayPreferenceStore.TypeDescriptor.BOOLEAN, item
	 * .getStrikethroughKey())); overlayKeys.add(new OverlayKey(
	 * OverlayPreferenceStore.TypeDescriptor.BOOLEAN, item .getUnderlineKey()));
	 * 
	 * if (item instanceof SemanticHighlightingColorListItem) {
	 * overlayKeys.add(new OverlayKey(
	 * OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
	 * ((SemanticHighlightingColorListItem) item) .getEnableKey())); } }
	 * 
	 * final OverlayKey[] keys = new OverlayKey[overlayKeys.size()];
	 * overlayKeys.toArray(keys); return keys; }
	 */

	/**
	 * Returns the number of pixels corresponding to the width of the given
	 * number of characters.
	 * <p>
	 * This method may only be called after <code>initializeDialogUnits</code>
	 * has been called.
	 * </p>
	 * <p>
	 * Clients may call this framework method, but should not override it.
	 * </p>
	 * 
	 * @param chars
	 *            the number of characters
	 * @return the number of pixels
	 */
	@Override
	protected int convertWidthInCharsToPixels(int chars) {
		// test for failure to initialize for backward compatibility
		if (fFontMetrics == null) {
			return 0;
		}
		return Dialog.convertWidthInCharsToPixels(fFontMetrics, chars);
	}

	/**
	 * Returns the number of pixels corresponding to the height of the given
	 * number of characters.
	 * <p>
	 * This method may only be called after <code>initializeDialogUnits</code>
	 * has been called.
	 * </p>
	 * <p>
	 * Clients may call this framework method, but should not override it.
	 * </p>
	 * 
	 * @param chars
	 *            the number of characters
	 * @return the number of pixels
	 */
	@Override
	protected int convertHeightInCharsToPixels(int chars) {
		// test for failure to initialize for backward compatibility
		if (fFontMetrics == null) {
			return 0;
		}
		return Dialog.convertHeightInCharsToPixels(fFontMetrics, chars);
	}

	@Override
	public void performDefaults() {
		super.performDefaults();

		handleSyntaxColorListSelection();

		fPreviewViewer.invalidateTextPresentation();
	}

	@Override
	public boolean performOk() {
		IPreferenceStore store = getPreferenceStore();
		if (store instanceof IPersistentPreferenceStore) {
			try {
				((IPersistentPreferenceStore) store).save();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return super.performOk();
	}

	@Override
	public void dispose() {
		fColorManager.dispose();

		super.dispose();
	}

	void handleSyntaxColorListSelection() {
		final HighlightingColorListItem item = getHighlightingColorListItem();
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
		final RGB rgb = PreferenceConverter.getColor(getPreferenceStore(), item
				.getColorKey());
		fSyntaxForegroundColorEditor.setColorValue(rgb);
		fBoldCheckBox.setSelection(getPreferenceStore().getBoolean(
				item.getBoldKey()));
		fItalicCheckBox.setSelection(getPreferenceStore().getBoolean(
				item.getItalicKey()));
		fStrikethroughCheckBox.setSelection(getPreferenceStore().getBoolean(
				item.getStrikethroughKey()));
		fUnderlineCheckBox.setSelection(getPreferenceStore().getBoolean(
				item.getUnderlineKey()));
		if (item instanceof SemanticHighlightingColorListItem) {
			fEnableCheckbox.setEnabled(true);
			final boolean enable = getPreferenceStore().getBoolean(
					((SemanticHighlightingColorListItem) item).getEnableKey());
			fEnableCheckbox.setSelection(enable);
			fSyntaxForegroundColorEditor.getButton().setEnabled(enable);
			fColorEditorLabel.setEnabled(enable);
			fBoldCheckBox.setEnabled(enable);
			fItalicCheckBox.setEnabled(enable);
			fStrikethroughCheckBox.setEnabled(enable);
			fUnderlineCheckBox.setEnabled(enable);
		} else {
			fSyntaxForegroundColorEditor.getButton().setEnabled(true);
			fColorEditorLabel.setEnabled(true);
			fBoldCheckBox.setEnabled(true);
			fItalicCheckBox.setEnabled(true);
			fStrikethroughCheckBox.setEnabled(true);
			fUnderlineCheckBox.setEnabled(true);
			fEnableCheckbox.setEnabled(false);
			fEnableCheckbox.setSelection(true);
		}
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
		fListViewer.setInput(fListModel);
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
		for (Object element : fListModel) {
			final HighlightingColorListItem item = (HighlightingColorListItem) element;
			maxWidth = Math.max(maxWidth, convertWidthInCharsToPixels(item
					.getDisplayName().length()));
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
				.setText(PreferencesMessages.ErlEditorPreferencePage_strikethrough);
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
				final HighlightingColorListItem item = getHighlightingColorListItem();
				PreferenceConverter.setValue(getPreferenceStore(), item
						.getColorKey(), fSyntaxForegroundColorEditor
						.getColorValue());
			}
		});

		fBoldCheckBox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final HighlightingColorListItem item = getHighlightingColorListItem();
				getPreferenceStore().setValue(item.getBoldKey(),
						fBoldCheckBox.getSelection());
			}
		});

		fItalicCheckBox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final HighlightingColorListItem item = getHighlightingColorListItem();
				getPreferenceStore().setValue(item.getItalicKey(),
						fItalicCheckBox.getSelection());
			}
		});
		fStrikethroughCheckBox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final HighlightingColorListItem item = getHighlightingColorListItem();
				getPreferenceStore().setValue(item.getStrikethroughKey(),
						fStrikethroughCheckBox.getSelection());
			}
		});

		fUnderlineCheckBox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final HighlightingColorListItem item = getHighlightingColorListItem();
				getPreferenceStore().setValue(item.getUnderlineKey(),
						fUnderlineCheckBox.getSelection());
			}
		});

		fEnableCheckbox.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}

			public void widgetSelected(SelectionEvent e) {
				final HighlightingColorListItem item = getHighlightingColorListItem();
				if (item instanceof SemanticHighlightingColorListItem) {
					final boolean enable = fEnableCheckbox.getSelection();
					getPreferenceStore().setValue(
							((SemanticHighlightingColorListItem) item)
									.getEnableKey(), enable);
					fEnableCheckbox.setSelection(enable);
					fSyntaxForegroundColorEditor.getButton().setEnabled(enable);
					fColorEditorLabel.setEnabled(enable);
					fBoldCheckBox.setEnabled(enable);
					fItalicCheckBox.setEnabled(enable);
					fStrikethroughCheckBox.setEnabled(enable);
					fUnderlineCheckBox.setEnabled(enable);
				}
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
						getPreferenceStore(),
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
	HighlightingColorListItem getHighlightingColorListItem() {
		final IStructuredSelection selection = (IStructuredSelection) fListViewer
				.getSelection();
		final Object element = selection.getFirstElement();
		if (element instanceof String) {
			return null;
		}
		return (HighlightingColorListItem) element;
	}

	/**
	 * Initializes the computation of horizontal and vertical dialog units based
	 * on the size of current font.
	 * <p>
	 * This method must be called before any of the dialog unit based conversion
	 * methods are called.
	 * </p>
	 * 
	 * @param testControl
	 *            a control from which to obtain the current font
	 */
	@Override
	protected void initializeDialogUnits(Control testControl) {
		// Compute and store a font metric
		final GC gc = new GC(testControl);
		gc.setFont(JFaceResources.getDialogFont());
		fFontMetrics = gc.getFontMetrics();
		gc.dispose();
	}

	@Override
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);
		return createSyntaxPage(parent);
	}

}
