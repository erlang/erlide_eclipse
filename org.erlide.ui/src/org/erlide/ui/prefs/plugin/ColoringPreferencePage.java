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
import java.util.List;

import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Scrollable;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.ErlangSourceViewer;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.HighlightStyle;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.util.IColorManager;
import org.erlide.ui.util.OverlayPreferenceStore;
import org.erlide.ui.util.PixelConverter;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Lists;

/**
 * The color preferences.
 *
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ColoringPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {

    public static final String COLORS_QUALIFIER = "editor_colors_";
    public static final String STYLE_KEY = "style";
    public static final String COLOR_KEY = "color";
    public static final String OLD_COLORS_QUALIFIER = "org.erlide.ui/editor/colors";

    final String fErlangCategory = PreferencesMessages.ErlEditorPreferencePage_coloring_category_erlang;

    ColorSelector fSyntaxForegroundColorEditor;
    Label fColorEditorLabel;
    Button fEnableCheckbox;
    Button fBoldCheckBox;
    Button fItalicCheckBox;
    Button fStrikethroughCheckBox;
    Button fUnderlineCheckBox;

    private TreeViewer fListViewer;
    private IColorManager fColorManager;
    SourceViewer fPreviewViewer;

    List<TokenHighlight> fColors;
    private OverlayPreferenceStore fOverlayStore;

    public ColoringPreferencePage() {
        fColors = new ArrayList<TokenHighlight>();
    }

    /**
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(final IWorkbench workbench) {
        fColorManager = new ColorManager();
        fOverlayStore = new OverlayPreferenceStore(ErlideUIPlugin.getDefault()
                .getPreferenceStore(), new OverlayPreferenceStore.OverlayKey[] {});
        fOverlayStore.addKeys(createOverlayStoreKeys());
        fOverlayStore.load();
        fOverlayStore.start();

        for (final TokenHighlight th : TokenHighlight.values()) {
            fColors.add(th);
        }
    }

    private OverlayPreferenceStore.OverlayKey[] createOverlayStoreKeys() {
        final List<OverlayPreferenceStore.OverlayKey> overlayKeys = Lists.newArrayList();
        for (final TokenHighlight item : TokenHighlight.values()) {
            overlayKeys.add(new OverlayPreferenceStore.OverlayKey(
                    OverlayPreferenceStore.TypeDescriptor.STRING, item.getColorKey()));
            overlayKeys.add(new OverlayPreferenceStore.OverlayKey(
                    OverlayPreferenceStore.TypeDescriptor.INT, item.getStylesKey()));
        }
        final OverlayPreferenceStore.OverlayKey[] keys = new OverlayPreferenceStore.OverlayKey[overlayKeys
                .size()];
        overlayKeys.toArray(keys);
        return keys;
    }

    static class ColorListLabelProvider extends LabelProvider {

        @Override
        public String getText(final Object element) {
            if (element instanceof String) {
                return (String) element;
            }
            final String name = ((TokenHighlight) element).getDisplayName();
            final char c = Character.toUpperCase(name.charAt(0));
            return c + name.substring(1);
        }
    }

    class ColorListContentProvider implements ITreeContentProvider {

        @Override
        public Object[] getElements(final Object inputElement) {
            return new String[] { fErlangCategory
            // , fEdocCategory
            };
        }

        @Override
        public void dispose() {
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
        }

        @Override
        public Object[] getChildren(final Object parentElement) {
            if (parentElement instanceof String) {
                final String entry = (String) parentElement;
                if (fErlangCategory.equals(entry)) {
                    return fColors.toArray();
                }
                // if (fEdocCategory.equals(entry)) {
                // return fListModel.subList(0, 4).toArray();
                // }
            }
            return new Object[0];
        }

        @Override
        public Object getParent(final Object element) {
            if (element instanceof String) {
                return null;
            }
            // final int index = fListModel.indexOf(element);
            // if (index < 4) {
            // return fEdocCategory;
            // }
            return fErlangCategory;
        }

        @Override
        public boolean hasChildren(final Object element) {
            return element instanceof String;
        }
    }

    @Override
    public void performDefaults() {
        super.performDefaults();
        fOverlayStore.loadDefaults();
        handleSyntaxColorListSelection();
        fPreviewViewer.invalidateTextPresentation();
    }

    public void storeHighlight(final IPreferenceStore store, final TokenHighlight th,
            final HighlightStyle style) {
        if (store != null) {
            store.setValue(th.getColorKey(), StringConverter.asString(style.getColor()));
            store.setValue(th.getStylesKey(), style.getStyles());
            ErlLogger.debug("Store colors:: %s: %s; %s: %d", th.getColorKey(),
                    StringConverter.asString(style.getColor()), th.getStylesKey(),
                    style.getStyles());
        }
    }

    @Override
    public boolean performOk() {
        for (final TokenHighlight th : fColors) {
            final HighlightStyle data = th.getStyle(fOverlayStore);
            storeHighlight(fOverlayStore, th, data);
        }
        fOverlayStore.propagate();
        return super.performOk();
    }

    @Override
    public void dispose() {
        fColorManager.dispose();
        if (fOverlayStore != null) {
            fOverlayStore.stop();
            fOverlayStore = null;
        }
        super.dispose();
    }

    void handleSyntaxColorListSelection() {
        final TokenHighlight item = getHighlight();

        fEnableCheckbox.setEnabled(item != null);
        fSyntaxForegroundColorEditor.getButton().setEnabled(item != null);
        fColorEditorLabel.setEnabled(item != null);
        fBoldCheckBox.setEnabled(item != null);
        fItalicCheckBox.setEnabled(item != null);
        fStrikethroughCheckBox.setEnabled(item != null);
        fUnderlineCheckBox.setEnabled(item != null);

        if (item == null) {
            return;
        }

        final HighlightStyle style = item.getStyle(fOverlayStore);
        fSyntaxForegroundColorEditor.setColorValue(style.getColor());
        fBoldCheckBox.setSelection(style.hasStyle(SWT.BOLD));
        fItalicCheckBox.setSelection(style.hasStyle(SWT.ITALIC));
        fStrikethroughCheckBox.setSelection(style.hasStyle(TextAttribute.STRIKETHROUGH));
        fUnderlineCheckBox.setSelection(style.hasStyle(TextAttribute.UNDERLINE));

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
        // }
    }

    private Control createSyntaxPage(final Composite parent) {
        final Composite colorComposite = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        colorComposite.setLayout(layout);

        final Link link = new Link(colorComposite, SWT.NONE);
        link.setText(PreferencesMessages.ErlEditorColoringConfigurationBlock_link);
        link.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                PreferencesUtil.createPreferenceDialogOn(parent.getShell(), e.text, null,
                        null);
            }
        });

        final GridData gridData = new GridData(SWT.FILL, SWT.BEGINNING, true, false);
        gridData.widthHint = 150;
        gridData.horizontalSpan = 2;
        link.setLayoutData(gridData);

        addFiller(colorComposite, 1);

        Label label;
        label = new Label(colorComposite, SWT.LEFT);
        label.setText(PreferencesMessages.ErlEditorPreferencePage_coloring_element);
        label.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        final Composite editorComposite = new Composite(colorComposite, SWT.NONE);
        layout = new GridLayout();
        layout.numColumns = 2;
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        editorComposite.setLayout(layout);
        GridData gd = new GridData(SWT.FILL, SWT.BEGINNING, true, false);
        editorComposite.setLayoutData(gd);

        fListViewer = new TreeViewer(editorComposite, SWT.SINGLE | SWT.BORDER);
        final Tree tree = fListViewer.getTree();
        final GridData gdTree = new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1);
        gdTree.widthHint = 100;
        tree.setLayoutData(gdTree);
        fListViewer.setLabelProvider(new ColorListLabelProvider());
        fListViewer.setContentProvider(new ColorListContentProvider());
        fListViewer.setInput(fColors);
        fListViewer.setSelection(new StructuredSelection(fErlangCategory));
        fListViewer.setSorter(new ViewerSorter() {

            @Override
            public int category(final Object element) {
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
        for (final TokenHighlight item : fColors) {
            maxWidth = Math.max(maxWidth, convertWidthInCharsToPixels(item.getName()
                    .length()));
        }
        final ScrollBar vBar = ((Scrollable) fListViewer.getControl()).getVerticalBar();
        if (vBar != null) {
            maxWidth += vBar.getSize().x * 3; // scrollbars and tree
        }
        // indentation guess
        gd.widthHint = maxWidth;

        fListViewer.getControl().setLayoutData(gd);
        fListViewer.expandAll();

        final Composite stylesComposite = new Composite(editorComposite, SWT.NONE);
        layout = new GridLayout();
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        layout.numColumns = 2;
        stylesComposite.setLayout(layout);
        stylesComposite.setLayoutData(new GridData(GridData.FILL_BOTH));

        fEnableCheckbox = new Button(stylesComposite, SWT.CHECK);
        fEnableCheckbox.setText(PreferencesMessages.ErlEditorPreferencePage_enable);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalAlignment = GridData.BEGINNING;
        gd.horizontalSpan = 2;
        fEnableCheckbox.setLayoutData(gd);
        // TODO hide this until reworking the dialog
        fEnableCheckbox.setVisible(false);

        fColorEditorLabel = new Label(stylesComposite, SWT.LEFT);
        fColorEditorLabel.setText(PreferencesMessages.ErlEditorPreferencePage_color);
        gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        gd.horizontalIndent = 20;
        fColorEditorLabel.setLayoutData(gd);

        fSyntaxForegroundColorEditor = new ColorSelector(stylesComposite);
        final Button foregroundColorButton = fSyntaxForegroundColorEditor.getButton();
        gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        foregroundColorButton.setLayoutData(gd);
        new Label(stylesComposite, SWT.NONE);

        fBoldCheckBox = new Button(stylesComposite, SWT.CHECK);
        fBoldCheckBox.setText(PreferencesMessages.ErlEditorPreferencePage_bold);
        gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        gd.horizontalIndent = 20;
        gd.horizontalSpan = 2;
        fBoldCheckBox.setLayoutData(gd);

        fItalicCheckBox = new Button(stylesComposite, SWT.CHECK);
        fItalicCheckBox.setText(PreferencesMessages.ErlEditorPreferencePage_italic);
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
        fUnderlineCheckBox.setText(PreferencesMessages.ErlEditorPreferencePage_underline);
        gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        gd.horizontalIndent = 20;
        gd.horizontalSpan = 2;
        fUnderlineCheckBox.setLayoutData(gd);

        final String content = loadPreviewContentFromFile(getClass(),
                "ColorSettingPreviewCode.txt"); //$NON-NLS-1$
        fPreviewViewer = ErlangSourceViewer.createErlangPreviewer(colorComposite,
                fColorManager, fOverlayStore, fColors, content);

        final Control previewer = fPreviewViewer.getControl();
        gd = new GridData(GridData.FILL_BOTH);
        gd.widthHint = convertWidthInCharsToPixels(20);
        gd.heightHint = convertHeightInCharsToPixels(5);
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        previewer.setLayoutData(gd);

        fListViewer.addSelectionChangedListener(new ISelectionChangedListener() {

            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
                handleSyntaxColorListSelection();
            }
        });

        foregroundColorButton.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                // do nothing
            }

            @Override
            public void widgetSelected(final SelectionEvent e) {
                final TokenHighlight item = getHighlight();
                final HighlightStyle data = item.getStyle(fOverlayStore);
                if (data == null) {
                    return;
                }
                data.setColor(fSyntaxForegroundColorEditor.getColorValue());
                storeHighlight(fOverlayStore, item, data);
                fPreviewViewer.invalidateTextPresentation();
            }
        });

        fBoldCheckBox.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                // do nothing
            }

            @Override
            public void widgetSelected(final SelectionEvent e) {
                final TokenHighlight item = getHighlight();
                final HighlightStyle data = item.getStyle(fOverlayStore);
                if (data == null) {
                    return;
                }
                data.setStyle(SWT.BOLD, fBoldCheckBox.getSelection());
                storeHighlight(fOverlayStore, item, data);
                fPreviewViewer.invalidateTextPresentation();
            }
        });

        fItalicCheckBox.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                // do nothing
            }

            @Override
            public void widgetSelected(final SelectionEvent e) {
                final TokenHighlight item = getHighlight();
                final HighlightStyle data = item.getStyle(fOverlayStore);
                if (data == null) {
                    return;
                }
                data.setStyle(SWT.ITALIC, fItalicCheckBox.getSelection());
                storeHighlight(fOverlayStore, item, data);
            }
        });
        fStrikethroughCheckBox.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                // do nothing
            }

            @Override
            public void widgetSelected(final SelectionEvent e) {
                final TokenHighlight item = getHighlight();
                final HighlightStyle data = item.getStyle(fOverlayStore);
                if (data == null) {
                    return;
                }
                data.setStyle(TextAttribute.STRIKETHROUGH,
                        fStrikethroughCheckBox.getSelection());
                storeHighlight(fOverlayStore, item, data);
            }
        });

        fUnderlineCheckBox.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                // do nothing
            }

            @Override
            public void widgetSelected(final SelectionEvent e) {
                final TokenHighlight item = getHighlight();
                final HighlightStyle data = item.getStyle(fOverlayStore);
                if (data == null) {
                    return;
                }
                data.setStyle(TextAttribute.UNDERLINE, fUnderlineCheckBox.getSelection());
                storeHighlight(fOverlayStore, item, data);
            }
        });

        fEnableCheckbox.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                // do nothing
            }

            @Override
            public void widgetSelected(final SelectionEvent e) {
                fEnableCheckbox.setSelection(true);
                // final TokenHighlight item = getHighlight();
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

        handleSyntaxColorListSelection();
        fPreviewViewer.invalidateTextPresentation();
        return colorComposite;
    }

    private void addFiller(final Composite composite, final int horizontalSpan) {
        final PixelConverter pixelConverter = new PixelConverter(composite);
        final Label filler = new Label(composite, SWT.LEFT);
        final GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
        gd.horizontalSpan = horizontalSpan;
        gd.heightHint = pixelConverter.convertHeightInCharsToPixels(1) / 2;
        filler.setLayoutData(gd);
    }

    public static String loadPreviewContentFromFile(final Class<?> clazz,
            final String filename) {
        String line;
        final String separator = System.getProperty("line.separator"); //$NON-NLS-1$
        final StringBuilder buffer = new StringBuilder(512);
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(
                    clazz.getResourceAsStream(filename)));
            while ((line = reader.readLine()) != null) {
                buffer.append(line);
                buffer.append(separator);
            }
        } catch (final IOException io) {
            ErlLogger.error(io);
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
        if (fListViewer == null) {
            return null;
        }
        final IStructuredSelection selection = (IStructuredSelection) fListViewer
                .getSelection();
        final Object element = selection.getFirstElement();
        if (element == null || element instanceof String) {
            return null;
        }
        return (TokenHighlight) element;
    }

    @Override
    protected Control createContents(final Composite parent) {
        initializeDialogUnits(parent);
        return createSyntaxPage(parent);
    }

}
