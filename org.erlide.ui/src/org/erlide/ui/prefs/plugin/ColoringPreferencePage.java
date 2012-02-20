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

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
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
import org.eclipse.swt.graphics.Cursor;
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
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.editors.text.TextSourceViewerConfiguration;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.erlide.core.ErlangCoreOptions;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.ErlangDocumentSetupParticipant;
import org.erlide.ui.editors.erl.SyntaxColorPreviewEditorConfiguration;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.HighlightStyle;
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
    public ColoringPreferencePage() {
    }

    private static final String COMPILER_TASK_TAGS = ErlangCoreOptions.COMPILER_TASK_TAGS
            .getValue();

    public static final String COLORS_QUALIFIER = ErlideUIPlugin.PLUGIN_ID
            + "/editor/colors/";

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

    Map<TokenHighlight, HighlightStyle> fColors;

    /**
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(final IWorkbench workbench) {
        fColorManager = new ColorManager();

        loadColors();
    }

    static class ColorListLabelProvider extends LabelProvider {

        @Override
        public String getText(final Object element) {
            if (element instanceof String) {
                return (String) element;
            }
            final String name = ((TokenHighlight) element).getName();
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
                    return fColors.keySet().toArray();
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

        loadColors();

        handleSyntaxColorListSelection();
        fPreviewViewer.invalidateTextPresentation();
    }

    private void loadColors() {
        fColors = new HashMap<TokenHighlight, HighlightStyle>();
        for (final TokenHighlight th : TokenHighlight.values()) {
            final HighlightStyle data = new HighlightStyle();
            data.load(COLORS_QUALIFIER + th.getName(), th.getDefaultData());
            fColors.put(th, data);
        }
    }

    @Override
    public boolean performOk() {
        for (final TokenHighlight th : fColors.keySet()) {
            final IEclipsePreferences node = new InstanceScope()
                    .getNode(COLORS_QUALIFIER + th.getName());
            final HighlightStyle data = fColors.get(th);

            // TODO only if different than default!
            data.store(node);
        }
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
        final HighlightStyle style = fColors.get(item);
        fSyntaxForegroundColorEditor.setColorValue(style.getColor());
        fBoldCheckBox.setSelection(style.hasStyle(SWT.BOLD));
        fItalicCheckBox.setSelection(style.hasStyle(SWT.ITALIC));
        fStrikethroughCheckBox.setSelection(style
                .hasStyle(TextAttribute.STRIKETHROUGH));
        fUnderlineCheckBox
                .setSelection(style.hasStyle(TextAttribute.UNDERLINE));

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
        fSyntaxForegroundColorEditor.getButton().setEnabled(true);
        fColorEditorLabel.setEnabled(true);
        fBoldCheckBox.setEnabled(true);
        fItalicCheckBox.setEnabled(true);
        fStrikethroughCheckBox.setEnabled(true);
        fUnderlineCheckBox.setEnabled(true);
        fEnableCheckbox.setEnabled(false);
        fEnableCheckbox.setSelection(true);
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
                PreferencesUtil.createPreferenceDialogOn(parent.getShell(),
                        e.text, null, null);
            }
        });

        final GridData gridData = new GridData(SWT.FILL, SWT.BEGINNING, true,
                false);
        gridData.widthHint = 150;
        gridData.horizontalSpan = 2;
        link.setLayoutData(gridData);

        addFiller(colorComposite, 1);

        Label label;
        label = new Label(colorComposite, SWT.LEFT);
        label.setText(PreferencesMessages.ErlEditorPreferencePage_coloring_element);
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
        final Tree tree = fListViewer.getTree();
        final GridData gd_tree = new GridData(SWT.FILL, SWT.FILL, false, false,
                1, 1);
        gd_tree.widthHint = 100;
        tree.setLayoutData(gd_tree);
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
        for (final TokenHighlight item : fColors.keySet()) {
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
        fListViewer.expandAll();

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
        new Label(stylesComposite, SWT.NONE);

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
        label.setText(PreferencesMessages.ErlEditorPreferencePage_preview
                + "\n  - Currently the preview doesn't work, but this dialog does."
                + "\n  - Open files need to be reopened to refresh the coloring.");
        label.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));

        final String content = loadPreviewContentFromFile(getClass(),
                "ColorSettingPreviewCode.txt"); //$NON-NLS-1$
        fPreviewViewer = createErlangPreviewer(colorComposite, fColorManager,
                fColors, content);
        final Control previewer = fPreviewViewer.getControl();
        gd = new GridData(GridData.FILL_BOTH);
        gd.widthHint = convertWidthInCharsToPixels(20);
        gd.heightHint = convertHeightInCharsToPixels(5);
        gd.grabExcessHorizontalSpace = true;
        previewer.setLayoutData(gd);

        fListViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {

                    @Override
                    public void selectionChanged(
                            final SelectionChangedEvent event) {
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
                final HighlightStyle data = fColors.get(item);
                if (data == null) {
                    return;
                }
                data.setColor(fSyntaxForegroundColorEditor.getColorValue());
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
                final HighlightStyle data = fColors.get(item);
                if (data == null) {
                    return;
                }
                data.setStyle(SWT.BOLD, fBoldCheckBox.getSelection());
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
                final HighlightStyle data = fColors.get(item);
                if (data == null) {
                    return;
                }
                data.setStyle(SWT.ITALIC, fItalicCheckBox.getSelection());
                fPreviewViewer.invalidateTextPresentation();
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
                final HighlightStyle data = fColors.get(item);
                if (data == null) {
                    return;
                }
                data.setStyle(TextAttribute.STRIKETHROUGH,
                        fStrikethroughCheckBox.getSelection());
                fPreviewViewer.invalidateTextPresentation();
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
                final HighlightStyle data = fColors.get(item);
                if (data == null) {
                    return;
                }
                data.setStyle(TextAttribute.UNDERLINE,
                        fUnderlineCheckBox.getSelection());
                fPreviewViewer.invalidateTextPresentation();
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

    public static SourceViewer createErlangPreviewer(final Composite parent,
            IColorManager colorManager,
            Map<TokenHighlight, HighlightStyle> colors, final String content) {
        // TODO we should move this method, to a utility class (or maybe create
        // an ErlangPreviewSourceViewer class)
        if (colorManager == null) {
            colorManager = new ColorManager();
        }
        if (colors == null) {
            colors = new HashMap<TokenHighlight, HighlightStyle>();
            for (final TokenHighlight th : TokenHighlight.values()) {
                colors.put(th, th.getDefaultData());
            }
        }

        final IPreferenceStore generalTextStore = EditorsUI
                .getPreferenceStore();
        final IPreferenceStore store = new ChainedPreferenceStore(
                new IPreferenceStore[] {
                        ErlideUIPlugin.getDefault().getPreferenceStore(),
                        generalTextStore });

        final SourceViewer viewer = new SourceViewer(parent, null, null, false,
                SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
        final IDocument document = new Document(content);
        viewer.setDocument(document);

        final ErlangDocumentSetupParticipant setupParticipant = new ErlangDocumentSetupParticipant();
        setupParticipant.setup(document);

        final TextSourceViewerConfiguration configuration = new SyntaxColorPreviewEditorConfiguration(
                store, colorManager, colors);
        viewer.configure(configuration);

        final Font font = JFaceResources
                .getFont(PreferenceConstants.EDITOR_TEXT_FONT);
        viewer.getTextWidget().setFont(font);
        new ErlangSourceViewerUpdater(viewer, configuration, store);
        viewer.setEditable(false);

        final Cursor arrowCursor = viewer.getTextWidget().getDisplay()
                .getSystemCursor(SWT.CURSOR_ARROW);
        viewer.getTextWidget().setCursor(arrowCursor);

        return viewer;
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
        if (element instanceof String) {
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
