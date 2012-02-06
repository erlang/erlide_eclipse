package org.erlide.ui.prefs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.erlide.ui.editors.erl.folding.IErlangFoldingPreferenceBlock;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.internal.folding.ErlangFoldingStructureProviderDescriptor;
import org.erlide.ui.internal.folding.ErlangFoldingStructureProviderRegistry;
import org.erlide.ui.prefs.plugin.PreferencesMessages;
import org.erlide.ui.util.OverlayPreferenceStore;
import org.erlide.ui.util.PixelConverter;

public class FoldingConfigurationBlock implements IPreferenceConfigurationBlock {

    private static class ErrorPreferences implements
            IErlangFoldingPreferenceBlock {

        private final String fMessage;

        protected ErrorPreferences(final String message) {
            fMessage = message;
        }

        /*
         * @see
         * org.eclipse.jdt.internal.ui.text.folding.IErlangFoldingPreferences#
         * createControl(org.eclipse.swt.widgets.Group)
         */
        @Override
        public Control createControl(final Composite composite) {
            final Composite inner = new Composite(composite, SWT.NONE);
            inner.setLayout(new FillLayout(SWT.VERTICAL));

            final Label label = new Label(inner, SWT.CENTER);
            label.setText(fMessage);

            return inner;
        }

        @Override
        public void initialize() {
        }

        @Override
        public void performOk() {
        }

        @Override
        public void performDefaults() {
        }

        @Override
        public void dispose() {
        }

    }

    /** The overlay preference store. */
    final OverlayPreferenceStore fStore;

    /* The controls */
    private Combo fProviderCombo;

    Button fFoldingCheckbox;

    private ComboViewer fProviderViewer;

    private Composite fGroup;

    private StackLayout fStackLayout;

    /* the model */
    final Map<String, ErlangFoldingStructureProviderDescriptor> fProviderDescriptors;

    private final Map<String, IErlangFoldingPreferenceBlock> fProviderPreferences;

    private final Map<String, Control> fProviderControls;

    public FoldingConfigurationBlock(final OverlayPreferenceStore store) {
        Assert.isNotNull(store);
        fStore = store;
        fStore.addKeys(createOverlayStoreKeys());
        fProviderDescriptors = createListModel();
        fProviderPreferences = new HashMap<String, IErlangFoldingPreferenceBlock>();
        fProviderControls = new HashMap<String, Control>();
    }

    private Map<String, ErlangFoldingStructureProviderDescriptor> createListModel() {
        final ErlangFoldingStructureProviderRegistry reg = ErlideUIPlugin
                .getDefault().getFoldingStructureProviderRegistry();
        reg.reloadExtensions();
        final ErlangFoldingStructureProviderDescriptor[] descs = reg
                .getFoldingProviderDescriptors();
        final Map<String, ErlangFoldingStructureProviderDescriptor> map = new HashMap<String, ErlangFoldingStructureProviderDescriptor>();
        for (final ErlangFoldingStructureProviderDescriptor element : descs) {
            map.put(element.getId(), element);
        }
        return map;
    }

    private OverlayPreferenceStore.OverlayKey[] createOverlayStoreKeys() {

        final ArrayList<OverlayPreferenceStore.OverlayKey> overlayKeys = new ArrayList<OverlayPreferenceStore.OverlayKey>();

        overlayKeys.add(new OverlayPreferenceStore.OverlayKey(
                OverlayPreferenceStore.TypeDescriptor.BOOLEAN,
                PreferenceConstants.EDITOR_FOLDING_ENABLED));
        overlayKeys.add(new OverlayPreferenceStore.OverlayKey(
                OverlayPreferenceStore.TypeDescriptor.STRING,
                PreferenceConstants.EDITOR_FOLDING_PROVIDER));

        return overlayKeys
                .toArray(new OverlayPreferenceStore.OverlayKey[overlayKeys
                        .size()]);
    }

    /**
     * Creates page for folding preferences.
     * 
     * @param parent
     *            the parent composite
     * @return the control for the preference page
     */
    @Override
    public Control createControl(final Composite parent) {

        final Composite composite = new Composite(parent, SWT.NULL);
        // assume parent page uses griddata
        GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_CENTER
                | GridData.VERTICAL_ALIGN_FILL);
        composite.setLayoutData(gd);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        final PixelConverter pc = new PixelConverter(composite);
        layout.verticalSpacing = pc.convertHeightInCharsToPixels(1) / 2;
        composite.setLayout(layout);

        /* check box for new editors */
        fFoldingCheckbox = new Button(composite, SWT.CHECK);
        fFoldingCheckbox
                .setText(PreferencesMessages.FoldingConfigurationBlock_enable);
        gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING
                | GridData.VERTICAL_ALIGN_BEGINNING);
        fFoldingCheckbox.setLayoutData(gd);
        fFoldingCheckbox.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                final boolean enabled = fFoldingCheckbox.getSelection();
                fStore.setValue(PreferenceConstants.EDITOR_FOLDING_ENABLED,
                        enabled);
                updateCheckboxDependencies();
            }

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
            }
        });

        Label label = new Label(composite, SWT.CENTER);
        gd = new GridData(GridData.FILL_HORIZONTAL
                | GridData.VERTICAL_ALIGN_BEGINNING);
        label.setLayoutData(gd);

        if (fProviderDescriptors.size() > 1) {
            /* list */
            final Composite comboComp = new Composite(composite, SWT.NONE);
            gd = new GridData(GridData.FILL_HORIZONTAL
                    | GridData.VERTICAL_ALIGN_BEGINNING);
            final GridLayout gridLayout = new GridLayout(2, false);
            gridLayout.marginWidth = 0;
            comboComp.setLayout(gridLayout);

            final Label comboLabel = new Label(comboComp, SWT.CENTER);
            gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING
                    | GridData.VERTICAL_ALIGN_CENTER);
            comboLabel.setLayoutData(gd);
            comboLabel
                    .setText(PreferencesMessages.FoldingConfigurationBlock_combo_caption);

            label = new Label(composite, SWT.CENTER);
            gd = new GridData(GridData.FILL_HORIZONTAL
                    | GridData.VERTICAL_ALIGN_BEGINNING);
            label.setLayoutData(gd);

            fProviderCombo = new Combo(comboComp, SWT.READ_ONLY | SWT.DROP_DOWN);
            gd = new GridData(GridData.HORIZONTAL_ALIGN_END
                    | GridData.VERTICAL_ALIGN_CENTER);
            fProviderCombo.setLayoutData(gd);

            fProviderViewer = createProviderViewer();
        }

        final Composite groupComp = new Composite(composite, SWT.NONE);
        gd = new GridData(GridData.FILL_BOTH);
        gd.horizontalSpan = 2;
        groupComp.setLayoutData(gd);
        final GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.marginWidth = 0;
        groupComp.setLayout(gridLayout);

        /* contributed provider preferences. */
        fGroup = new Composite(groupComp, SWT.NONE);
        gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING
                | GridData.VERTICAL_ALIGN_BEGINNING);
        fGroup.setLayoutData(gd);
        fStackLayout = new StackLayout();
        fGroup.setLayout(fStackLayout);

        return composite;
    }

    private ComboViewer createProviderViewer() {
        /* list viewer */
        final ComboViewer viewer = new ComboViewer(fProviderCombo);
        viewer.setContentProvider(new IStructuredContentProvider() {

            /*
             * @see org.eclipse.jface.viewers.IContentProvider#dispose()
             */
            @Override
            public void dispose() {
            }

            /*
             * @see
             * org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse
             * .jface.viewers.Viewer, java.lang.Object, java.lang.Object)
             */
            @Override
            public void inputChanged(final Viewer v, final Object oldInput,
                    final Object newInput) {
            }

            /*
             * @see
             * org.eclipse.jface.viewers.IStructuredContentProvider#getElements
             * (java.lang.Object)
             */
            @Override
            public Object[] getElements(final Object inputElement) {
                return fProviderDescriptors.values().toArray();
            }
        });
        viewer.setLabelProvider(new LabelProvider() {

            /*
             * @see
             * org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object
             * )
             */
            @Override
            public Image getImage(final Object element) {
                return null;
            }

            /*
             * @see
             * org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
             */
            @Override
            public String getText(final Object element) {
                return ((ErlangFoldingStructureProviderDescriptor) element)
                        .getName();
            }
        });
        viewer.addSelectionChangedListener(new ISelectionChangedListener() {

            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
                final IStructuredSelection sel = (IStructuredSelection) event
                        .getSelection();
                if (!sel.isEmpty()) {
                    fStore.setValue(
                            PreferenceConstants.EDITOR_FOLDING_PROVIDER,
                            ((ErlangFoldingStructureProviderDescriptor) sel
                                    .getFirstElement()).getId());
                    updateListDependencies();
                }
            }
        });
        viewer.setInput(fProviderDescriptors);
        viewer.refresh();

        return viewer;
    }

    void updateCheckboxDependencies() {
    }

    void updateListDependencies() {
        final String id = fStore
                .getString(PreferenceConstants.EDITOR_FOLDING_PROVIDER);
        final ErlangFoldingStructureProviderDescriptor desc = fProviderDescriptors
                .get(id);
        IErlangFoldingPreferenceBlock prefs;

        if (desc == null) {
            // safety in case there is no such descriptor
            final String message = PreferencesMessages.FoldingConfigurationBlock_error_not_exist;
            ErlideUIPlugin.log(new Status(IStatus.WARNING,
                    ErlideUIPlugin.PLUGIN_ID, IStatus.OK, message, null));
            prefs = new ErrorPreferences(message);
        } else {
            prefs = fProviderPreferences.get(id);
            if (prefs == null) {
                try {
                    prefs = desc.createPreferences();
                    fProviderPreferences.put(id, prefs);
                } catch (final CoreException e) {
                    ErlideUIPlugin.log(e);
                    prefs = new ErrorPreferences(e.getLocalizedMessage());
                }
            }
        }

        Control control = fProviderControls.get(id);
        if (control == null) {
            control = prefs.createControl(fGroup);
            if (control == null) {
                final String message = PreferencesMessages.FoldingConfigurationBlock_info_no_preferences;
                control = new ErrorPreferences(message).createControl(fGroup);
            } else {
                fProviderControls.put(id, control);
            }
        }
        Dialog.applyDialogFont(control);
        fStackLayout.topControl = control;
        control.pack();
        fGroup.layout();
        fGroup.getParent().layout();

        prefs.initialize();
    }

    @Override
    public void initialize() {
        restoreFromPreferences();
    }

    @Override
    public void performOk() {
        for (final Object element : fProviderPreferences.values()) {
            final IErlangFoldingPreferenceBlock prefs = (IErlangFoldingPreferenceBlock) element;
            prefs.performOk();
        }
    }

    @Override
    public void performDefaults() {
        restoreFromPreferences();
        for (final Object element : fProviderPreferences.values()) {
            final IErlangFoldingPreferenceBlock prefs = (IErlangFoldingPreferenceBlock) element;
            prefs.performDefaults();
        }
    }

    @Override
    public void dispose() {
        for (final Object element : fProviderPreferences.values()) {
            final IErlangFoldingPreferenceBlock prefs = (IErlangFoldingPreferenceBlock) element;
            prefs.dispose();
        }
    }

    private void restoreFromPreferences() {
        final boolean enabled = fStore
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_ENABLED);
        fFoldingCheckbox.setSelection(enabled);
        updateCheckboxDependencies();

        final String id = fStore
                .getString(PreferenceConstants.EDITOR_FOLDING_PROVIDER);
        final Object provider = fProviderDescriptors.get(id);
        if (provider != null) {
            if (fProviderViewer == null) {
                updateListDependencies();
            } else {
                fProviderViewer.setSelection(new StructuredSelection(provider),
                        true);
            }
        }
    }

}
