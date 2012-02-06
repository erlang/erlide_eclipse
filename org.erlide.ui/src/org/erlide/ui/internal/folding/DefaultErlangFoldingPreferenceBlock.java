package org.erlide.ui.internal.folding;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.erlide.ui.editors.erl.folding.IErlangFoldingPreferenceBlock;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.util.OverlayPreferenceStore;
import org.erlide.ui.util.OverlayPreferenceStore.OverlayKey;
import org.erlide.ui.util.OverlayPreferenceStore.TypeDescriptor;

/**
 * Erlang (erlide) default folding preferences.
 */
public class DefaultErlangFoldingPreferenceBlock implements
        IErlangFoldingPreferenceBlock {

    private final IPreferenceStore fStore;

    OverlayPreferenceStore fOverlayStore;

    private final OverlayKey[] fKeys;

    Map<Button, String> fCheckBoxes = new HashMap<Button, String>();

    private final SelectionListener fCheckBoxListener = new SelectionListener() {

        @Override
        public void widgetDefaultSelected(final SelectionEvent e) {
        }

        @Override
        public void widgetSelected(final SelectionEvent e) {
            final Button button = (Button) e.widget;
            fOverlayStore.setValue(fCheckBoxes.get(button),
                    button.getSelection());
        }
    };

    public DefaultErlangFoldingPreferenceBlock() {
        fStore = ErlideUIPlugin.getDefault().getPreferenceStore();
        fKeys = createKeys();
        fOverlayStore = new OverlayPreferenceStore(fStore, fKeys);
    }

    private OverlayKey[] createKeys() {
        final ArrayList<OverlayKey> overlayKeys = new ArrayList<OverlayKey>();

        overlayKeys.add(new OverlayPreferenceStore.OverlayKey(
                TypeDescriptor.BOOLEAN,
                PreferenceConstants.EDITOR_FOLDING_CLAUSES));
        overlayKeys.add(new OverlayPreferenceStore.OverlayKey(
                TypeDescriptor.BOOLEAN,
                PreferenceConstants.EDITOR_FOLDING_COMMENTS));
        overlayKeys.add(new OverlayPreferenceStore.OverlayKey(
                TypeDescriptor.BOOLEAN,
                PreferenceConstants.EDITOR_FOLDING_HEADER_COMMENTS));
        overlayKeys.add(new OverlayPreferenceStore.OverlayKey(
                TypeDescriptor.BOOLEAN,
                PreferenceConstants.EDITOR_FOLDING_TYPESPECS));

        return overlayKeys.toArray(new OverlayKey[overlayKeys.size()]);
    }

    @Override
    public Control createControl(final Composite composite) {
        fOverlayStore.load();
        fOverlayStore.start();

        final Composite inner = new Composite(composite, SWT.NONE);
        final GridLayout layout = new GridLayout(1, true);
        layout.verticalSpacing = 3;
        layout.marginWidth = 0;
        inner.setLayout(layout);

        final Label label = new Label(inner, SWT.LEFT);
        label.setText(FoldingMessages.DefaultErlangFoldingPreferenceBlock_title);

        addCheckBox(
                inner,
                FoldingMessages.DefaultErlangFoldingPreferenceBlock_header_comments,
                PreferenceConstants.EDITOR_FOLDING_HEADER_COMMENTS, 0);
        addCheckBox(inner,
                FoldingMessages.DefaultErlangFoldingPreferenceBlock_clauses,
                PreferenceConstants.EDITOR_FOLDING_CLAUSES, 0);
        addCheckBox(inner,
                FoldingMessages.DefaultErlangFoldingPreferenceBlock_comments,
                PreferenceConstants.EDITOR_FOLDING_COMMENTS, 0);
        addCheckBox(inner,
                FoldingMessages.DefaultErlangFoldingPreferenceBlock_typespecs,
                PreferenceConstants.EDITOR_FOLDING_TYPESPECS, 0);

        return inner;
    }

    private Button addCheckBox(final Composite parent, final String label,
            final String key, final int indentation) {
        final Button checkBox = new Button(parent, SWT.CHECK);
        checkBox.setText(label);

        final GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        gd.horizontalIndent = indentation;
        gd.horizontalSpan = 1;
        gd.grabExcessVerticalSpace = false;
        checkBox.setLayoutData(gd);
        checkBox.addSelectionListener(fCheckBoxListener);

        fCheckBoxes.put(checkBox, key);

        return checkBox;
    }

    private void initializeFields() {
        final Iterator<Button> it = fCheckBoxes.keySet().iterator();
        while (it.hasNext()) {
            final Button b = it.next();
            final String key = fCheckBoxes.get(b);
            b.setSelection(fOverlayStore.getBoolean(key));
        }
    }

    @Override
    public void performOk() {
        fOverlayStore.propagate();
    }

    @Override
    public void initialize() {
        initializeFields();
    }

    @Override
    public void performDefaults() {
        fOverlayStore.loadDefaults();
        initializeFields();
    }

    @Override
    public void dispose() {
        fOverlayStore.stop();
    }
}
