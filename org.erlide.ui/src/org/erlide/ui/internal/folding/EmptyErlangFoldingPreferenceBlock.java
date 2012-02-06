package org.erlide.ui.internal.folding;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.erlide.ui.editors.erl.folding.IErlangFoldingPreferenceBlock;

/**
 * Empty preference block for extensions to the
 * <code>org.erlide.ui.erlangFoldingStructureProvider</code> extension point
 * that do not specify their own.
 * 
 * @since 3.0
 */

public class EmptyErlangFoldingPreferenceBlock implements
        IErlangFoldingPreferenceBlock {

    @Override
    public Control createControl(final Composite composite) {
        final Composite inner = new Composite(composite, SWT.NONE);
        inner.setLayout(new GridLayout(3, false));

        Label label = new Label(inner, SWT.CENTER);
        GridData gd = new GridData(GridData.FILL_BOTH);
        gd.widthHint = 30;
        label.setLayoutData(gd);

        label = new Label(inner, SWT.CENTER);
        // TODO
        // label.setText(FoldingMessages.EmptyErlangFoldingPreferenceBlock_emptyCaption);
        label.setText("");
        gd = new GridData(GridData.CENTER);
        label.setLayoutData(gd);

        label = new Label(inner, SWT.CENTER);
        gd = new GridData(GridData.FILL_BOTH);
        gd.widthHint = 30;
        label.setLayoutData(gd);

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
