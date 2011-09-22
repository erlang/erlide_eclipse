package org.erlide.cover.ui.launch;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.dialogs.SelectionDialog;

/**
 * Browser for selecting projects, modules or files.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class ItemBrowser {

    private final Text text;
    private final Button button;
    private final SelectionDialog dialog;
    private char type;

    public ItemBrowser(final Composite comp, final int style,
            final SelectionDialog dial) {

        final GridData gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        gd.widthHint = 500;

        dialog = dial;

        text = new Text(comp, style | SWT.RESIZE);
        text.setLayoutData(gd);
        text.setEditable(false);

        button = new Button(comp, SWT.CENTER | SWT.PUSH);
        button.setText("Browse");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent e) {

                dialog.open();

                if (dialog.getReturnCode() == Window.OK
                        && dialog.getResult() != null
                        && dialog.getResult().length > 0) {

                    final Object[] res = dialog.getResult();
                    String result = res[0].toString();
                    if (dialog instanceof ElementTreeSelectionDialog) {
                        type = result.charAt(0);
                        result = result.substring(2);
                    }
                    text.setText(result);
                }
            }

        });

    }

    public void addModifyListener(final ModifyListener ml) {
        text.addModifyListener(ml);
    }

    public void setFiledLength(final int size) {
        ((GridData) text.getLayoutData()).widthHint = size;
    }

    public String getText() {
        return text.getText();
    }

    public void setText(final String info) {
        text.setText(info);
    }

    public GridData getTextGridData() {
        return (GridData) text.getLayoutData();
    }

    public void setEnabled(final boolean enabled) {
        text.setEnabled(enabled);
        button.setEnabled(enabled);
    }

    public char getType() {
        return type;
    }

}
