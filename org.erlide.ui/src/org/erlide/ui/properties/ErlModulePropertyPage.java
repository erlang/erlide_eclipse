package org.erlide.ui.properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.PropertyPage;
import org.eclipse.wb.swt.SWTResourceManager;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;

public class ErlModulePropertyPage extends PropertyPage {

    private Text text;

    public ErlModulePropertyPage() {
        super();
    }

    @Override
    protected Control createContents(final Composite parent) {
        final Composite control = new Composite(parent, SWT.NONE);
        control.setLayout(new FillLayout());

        text = new Text(control, SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL);
        text.setFont(SWTResourceManager.getFont("Courier New", 10, SWT.NONE));

        final IAdaptable element = getElement();
        final IFile file = (IFile) element.getAdapter(IFile.class);
        final IErlModule module = ErlangEngine.getInstance().getModel().findModule(file);
        final String value = ErlangEngine.getInstance().getModelUtilService()
                .getModuleInfo(module);
        text.setText(value);

        return control;
    }

}
