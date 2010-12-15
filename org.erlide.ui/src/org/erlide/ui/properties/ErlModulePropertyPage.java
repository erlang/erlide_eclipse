package org.erlide.ui.properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.TypeConverter;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.swtdesigner.SWTResourceManager;

public class ErlModulePropertyPage extends PropertyPage implements
        IWorkbenchPropertyPage {

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
        final IErlModule module = ErlangCore.getModel().findModule(file);
        String value = "There is no module information about this file.";
        if (module != null) {
            final IErlProject project = module.getErlProject();
            final IOldErlangProjectProperties prefs = project.getProperties();
            final IPath beamPath = prefs.getOutputDir()
                    .append(module.getModuleName()).addFileExtension("beam");
            final IFile beam = project.getProject().getFile(beamPath);

            // TODO should it be the build backend?
            final Backend backend = ErlangCore.getBackendManager()
                    .getIdeBackend();
            try {
                final OtpErlangObject info = backend.call("erlide_backend",
                        "get_module_info", "s", beam.getLocation()
                                .toPortableString());
                value = (String) TypeConverter.erlang2java(info, String.class);
            } catch (final Exception e) {
            }
        }
        text.setText(value);

        return control;
    }

}
