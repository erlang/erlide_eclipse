package org.erlide.ui.editors.erl.correction.fixes;

import java.io.ByteArrayInputStream;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.dialogs.MessageDialog;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.ui.editors.erl.correction.MarkerQuickFixExecutor;
import org.erlide.util.Util;

public class RenameModuleQuickFix extends MarkerQuickFixExecutor {

    @Override
    public void run() throws Exception {
        final IMarker marker = getMarker();
        final List<String> margs = getQuickFix().getArgs();
        final String moduleName = margs.get(0);

        final IErlModel svc = ErlangEngine.getInstance().getModel();

        IErlModule newModule;
        newModule = svc.findModule(moduleName);
        if (newModule != null) {
            MessageDialog.openInformation(null, "Rename module quickfix",
                    "A module with name '" + moduleName + "' already exists.");
            newModule.dispose();
            return;
        }

        final IFile file = (IFile) marker.getResource();
        final IErlModule oldModule = svc.findModule(file);
        if (oldModule == null) {
            return;
        }

        String in = Util.getInputStreamAsString(file.getContents(), file.getCharset());
        in = in.replaceFirst("-module\\([^)]+\\)\\.", "-module(" + moduleName + ").");
        file.setContents(new ByteArrayInputStream(in.getBytes()), IResource.FORCE, null);
    }

}
