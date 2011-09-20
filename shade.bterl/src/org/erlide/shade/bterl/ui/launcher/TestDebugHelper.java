package org.erlide.shade.bterl.ui.launcher;

import java.io.File;
import java.io.FilenameFilter;
import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.erlide.core.common.CommonUtils;
import org.erlide.core.debug.ErlangDebugHelper;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.jinterface.ErlLogger;

public class TestDebugHelper extends ErlangDebugHelper {
    private final File workdir;

    public TestDebugHelper(final File workdir) {
        this.workdir = workdir;
    }

    @Override
    protected IFile findModuleBeam(final IProject project, final String module)
            throws ErlModelException {
        final String beam = CommonUtils.withoutExtension(module) + ".beam";
        final File[] files = workdir.listFiles(new FilenameFilter() {
            public boolean accept(final File dir, final String name) {
                ErlLogger.debug(dir.getAbsolutePath() + "   " + name);
                return name.equals(beam);
            }
        });
        if (files.length == 0) {
            return super.findModuleBeam(project, module);
        }
        try {
            final IFile[] ifs = ResourcesPlugin
                    .getWorkspace()
                    .getRoot()
                    .findFilesForLocationURI(
                            new URI("file://" + files[0].getAbsolutePath()));
            if (ifs.length == 0) {
                return null;
            }
            return ifs[0];
        } catch (final URISyntaxException e) {
            e.printStackTrace();
        }
        return null;
    }

}
