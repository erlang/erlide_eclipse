package org.erlide.test_support.ui.launcher;

import java.io.File;
import java.io.FilenameFilter;
import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.runtime.launch.ErlangDebugHelper;

public class TestDebugHelper extends ErlangDebugHelper {
    private final File workdir;

    public TestDebugHelper(final File workdir) {
        this.workdir = workdir;
    }

    @Override
    protected IFile findModuleBeam(final String project, final String module)
            throws ErlModelException {
        final String beam = ErlideUtil.withoutExtension(module) + ".beam";
        final File[] files = workdir.listFiles(new FilenameFilter() {
            public boolean accept(final File dir, final String name) {
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
