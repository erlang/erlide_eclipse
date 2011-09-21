/*
 * ResourceUtil.java
 * Created on 2004-08-20
 *
 * cvs-id : $Id$
 */
package org.erlide.core.model.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;

/**
 * <p>
 * contains static helping functionality to work on file resources in the
 * workspace.
 * </p>
 * 
 * @author Leif Frenzel
 * @author Andrei Formiga
 */
public class ResourceUtil {

    /**
     * <p>
     * reads an input stream and returns the contents as String.
     * </p>
     */
    public static String readStream(final InputStream is) throws IOException {
        final StringBuilder sbResult = new StringBuilder();
        final BufferedReader br = new BufferedReader(new InputStreamReader(is));
        String line = br.readLine();
        while (line != null) {
            sbResult.append(line);
            sbResult.append('\n');
            line = br.readLine();
        }
        br.close();
        is.close();

        return sbResult.toString();
    }

    /**
     * finds the corresponding resource for the specified element. This is
     * element itself, if it is an IResource, or an adapter. Returns null, if no
     * resource could be found.
     */
    public static IResource findResource(final Object element) {
        IResource result = null;
        if (element instanceof IResource) {
            result = (IResource) element;
        } else if (element instanceof IAdaptable) {
            final Object adapter = ((IAdaptable) element)
                    .getAdapter(IResource.class);
            if (adapter instanceof IResource) {
                result = (IResource) adapter;
            }
        }
        return result;
    }

    public static IResource recursiveFindNamedResource(
            final IContainer container, final String name,
            final ContainerFilter filter) throws CoreException {
        if (!container.isAccessible()) {
            return null;
        }
        IResource r = container.findMember(name);
        if (r != null && (filter == null || filter.accept(container))) {
            return r;
        }
        final IResource[] members = container.members();
        for (final IResource element : members) {
            r = element;
            if (r instanceof IContainer) {
                r = recursiveFindNamedResource((IContainer) r, name, filter);
                if (r != null) {
                    return r;
                }
            }
        }
        return null;
    }

    public static IFile getFileFromLocation(final String location) {
        final IWorkspaceRoot wr = ResourcesPlugin.getWorkspace().getRoot();
        final IFile[] f = wr.findFilesForLocationURI(URIUtil.toURI(location));
        if (f.length > 0) {
            return f[0];
        }
        return null;
    }
}
