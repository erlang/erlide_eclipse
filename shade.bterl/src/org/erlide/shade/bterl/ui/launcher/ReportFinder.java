package org.erlide.shade.bterl.ui.launcher;

import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

public class ReportFinder {

    private ReportFinder() {
    }

    public static IFile findReportFile(final File parent) {
        String location = getFileContent(parent, "last_test.html");
        if (location != null) {
            final Pattern pattern = Pattern
                    .compile("<a href=\"([^\"]+)\">.*</a>");
            final Matcher matcher = pattern.matcher(location);
            if (matcher.find()) {
                location = matcher.group(1);
            } else {
                location = getFileContent(parent, "last_name");
            }
        } else {
            location = getFileContent(parent, "last_name");
        }
        if (!location.startsWith("/")) {
            location = parent.getAbsolutePath() + "/" + location;
        }
        final IWorkspaceRoot wroot = ResourcesPlugin.getWorkspace().getRoot();
        try {
            return wroot.findFilesForLocationURI(new URI("file://" + location))[0];
        } catch (final URISyntaxException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static IFile findTraceFile(final File parent) {
        final File[] files = parent.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(final File dir, final String name) {
                return "tr.txt".equals(name);
            }
        });
        if (files.length == 0) {
            return null;
        }
        final String location = files[0].getAbsolutePath();
        final IWorkspaceRoot wroot = ResourcesPlugin.getWorkspace().getRoot();
        try {
            return wroot.findFilesForLocationURI(new URI("file://" + location))[0];
        } catch (final URISyntaxException e) {
            e.printStackTrace();
        }
        return null;
    }

    private static String getFileContent(final File parent, final String name) {
        final IPath ppath = new Path(parent.getAbsolutePath() + "/" + name);
        final IWorkspaceRoot wroot = ResourcesPlugin.getWorkspace().getRoot();
        @SuppressWarnings("deprecation")
        final IFile[] files = wroot.findFilesForLocation(ppath);
        if (files.length > 0) {
            final IFile file = files[0];
            try {
                file.refreshLocal(IResource.DEPTH_ZERO, null);
                if (file.exists() && file.isAccessible()) {
                    return readFile(file);
                }
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    private static String readFile(final IFile file) {
        InputStream inputStream;
        try {
            inputStream = file.getContents();
            final BufferedReader is = new BufferedReader(new InputStreamReader(
                    inputStream));
            try {
                String line = is.readLine();
                final StringBuilder result = new StringBuilder();
                while (line != null) {
                    result.append(line);
                    line = is.readLine();
                }
                return result.toString();
            } finally {
                inputStream.close();
                is.close();
            }
        } catch (final CoreException e) {
            e.printStackTrace();
        } catch (final IOException e) {
            e.printStackTrace();
        }
        return null;
    }

}
