package org.erlide.ui.wizards;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.ui.dialogs.FileSystemElement;
import org.eclipse.ui.model.AdaptableList;
import org.eclipse.ui.wizards.datatransfer.IImportStructureProvider;

public class MinimizedFileSystemElement extends FileSystemElement {
    private boolean populated = false;

    /**
     * Create a <code>MinimizedFileSystemElement</code> with the supplied name
     * and parent.
     * 
     * @param name
     *            the name of the file element this represents
     * @param parent
     *            the containing parent
     * @param isDirectory
     *            indicated if this could have children or not
     */
    public MinimizedFileSystemElement(final String name,
            final FileSystemElement parent, final boolean isDirectory) {
        super(name, parent, isDirectory);
    }

    /**
     * Returns a list of the files that are immediate children. Use the supplied
     * provider if it needs to be populated. of this folder.
     */
    public AdaptableList getFiles(final IImportStructureProvider provider) {
        if (!populated) {
            populate(provider);
        }
        return super.getFiles();
    }

    /**
     * Returns a list of the folders that are immediate children. Use the
     * supplied provider if it needs to be populated. of this folder.
     */
    public AdaptableList getFolders(final IImportStructureProvider provider) {
        if (!populated) {
            populate(provider);
        }
        return super.getFolders();
    }

    /**
     * Return whether or not population has happened for the receiver.
     */
    boolean isPopulated() {
        return populated;
    }

    /**
     * Return whether or not population has not happened for the receiver.
     */
    boolean notPopulated() {
        return !populated;
    }

    /**
     * Populate the files and folders of the receiver using the suppliec
     * structure provider.
     * 
     * @param provider
     *            org.eclipse.ui.wizards.datatransfer.IImportStructureProvider
     */
    private void populate(final IImportStructureProvider provider) {

        final Object fileSystemObject = getFileSystemObject();

        List<?> children = provider.getChildren(fileSystemObject);
        if (children == null) {
            children = new ArrayList<Object>(1);
        }
        final Iterator<?> childrenEnum = children.iterator();
        while (childrenEnum.hasNext()) {
            final Object child = childrenEnum.next();

            final String elementLabel = provider.getLabel(child);
            // Create one level below
            final MinimizedFileSystemElement result = new MinimizedFileSystemElement(
                    elementLabel, this, provider.isFolder(child));
            result.setFileSystemObject(child);
        }
        setPopulated();
    }

    /**
     * Set whether or not population has happened for the receiver to true.
     */
    public void setPopulated() {
        populated = true;
    }
}
