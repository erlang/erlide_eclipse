package org.erlide.ui.util;

import java.util.Collection;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * The IElementFilter is a interface that defines the api for filtering the
 * current selection of a ResourceTreeAndListGroup in order to find a subset to
 * update as the result of a type filtering. This is meant as an internal class
 * and is used exlcusively by the import dialog.
 */

public interface IElementFilter {

    public void filterElements(Collection<?> elements, IProgressMonitor monitor)
            throws InterruptedException;

    public void filterElements(Object[] elements, IProgressMonitor monitor)
            throws InterruptedException;

}
