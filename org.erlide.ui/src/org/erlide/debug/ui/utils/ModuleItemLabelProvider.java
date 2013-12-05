package org.erlide.debug.ui.utils;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.erlide.engine.model.root.IErlElement;

/**
 * A label provider for ResourceDecorator objects. It creates labels with a
 * resource full path for duplicates. It uses the Platform UI label decorator
 * for providing extra resource info.
 */
public class ModuleItemLabelProvider extends LabelProvider implements
        ILabelProviderListener, IStyledLabelProvider {

    // Need to keep our own list of listeners
    protected final ListenerList listeners = new ListenerList();

    protected WorkbenchLabelProvider provider = new WorkbenchLabelProvider();

    public ModuleItemLabelProvider() {
        super();
        provider.addListener(this);
    }

    @Override
    public Image getImage(final Object element) {
        final IResource resource = getElementResource(element);
        if (resource == null) {
            return super.getImage(element);
        }

        return provider.getImage(resource);
    }

    protected IResource getElementResource(final Object element) {
        if (element instanceof IResource) {
            return (IResource) element;
        }
        if (element instanceof IErlElement) {
            final IErlElement erlElement = (IErlElement) element;
            return erlElement.getCorrespondingResource();
        }
        return null;
    }

    @Override
    public String getText(final Object element) {
        final IResource res = getElementResource(element);
        if (res == null) {
            return super.getText(element);
        }
        final String name = res.getName();
        if (showFullPath(element)) {
            return name + " - " + res.getParent().getFullPath().makeRelative().toString(); //$NON-NLS-1$
        }
        return name;
    }

    @Override
    public StyledString getStyledText(final Object element) {
        if (getElementResource(element) == null) {
            return new StyledString(super.getText(element));
        }
        final String text = getText(element);
        final StyledString str = new StyledString(text);
        final int index = text.indexOf(" - ");
        if (index != -1) {
            str.setStyle(index, text.length() - index, StyledString.QUALIFIER_STYLER);
        }
        return str;
    }

    @Override
    public void dispose() {
        provider.removeListener(this);
        provider.dispose();

        super.dispose();
    }

    @Override
    public void addListener(final ILabelProviderListener listener) {
        listeners.add(listener);
    }

    @Override
    public void removeListener(final ILabelProviderListener listener) {
        listeners.remove(listener);
    }

    @Override
    public void labelProviderChanged(final LabelProviderChangedEvent event) {
        final Object[] l = listeners.getListeners();
        for (int i = 0; i < listeners.size(); i++) {
            ((ILabelProviderListener) l[i]).labelProviderChanged(event);
        }
    }

    protected boolean showFullPath(final Object item) {
        return true;
    }

}
