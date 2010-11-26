/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl.outline;

import java.util.ArrayList;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.erlide.core.erlang.IErlElement;

public class ErlangLabelProvider implements ILabelProvider, IColorProvider {

    protected ListenerList fListeners = new ListenerList(1);

    protected ErlangElementImageProvider fImageLabelProvider;

    private ArrayList<ILabelDecorator> fLabelDecorators;

    private int fImageFlags;

    private long fTextFlags;

    /**
     * Creates a new label provider with default flags.
     */
    public ErlangLabelProvider() {
        this(0, ErlangElementImageProvider.OVERLAY_ICONS);
    }

    /**
     * @param textFlags
     *            Flags defined in <code>JavaElementLabels</code>.
     * @param imageFlags
     *            Flags defined in <code>JavaElementImageProvider</code>.
     */
    public ErlangLabelProvider(final long textFlags, final int imageFlags) {
        fImageLabelProvider = new ErlangElementImageProvider();
        fLabelDecorators = null;

        fImageFlags = imageFlags;
        fTextFlags = textFlags;
    }

    /**
     * Adds a decorator to the label provider
     */
    public void addLabelDecorator(final ILabelDecorator decorator) {
        if (fLabelDecorators == null) {
            fLabelDecorators = new ArrayList<ILabelDecorator>(2);
        }
        fLabelDecorators.add(decorator);
    }

    /**
     * Sets the textFlags.
     * 
     * @param textFlags
     *            The textFlags to set
     */
    public final void setTextFlags(final long textFlags) {
        fTextFlags = textFlags;
    }

    /**
     * Sets the imageFlags
     * 
     * @param imageFlags
     *            The imageFlags to set
     */
    public final void setImageFlags(final int imageFlags) {
        fImageFlags = imageFlags;
    }

    /**
     * Gets the image flags. Can be overwriten by super classes.
     * 
     * @return Returns a int
     */
    public final int getImageFlags() {
        return fImageFlags;
    }

    /**
     * Gets the text flags.
     * 
     * @return Returns a int
     */
    public final long getTextFlags() {
        return fTextFlags;
    }

    /**
     * Evaluates the image flags for a element. Can be overwriten by super
     * classes.
     * 
     * @return Returns a int
     */
    protected int evaluateImageFlags(final Object element) {
        return getImageFlags();
    }

    /**
     * Evaluates the text flags for a element. Can be overwriten by super
     * classes.
     * 
     * @return Returns a int
     */
    protected long evaluateTextFlags(final Object element) {
        return getTextFlags();
    }

    protected Image decorateImage(Image image, final Object element) {
        if (fLabelDecorators != null && image != null) {
            for (int i = 0; i < fLabelDecorators.size(); i++) {
                final ILabelDecorator decorator = fLabelDecorators.get(i);
                image = decorator.decorateImage(image, element);
            }
        }
        return image;
    }

    /*
     * (non-Javadoc)
     * 
     * @see ILabelProvider#getImage
     */
    public Image getImage(final Object element) {
        final Image result = fImageLabelProvider.getImageLabel(element,
                evaluateImageFlags(element));

        return decorateImage(result, element);
    }

    protected String decorateText(String text, final Object element) {
        if (fLabelDecorators != null && text.length() > 0) {
            for (int i = 0; i < fLabelDecorators.size(); i++) {
                final ILabelDecorator decorator = fLabelDecorators.get(i);
                text = decorator.decorateText(text, element);
            }
        }
        return text;
    }

    /*
     * (non-Javadoc)
     * 
     * @see ILabelProvider#getText
     */
    public String getText(final Object element) {
        String label;
        if (element instanceof IErlElement) {
            final IErlElement el = (IErlElement) element;
            label = el.getLabelString();
        } else {
            label = element.toString();
        }
        return decorateText(label, element);
    }

    // private String range(ISourceReference a)
    // {
    // try {
    // return a.getSourceRange().toString();
    // } catch (ErlModelException e) {
    // return "";
    // }
    // }

    /*
     * (non-Javadoc)
     * 
     * @see IBaseLabelProvider#dispose
     */
    public void dispose() {
        if (fLabelDecorators != null) {
            for (int i = 0; i < fLabelDecorators.size(); i++) {
                final ILabelDecorator decorator = fLabelDecorators.get(i);
                decorator.dispose();
            }
            fLabelDecorators = null;
        }
        fImageLabelProvider.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see IBaseLabelProvider#addListener(ILabelProviderListener)
     */
    public void addListener(final ILabelProviderListener listener) {
        if (fLabelDecorators != null) {
            for (int i = 0; i < fLabelDecorators.size(); i++) {
                final ILabelDecorator decorator = fLabelDecorators.get(i);
                decorator.addListener(listener);
            }
        }
        fListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see IBaseLabelProvider#isLabelProperty(Object, String)
     */
    public boolean isLabelProperty(final Object element, final String property) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see IBaseLabelProvider#removeListener(ILabelProviderListener)
     */
    public void removeListener(final ILabelProviderListener listener) {
        if (fLabelDecorators != null) {
            for (int i = 0; i < fLabelDecorators.size(); i++) {
                final ILabelDecorator decorator = fLabelDecorators.get(i);
                decorator.removeListener(listener);
            }
        }
        fListeners.remove(listener);
    }

    public static ILabelDecorator[] getDecorators(final boolean errortick,
            final ILabelDecorator extra) {
        if (errortick) {
            if (extra == null) {
                return new ILabelDecorator[] {};
            }
            return new ILabelDecorator[] { extra };
        }
        if (extra != null) {
            return new ILabelDecorator[] { extra };
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.IColorProvider#getForeground(java.lang.Object)
     */
    public Color getForeground(final Object element) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.IColorProvider#getBackground(java.lang.Object)
     */
    public Color getBackground(final Object element) {
        return null;
    }

    /**
     * Fires a label provider changed event to all registered listeners Only
     * listeners registered at the time this method is called are notified.
     * 
     * @param event
     *            a label provider changed event
     * 
     * @see ILabelProviderListener#labelProviderChanged
     */
    protected void fireLabelProviderChanged(
            final LabelProviderChangedEvent event) {
        final Object[] listeners = fListeners.getListeners();
        for (final Object element : listeners) {
            final ILabelProviderListener l = (ILabelProviderListener) element;
            SafeRunner.run(new SafeRunnable() {

                public void run() {
                    l.labelProviderChanged(event);
                }
            });
        }
    }

}
