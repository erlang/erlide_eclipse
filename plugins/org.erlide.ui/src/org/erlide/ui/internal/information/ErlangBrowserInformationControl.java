package org.erlide.ui.internal.information;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.internal.text.html.BrowserInformationControl;
import org.eclipse.jface.text.IInputChangedListener;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.widgets.Shell;

@SuppressWarnings("restriction")
public class ErlangBrowserInformationControl extends BrowserInformationControl {

    public ErlangBrowserInformationControl(final Shell parent,
            final String symbolicFontName, final boolean resizable) {
        super(parent, symbolicFontName, resizable);
    }

    public ErlangBrowserInformationControl(final Shell parent, final String font,
            final ToolBarManager tbm) {
        super(parent, font, tbm);
    }

    public ErlangBrowserInformationControl(final Shell parent, final String dialogFont,
            final String tooltipAffordanceString) {
        super(parent, dialogFont, tooltipAffordanceString);
    }

    @Override
    public void addInputChangeListener(final IInputChangedListener inputChangeListener) {
        super.addInputChangeListener(inputChangeListener);
    }

    @Override
    public void addLocationListener(final LocationListener listener) {
        super.addLocationListener(listener);
    }

    @Override
    public void notifyDelayedInputChange(final Object newInput) {
        super.notifyDelayedInputChange(newInput);
    }

    public static boolean isAvailable(final Shell parent) {
        return BrowserInformationControl.isAvailable(parent);
    }

    @Override
    public void setInput(final Object input) {
        super.setInput(input);
    }

    @Override
    public ErlangBrowserInformationControlInput getInput() {
        return (ErlangBrowserInformationControlInput) super.getInput();
    }

    @Override
    public void setSize(final int width, final int height) {
        super.setSize(width, height);
    }

    @Override
    public boolean hasDelayedInputChangeListener() {
        return super.hasDelayedInputChangeListener();
    }
}
