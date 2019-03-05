package org.erlide.ui.internal.information;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.internal.text.html.BrowserInformationControl;
import org.eclipse.jface.text.IInputChangedListener;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.widgets.Shell;

@SuppressWarnings("restriction")
public class ErlangBrowserInformationControl extends BrowserInformationControl {

    public ErlangBrowserInformationControl(Shell parent, String symbolicFontName,
            boolean resizable) {
        super(parent, symbolicFontName, resizable);
    }

    public ErlangBrowserInformationControl(Shell parent, String font,
            ToolBarManager tbm) {
        super(parent, font, tbm);
    }

    public ErlangBrowserInformationControl(Shell parent, String dialogFont,
            String tooltipAffordanceString) {
        super(parent, dialogFont, tooltipAffordanceString);
    }

    @Override
    public void addInputChangeListener(IInputChangedListener inputChangeListener) {
        super.addInputChangeListener(inputChangeListener);
    }

    @Override
    public void addLocationListener(LocationListener listener) {
        super.addLocationListener(listener);
    }

    @Override
    public void notifyDelayedInputChange(Object newInput) {
        super.notifyDelayedInputChange(newInput);
    }

    public static boolean isAvailable(Shell parent) {
        return BrowserInformationControl.isAvailable(parent);
    }

    @Override
    public void setInput(Object input) {
        super.setInput(input);
    }

    @Override
    public ErlangBrowserInformationControlInput getInput() {
        return (ErlangBrowserInformationControlInput) super.getInput();
    }

    @Override
    public void setSize(int width, int height) {
        super.setSize(width, height);
    }

    @Override
    public boolean hasDelayedInputChangeListener() {
        return super.hasDelayedInputChangeListener();
    }
}
