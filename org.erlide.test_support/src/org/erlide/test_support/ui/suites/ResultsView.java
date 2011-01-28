package org.erlide.test_support.ui.suites;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ResultsView extends ViewPart {

    private final TestEventHandler eventHandler;
    private Composite control;

    public ResultsView() {
        eventHandler = new TestEventHandler(this);
    }

    @Override
    public void createPartControl(final Composite parent) {
        control = new Composite(parent, SWT.NONE);
    }

    @Override
    public void setFocus() {
        control.setFocus();
    }

    public void notifyEvent(final OtpErlangObject msg) {

    }

}
