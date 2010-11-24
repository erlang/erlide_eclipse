package org.erlide.ui;

import org.erlide.ui.util.PopupDialog;

public class MessageReporter extends org.erlide.core.util.MessageReporter {

    public MessageReporter() {
    }

    @Override
    public void displayMessage(final String message,
            final ReporterPosition style) {
        switch (style) {
        case CENTER:
            PopupDialog.showDialog("erlide error", message, 1000);
            break;
        case CORNER:
            PopupDialog.showBalloon("erlide error", message, 1000);
            break;
        default:
            break;
        }
    }

}
