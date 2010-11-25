package org.erlide.ui;

import org.erlide.ui.util.PopupDialog;

public class MessageReporter extends org.erlide.core.util.MessageReporter {

    public MessageReporter() {
    }

    @Override
    public void displayMessage(final String message,
            final ReporterPosition style) {
        switch (style) {
        case MODAL:
            PopupDialog.showModalDialog("Erlide error", message);
            break;
        case CENTER:
            PopupDialog.showDialog("Erlide error", message, 3000);
            break;
        case CORNER:
            PopupDialog.showBalloon("Erlide error", message, 3000);
            break;
        default:
            break;
        }
    }

}
